"""Embedding pipeline: parse FESOM2 RST docs, embed via ollama, write to ChromaDB.

Run as:
    pixi run fesom2-embed-docs
"""

import logging
from pathlib import Path

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
log = logging.getLogger(__name__)
logging.getLogger("httpx").setLevel(logging.WARNING)
logging.getLogger("httpcore").setLevel(logging.WARNING)

from ...embed_utils import _chunk_text, BATCH_SIZE, MAX_CHARS, OVERLAP
from ...rst_parser import iter_sections
from .pipeline import _embed_with_retry
from .store import CHROMA_PATH, get_docs_collection

FESOM2_DOC_ROOT = Path("FESOM2/docs")
FESOM2_ROOT = Path("FESOM2")


def _file_chunks(
    file_id: str, file: str, text: str
) -> list[tuple[str, str, dict]]:
    """Return chunks for a plain file (no RST section structure)."""
    header = f"[{file}]\n"
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    n = len(chunks)
    return [
        (
            f"{file_id}_{i}",
            header + chunk,
            {
                "file": file,
                "section": Path(file).name,
                "chunk_index": i,
                "n_chunks": n,
                "section_id": file_id,
            },
        )
        for i, chunk in enumerate(chunks)
    ]


def _iter_extra_files(fesom2_root: Path) -> list[dict]:
    """Return plain-text files not covered by RST parsing.

    Covers:
      - visualization/*/README.md  — tool READMEs (pyfesom2, view, tripyview, spheRlab)
      - visualization/README.md    — top-level visualization overview
      - src/*.h, src/*.inc         — mesh association macros and gather templates
    """
    results = []
    globs = [
        fesom2_root.glob("visualization/README*"),
        fesom2_root.glob("visualization/*/README*"),
        fesom2_root.glob("src/*.h"),
        fesom2_root.glob("src/*.inc"),
    ]
    import itertools
    for path in sorted(itertools.chain(*globs)):
        try:
            text = path.read_text(encoding="utf-8", errors="replace").strip()
        except (PermissionError, OSError):
            continue
        if not text:
            continue
        results.append({
            "file": path.relative_to(fesom2_root).as_posix(),
            "text": text,
        })
    return results


def _doc_chunks(
    section_id: str, file: str, section: str, text: str
) -> list[tuple[str, str, dict]]:
    """Return one (chroma_id, document_text, metadata) tuple per chunk."""
    header = f"[{file}] {section}\n" if section else f"[{file}]\n"
    chunks = _chunk_text(text, MAX_CHARS, OVERLAP)
    n = len(chunks)
    return [
        (
            f"{section_id}_{i}",
            header + chunk,
            {
                "file": file,
                "section": section,
                "chunk_index": i,
                "n_chunks": n,
                "section_id": section_id,
            },
        )
        for i, chunk in enumerate(chunks)
    ]


def run(
    doc_root: Path = FESOM2_DOC_ROOT,
    fesom2_root: Path = FESOM2_ROOT,
    chroma_path: Path = CHROMA_PATH,
) -> None:
    sections = iter_sections(doc_root)
    log.info(f"Parsed {len(sections)} sections from {doc_root}")

    extras = _iter_extra_files(fesom2_root)
    log.info(f"Found {len(extras)} extra files (visualization READMEs + src headers)")

    collection = get_docs_collection(chroma_path)

    all_chunks = []
    for idx, sec in enumerate(sections):
        all_chunks.extend(_doc_chunks(f"doc_{idx}", sec["file"], sec["section"], sec["text"]))
    for idx, ex in enumerate(extras):
        all_chunks.extend(_file_chunks(f"extra_{idx}", ex["file"], ex["text"]))
    log.info(f"Generated {len(all_chunks)} chunks from {len(sections)} sections + {len(extras)} extra files")

    total = 0
    for i in range(0, len(all_chunks), BATCH_SIZE):
        batch = all_chunks[i : i + BATCH_SIZE]
        ids = [c[0] for c in batch]
        docs = [c[1] for c in batch]
        metadatas = [c[2] for c in batch]

        ids, embeddings, docs, metadatas = _embed_with_retry(ids, docs, metadatas)
        if ids:
            collection.upsert(
                ids=ids,
                embeddings=embeddings,
                documents=docs,
                metadatas=metadatas,
            )

        total += len(batch)
        if total % 100 == 0 or total == len(all_chunks):
            log.info(f"  Embedded {total}/{len(all_chunks)}")

    log.info(f"Done. {collection.count()} chunks ({len(sections)} sections).")


if __name__ == "__main__":
    run()

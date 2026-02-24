"""Embedding pipeline: parse FESOM2 RST docs, embed via ollama, write to ChromaDB.

Run as:
    pixi run fesom2-embed-docs
"""

import logging
from pathlib import Path

import ollama

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
log = logging.getLogger(__name__)
logging.getLogger("httpx").setLevel(logging.WARNING)
logging.getLogger("httpcore").setLevel(logging.WARNING)

from ..embedder.pipeline import _chunk_text, EMBED_MODEL, BATCH_SIZE, MAX_CHARS, OVERLAP
from ..docs_indexer.parse import iter_sections
from .pipeline import _embed_with_retry
from .store import CHROMA_PATH, get_docs_collection

FESOM2_DOC_ROOT = Path("FESOM2/docs")


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


def run(doc_root: Path = FESOM2_DOC_ROOT, chroma_path: Path = CHROMA_PATH) -> None:
    sections = iter_sections(doc_root)
    log.info(f"Parsed {len(sections)} sections from {doc_root}")

    collection = get_docs_collection(chroma_path)

    all_chunks = []
    for idx, sec in enumerate(sections):
        all_chunks.extend(_doc_chunks(f"doc_{idx}", sec["file"], sec["section"], sec["text"]))
    log.info(f"Generated {len(all_chunks)} chunks from {len(sections)} sections")

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

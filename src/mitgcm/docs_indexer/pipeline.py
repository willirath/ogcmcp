"""Embedding pipeline: parse MITgcm RST docs, embed via ollama, write to ChromaDB.

Run as:
    pixi run embed-docs

The collection 'mitgcm_docs' is created in the same ChromaDB path as the
subroutines collection (data/mitgcm/chroma).
"""

import logging
import time
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

from ...embed_utils import _chunk_text, EMBED_MODEL, BATCH_SIZE, MAX_CHARS, OVERLAP
from ..embedder.store import CHROMA_PATH, get_docs_collection
from ...rst_parser import iter_sections
from .parse import iter_headers

DOC_ROOT = Path("MITgcm/doc")
MITGCM_ROOT = Path("MITgcm")


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
    doc_root: Path = DOC_ROOT,
    mitgcm_root: Path = MITGCM_ROOT,
    chroma_path: Path = CHROMA_PATH,
) -> None:
    sections = iter_sections(doc_root)
    log.info(f"Parsed {len(sections)} sections from {doc_root}")

    headers = iter_headers(mitgcm_root)
    log.info(f"Found {len(headers)} .h files under {mitgcm_root}")

    collection = get_docs_collection(chroma_path)

    all_chunks = []
    for idx, sec in enumerate(sections):
        all_chunks.extend(_doc_chunks(f"doc_{idx}", sec["file"], sec["section"], sec["text"]))
    for idx, hdr in enumerate(headers):
        all_chunks.extend(_doc_chunks(f"hdr_{idx}", hdr["file"], hdr["section"], hdr["text"]))
    log.info(f"Generated {len(all_chunks)} chunks from {len(sections)} sections and {len(headers)} headers")

    total = 0
    for i in range(0, len(all_chunks), BATCH_SIZE):
        batch = all_chunks[i : i + BATCH_SIZE]
        ids = [c[0] for c in batch]
        docs = [c[1] for c in batch]
        metadatas = [c[2] for c in batch]

        try:
            embeddings = ollama.embed(model=EMBED_MODEL, input=docs)["embeddings"]
        except Exception as e:
            log.warning(f"batch {i // BATCH_SIZE} failed ({e}), retrying in 10s")
            time.sleep(10)
            try:
                embeddings = ollama.embed(model=EMBED_MODEL, input=docs)["embeddings"]
            except Exception as e2:
                log.warning(f"batch still failing ({e2}), falling back to one-at-a-time")
                keep = []
                for chunk_id, d, meta in zip(ids, docs, metadatas):
                    for attempt in range(3):
                        try:
                            emb = ollama.embed(model=EMBED_MODEL, input=[d])["embeddings"][0]
                            keep.append((chunk_id, emb, d, meta))
                            break
                        except Exception as e3:
                            if "context length" in str(e3):
                                mid = len(d) // 2
                                log.warning(f"splitting {chunk_id} ({len(d)} chars) in two")
                                for suffix, half in (("_a", d[:mid]), ("_b", d[mid:])):
                                    try:
                                        emb = ollama.embed(model=EMBED_MODEL, input=[half])["embeddings"][0]
                                        keep.append((chunk_id + suffix, emb, half, meta))
                                    except Exception:
                                        log.warning(f"skipping {chunk_id}{suffix} after split")
                                break
                            if attempt == 2:
                                raise
                            time.sleep(10)
                ids, embeddings, docs, metadatas = zip(*keep) if keep else ([], [], [], [])
                ids, embeddings, docs, metadatas = list(ids), list(embeddings), list(docs), list(metadatas)

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

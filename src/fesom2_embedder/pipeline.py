"""Embedding pipeline: read FESOM2 subroutines from DuckDB, embed via ollama, write to ChromaDB.

Run as:
    pixi run fesom2-embed
"""

import argparse
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

from ..embedder.pipeline import _chunk_text, EMBED_MODEL, BATCH_SIZE, MAX_CHARS, OVERLAP
from ..fesom2_indexer.schema import DB_PATH, connect as duckdb_connect
from .store import CHROMA_PATH, get_subroutine_collection


def _doc_chunks(
    db_id: int, name: str, file: str, module_name: str, source_text: str
) -> list[tuple[str, str, dict]]:
    """Return one (chroma_id, document_text, metadata) tuple per chunk.

    Header format: SUBROUTINE {name} [{module_name}]
    Each chunk gets a unique id of the form "{db_id}_{chunk_index}".
    """
    header = f"SUBROUTINE {name} [{module_name}]\n"
    chunks = _chunk_text(source_text, MAX_CHARS, OVERLAP)
    n = len(chunks)
    return [
        (
            f"{db_id}_{i}",
            header + chunk,
            {
                "name": name,
                "file": file,
                "module_name": module_name,
                "db_id": db_id,
                "chunk_index": i,
                "n_chunks": n,
            },
        )
        for i, chunk in enumerate(chunks)
    ]


def _embed_with_retry(
    ids: list, docs: list[str], metadatas: list
) -> tuple[list, list, list, list]:
    """Embed batch with retry and one-at-a-time fallback.

    Returns (ids, embeddings, docs, metadatas) with any unembeddable items dropped.
    Shared by docs_pipeline.py and nml_pipeline.py.
    """
    try:
        embeddings = ollama.embed(model=EMBED_MODEL, input=docs)["embeddings"]
        return ids, embeddings, docs, metadatas
    except Exception as e:
        log.warning(f"batch failed ({e}), retrying in 10s")
        time.sleep(10)
        try:
            embeddings = ollama.embed(model=EMBED_MODEL, input=docs)["embeddings"]
            return ids, embeddings, docs, metadatas
        except Exception as e2:
            log.warning(f"batch still failing ({e2}), falling back to one-at-a-time")
            keep: list[tuple] = []
            for chunk_id, d, meta in zip(ids, docs, metadatas):
                for attempt in range(3):
                    try:
                        emb = ollama.embed(model=EMBED_MODEL, input=[d])["embeddings"][0]
                        keep.append((chunk_id, emb, d, meta))
                        break
                    except Exception as e3:
                        if "context length" in str(e3):
                            mid = len(d) // 2
                            log.warning(f"splitting chunk {chunk_id} ({len(d)} chars) in two")
                            for suffix, half in (("_a", d[:mid]), ("_b", d[mid:])):
                                try:
                                    emb = ollama.embed(model=EMBED_MODEL, input=[half])["embeddings"][0]
                                    keep.append((chunk_id + suffix, emb, half, meta))
                                except Exception:
                                    log.warning(f"skipping {chunk_id}{suffix} after split")
                            break
                        if attempt == 2:
                            raise
                        log.warning(f"fallback retry {attempt+1} failed ({e3}), waiting 10s")
                        time.sleep(10)
            if keep:
                out_ids, out_embs, out_docs, out_metas = zip(*keep)
                return list(out_ids), list(out_embs), list(out_docs), list(out_metas)
            return [], [], [], []


def run(db_path: Path = DB_PATH, chroma_path: Path = CHROMA_PATH, start_chunk: int = 0) -> None:
    model_info = ollama.show(EMBED_MODEL)
    num_ctx = (model_info.modelinfo or {}).get("nomic-bert.context_length", "unknown")
    log.info(f"{EMBED_MODEL} context_length={num_ctx}")

    con = duckdb_connect(db_path)
    rows = con.execute(
        "SELECT id, name, file, module_name, source_text FROM subroutines ORDER BY id"
    ).fetchall()
    con.close()
    log.info(f"Loaded {len(rows)} subroutines from DuckDB")

    collection = get_subroutine_collection(chroma_path)

    all_chunks = []
    for r in rows:
        all_chunks.extend(_doc_chunks(r[0], r[1], r[2], r[3], r[4]))
    log.info(f"Generated {len(all_chunks)} chunks from {len(rows)} subroutines")

    if start_chunk:
        log.info(f"Skipping to chunk {start_chunk}")

    total = start_chunk
    for i in range(start_chunk, len(all_chunks), BATCH_SIZE):
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

    log.info(f"Done. {collection.count()} chunks ({len(rows)} subroutines).")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--start-chunk", type=int, default=0, help="Skip to this chunk index")
    args = parser.parse_args()
    run(start_chunk=args.start_chunk)

"""Embedding pipeline: read subroutines from DuckDB, embed via ollama, write to ChromaDB."""

from pathlib import Path

import ollama

from ..indexer.schema import DB_PATH, connect as duckdb_connect
from .store import CHROMA_PATH, get_collection

EMBED_MODEL = "nomic-embed-text"
BATCH_SIZE = 10
# nomic-embed-text context window is ~2000 tokens; ~4000 chars of Fortran code
# fits safely within that budget.
MAX_CHARS = 4000
# Overlap between consecutive chunks so that content near a boundary
# appears in two chunks and is not lost to either.
OVERLAP = 200


def _chunk_text(text: str, max_chars: int, overlap: int) -> list[str]:
    """Split text into overlapping chunks of at most max_chars characters each.

    Short texts (len <= max_chars) are returned as a single-element list.
    Each chunk after the first starts overlap characters before the end of
    the previous chunk.
    """
    if len(text) <= max_chars:
        return [text]
    step = max_chars - overlap
    chunks = []
    start = 0
    while start < len(text):
        chunks.append(text[start : start + max_chars])
        start += step
    return chunks


def _doc_chunks(
    db_id: int, name: str, file: str, package: str, source_text: str
) -> list[tuple[str, str, dict]]:
    """Return one (chroma_id, document_text, metadata) tuple per chunk.

    Each chunk gets a unique id of the form "{db_id}_{chunk_index}".
    Metadata includes db_id for join-back to DuckDB, plus chunk_index
    and n_chunks for reassembly context.
    """
    header = f"SUBROUTINE {name} [{package}]\n"
    chunks = _chunk_text(source_text, MAX_CHARS, OVERLAP)
    n = len(chunks)
    return [
        (
            f"{db_id}_{i}",
            header + chunk,
            {
                "name": name,
                "file": file,
                "package": package,
                "db_id": db_id,
                "chunk_index": i,
                "n_chunks": n,
            },
        )
        for i, chunk in enumerate(chunks)
    ]


def run(db_path: Path = DB_PATH, chroma_path: Path = CHROMA_PATH) -> None:
    con = duckdb_connect(db_path)
    rows = con.execute(
        "SELECT id, name, file, package, source_text FROM subroutines ORDER BY id"
    ).fetchall()
    con.close()
    print(f"Loaded {len(rows)} subroutines from DuckDB")

    collection = get_collection(chroma_path)

    all_chunks = []
    for r in rows:
        all_chunks.extend(_doc_chunks(r[0], r[1], r[2], r[3], r[4]))
    print(f"Generated {len(all_chunks)} chunks from {len(rows)} subroutines")

    total = 0
    for i in range(0, len(all_chunks), BATCH_SIZE):
        batch = all_chunks[i : i + BATCH_SIZE]
        ids = [c[0] for c in batch]
        docs = [c[1] for c in batch]
        metadatas = [c[2] for c in batch]

        response = ollama.embed(model=EMBED_MODEL, input=docs)
        embeddings = response["embeddings"]

        collection.upsert(
            ids=ids,
            embeddings=embeddings,
            documents=docs,
            metadatas=metadatas,
        )

        total += len(batch)
        if total % 500 == 0 or total == len(all_chunks):
            print(f"  Embedded {total}/{len(all_chunks)}")

    print(f"\nDone. {collection.count()} chunks ({len(rows)} subroutines).")


if __name__ == "__main__":
    run()

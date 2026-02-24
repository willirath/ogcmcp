"""Embedding pipeline: embed FESOM2 namelist parameter descriptions into ChromaDB.

Reads the namelist_descriptions table from DuckDB and embeds each parameter
as:  "{param_name} ({namelist_group} in {config_file}): {description}"

This enables semantic search over namelist parameters alongside RST docs.

Run as:
    pixi run fesom2-embed-namelists
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

from ..embedder.pipeline import EMBED_MODEL, BATCH_SIZE
from ..fesom2_indexer.schema import DB_PATH, connect as duckdb_connect
from .pipeline import _embed_with_retry
from .store import CHROMA_PATH, get_namelists_collection


def _nml_doc(
    param_name: str, namelist_group: str, config_file: str, description: str
) -> tuple[str, str, dict]:
    """Return (chroma_id, document_text, metadata) for a namelist parameter.

    ID is {group}_{param} (lowercased) — unique by construction since
    namelist_descriptions is deduplicated by (group, param) in the pipeline.
    """
    chroma_id = f"nml_{namelist_group}_{param_name}".lower()
    doc = f"{param_name} ({namelist_group} in {config_file}): {description}"
    return (
        chroma_id,
        doc,
        {
            "param_name": param_name,
            "namelist_group": namelist_group,
            "config_file": config_file,
        },
    )


def run(db_path: Path = DB_PATH, chroma_path: Path = CHROMA_PATH) -> None:
    con = duckdb_connect(db_path)
    rows = con.execute(
        "SELECT param_name, namelist_group, config_file, description FROM namelist_descriptions"
    ).fetchall()
    con.close()
    log.info(f"Loaded {len(rows)} namelist descriptions from DuckDB")

    collection = get_namelists_collection(chroma_path)

    all_entries = [_nml_doc(r[0], r[1], r[2], r[3]) for r in rows]

    total = 0
    for i in range(0, len(all_entries), BATCH_SIZE):
        batch = all_entries[i : i + BATCH_SIZE]
        ids = [e[0] for e in batch]
        docs = [e[1] for e in batch]
        metadatas = [e[2] for e in batch]

        ids, embeddings, docs, metadatas = _embed_with_retry(ids, docs, metadatas)
        if ids:
            collection.upsert(
                ids=ids,
                embeddings=embeddings,
                documents=docs,
                metadatas=metadatas,
            )

        total += len(batch)
        if total % 100 == 0 or total == len(all_entries):
            log.info(f"  Embedded {total}/{len(all_entries)}")

    log.info(f"Done. {collection.count()} namelist parameter embeddings.")


if __name__ == "__main__":
    run()

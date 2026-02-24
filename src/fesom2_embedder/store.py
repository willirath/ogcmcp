"""ChromaDB client setup and FESOM2 collection access."""

import chromadb
from pathlib import Path

CHROMA_PATH = Path("data/fesom2/chroma")

FESOM2_SUBROUTINES_COLLECTION = "fesom2_subroutines"
FESOM2_DOCS_COLLECTION = "fesom2_docs"
FESOM2_NAMELISTS_COLLECTION = "fesom2_namelists"


def get_collection(name: str, path: Path = CHROMA_PATH) -> chromadb.Collection:
    """Return (or create) a named ChromaDB collection at the given path."""
    client = chromadb.PersistentClient(path=str(path))
    return client.get_or_create_collection(
        name=name,
        metadata={"hnsw:space": "cosine"},
    )


def get_subroutine_collection(path: Path = CHROMA_PATH) -> chromadb.Collection:
    return get_collection(FESOM2_SUBROUTINES_COLLECTION, path)


def get_docs_collection(path: Path = CHROMA_PATH) -> chromadb.Collection:
    return get_collection(FESOM2_DOCS_COLLECTION, path)


def get_namelists_collection(path: Path = CHROMA_PATH) -> chromadb.Collection:
    return get_collection(FESOM2_NAMELISTS_COLLECTION, path)

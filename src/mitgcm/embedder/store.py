"""ChromaDB client setup and collection access."""

import chromadb
from pathlib import Path

CHROMA_PATH = Path("data/mitgcm/chroma")
COLLECTION_NAME = "subroutines"
DOCS_COLLECTION_NAME = "mitgcm_docs"
VERIFICATION_COLLECTION_NAME = "mitgcm_verification"


def get_collection(name: str, path: Path = CHROMA_PATH) -> chromadb.Collection:
    """Return (or create) a named ChromaDB collection at the given path."""
    client = chromadb.PersistentClient(path=str(path))
    return client.get_or_create_collection(
        name=name,
        metadata={"hnsw:space": "cosine"},
    )


def get_subroutine_collection(path: Path = CHROMA_PATH) -> chromadb.Collection:
    return get_collection(COLLECTION_NAME, path)


def get_docs_collection(path: Path = CHROMA_PATH) -> chromadb.Collection:
    return get_collection(DOCS_COLLECTION_NAME, path)


def get_verification_collection(path: Path = CHROMA_PATH) -> chromadb.Collection:
    return get_collection(VERIFICATION_COLLECTION_NAME, path)

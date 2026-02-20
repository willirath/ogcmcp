"""ChromaDB client setup and collection access."""

import chromadb
from pathlib import Path

CHROMA_PATH = Path("data/chroma")
COLLECTION_NAME = "subroutines"


def get_collection(path: Path = CHROMA_PATH) -> chromadb.Collection:
    client = chromadb.PersistentClient(path=str(path))
    return client.get_or_create_collection(
        name=COLLECTION_NAME,
        metadata={"hnsw:space": "cosine"},
    )

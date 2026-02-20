# ChromaDB semantic index

## Overview

The ChromaDB index stores vector embeddings of MITgcm subroutines and supports
natural-language search over the codebase. It lives at `data/chroma/` (gitignored)
and is built from the DuckDB code graph by `src/embedder/`.

## Modules

### `store.py` — client setup

Defines `CHROMA_PATH` (`data/chroma/`) and `COLLECTION_NAME` (`subroutines`).
Exposes a single function:

```python
get_collection(path: Path = CHROMA_PATH) -> chromadb.Collection
```

Opens or creates a `PersistentClient` at `path` and returns the collection,
creating it with cosine similarity if it does not yet exist. The caller is
responsible for not holding the client across process boundaries.

### `pipeline.py` — embedding pipeline

Reads all subroutines from DuckDB, chunks them, embeds each chunk via ollama,
and upserts into ChromaDB.

Key constants:

| Constant | Value | Meaning |
|---|---|---|
| `EMBED_MODEL` | `nomic-embed-text` | ollama embedding model |
| `MAX_CHARS` | 4000 | maximum chars per chunk |
| `OVERLAP` | 200 | chars shared between adjacent chunks |
| `BATCH_SIZE` | 10 | chunks sent to ollama per request |

### Error handling

Each batch embed is wrapped in a two-level fallback:

1. **Retry once** after a 2-second pause — handles transient server errors
   (observed: ollama returning HTTP 400 immediately after a cold start or
   container restart).
2. **One-at-a-time fallback** — if the retry also fails, each document in the
   batch is embedded individually. This eliminates any risk of a combined
   batch exceeding the model's context window.

## Chunking

`nomic-embed-text` has a context window of approximately 2000 tokens, which
corresponds to roughly 4000 characters of Fortran source. Many MITgcm
subroutines are larger than this — the largest exceeds 100 000 characters.

To avoid truncation, each subroutine is split into overlapping chunks:

```
_chunk_text(source_text, max_chars=4000, overlap=200)
```

Short subroutines (≤ 4000 chars) produce a single chunk. Longer ones produce
multiple chunks, each sharing 200 characters with its neighbours so that
content near a boundary is not missed by either chunk's embedding.

Each chunk is stored as a separate ChromaDB entry. The ChromaDB id is
`"{db_id}_{chunk_index}"`, e.g. `"42_0"`, `"42_1"`. All chunks carry the
same `db_id` metadata field for join-back to the DuckDB `subroutines` table.

## Document format

Every chunk document is prefixed with a header line:

```
SUBROUTINE <name> [<package>]
<chunk of source_text>
```

The header gives the embedding model context about what is being embedded,
anchoring the chunk to its subroutine name and package even when the source
text alone would be ambiguous.

## Metadata schema

Each ChromaDB entry carries:

| Field | Type | Content |
|---|---|---|
| `name` | str | subroutine name (uppercase) |
| `file` | str | path relative to project root |
| `package` | str | package name (`model`, `seaice`, …) |
| `db_id` | int | DuckDB `subroutines.id` for join-back |
| `chunk_index` | int | 0-based index within the subroutine |
| `n_chunks` | int | total chunks for this subroutine |

## Building the index

```sh
docker compose up -d                  # ollama must be running
pixi run embed
```

To rebuild from scratch:

```sh
rm -rf data/chroma
pixi run embed
```

The DuckDB index must exist first (`pixi run index`). The embedder reads from
`data/index.duckdb` and writes to `data/chroma/`.

## Querying

```python
import ollama
from src.embedder.store import get_collection

collection = get_collection()

query = "non-hydrostatic pressure solve"
q_vec = ollama.embed(model="nomic-embed-text", input=[query])["embeddings"][0]

results = collection.query(
    query_embeddings=[q_vec],
    n_results=10,
    include=["metadatas", "distances", "documents"],
)

for meta, dist in zip(results["metadatas"][0], results["distances"][0]):
    print(f"{dist:.3f}  {meta['name']}  [{meta['package']}]  chunk {meta['chunk_index']}/{meta['n_chunks']}")
```

Results may include multiple chunks from the same subroutine. Deduplicate by
`db_id` to get a ranked list of subroutines:

```python
seen = {}
for meta, dist in zip(results["metadatas"][0], results["distances"][0]):
    db_id = meta["db_id"]
    if db_id not in seen or dist < seen[db_id][0]:
        seen[db_id] = (dist, meta["name"], meta["package"])

for dist, name, pkg in sorted(seen.values()):
    print(f"{dist:.3f}  {name}  [{pkg}]")
```

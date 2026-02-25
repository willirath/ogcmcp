# ChromaDB semantic index

## Overview

ChromaDB stores vector embeddings for semantic search over source code and
documentation. Both backends maintain their own collections under separate
`data/<backend>/chroma/` directories but share the same embedding model
(`nomic-embed-text` via Ollama).

---

## MITgcm collections

Three collections, each built by a separate pipeline:

| Collection | Pipeline | Content |
|---|---|---|
| `subroutines` | `pixi run mitgcm-embed` | `.F` / `.F90` subroutine source |
| `mitgcm_docs` | `pixi run mitgcm-embed-docs` | RST documentation + `.h` header files |
| `mitgcm_verification` | `pixi run mitgcm-embed-verification` | Verification experiment namelists and code |

All three share the same `data/mitgcm/chroma/` path.

### File type coverage

| File type | Location | Count | Indexed in | Notes |
|---|---|---|---|---|
| `.F` | `model/src/`, `pkg/*/` | ~2100 | `subroutines` | Full coverage |
| `.F90` | various | ~37 | `subroutines` | Full coverage |
| `.h` (experiment) | `verification/*/code/` | ~500 | `mitgcm_docs` | Experiment-level overrides |
| `.h` (model/inc) | `model/inc/` | 46 | `mitgcm_docs` | PARAMS.h, DYNVARS.h, GRID.h, … |
| `.h` (eesupp/inc) | `eesupp/inc/` | 16 | `mitgcm_docs` | EXCH.h, EEPARAMS.h, … |
| `.rst` | `doc/` | ~85 | `mitgcm_docs` | Full RST documentation |
| namelist (`data*`) | `verification/*/input/` | — | `mitgcm_verification` | Per-experiment physics config |
| `packages.conf` | `verification/*/code/` | — | `mitgcm_verification` | Package selection |
| `.py`, `.m`, `.c` | various | many | — | Build/analysis tools; not indexed |
| `.bin`, `.nc`, `.data` | various | many | — | Binary data; not indexed |

### Modules

#### `store.py` — client setup

Defines `CHROMA_PATH` (`data/mitgcm/chroma/`) and collection name constants.
Exposes one function per collection plus a generic:

```python
get_collection(name: str, path: Path = CHROMA_PATH) -> chromadb.Collection
```

Opens or creates a `PersistentClient` at `path` and returns the named
collection, creating it with cosine similarity if it does not yet exist.

#### `pipeline.py` — subroutine embedding pipeline

Reads all subroutines from `data/mitgcm/index.duckdb`, chunks them, embeds
each chunk via Ollama, and upserts into the `subroutines` collection.

Key constants (defined in `src/embed_utils.py`):

| Constant | Value | Meaning |
|---|---|---|
| `EMBED_MODEL` | `nomic-embed-text` | Ollama embedding model |
| `MAX_CHARS` | 4000 | maximum chars per chunk |
| `OVERLAP` | 200 | chars shared between adjacent chunks |
| `BATCH_SIZE` | 10 | chunks sent to Ollama per request |

#### Error handling

Each batch embed is wrapped in a two-level fallback:

1. **Retry once** after a 10-second pause — handles transient server errors.
   Observed cause: resource contention when concurrent MCP calls saturate
   the Ollama container, causing spurious HTTP 400 "context length" rejections
   even for documents well within the 8192-token limit.
2. **One-at-a-time fallback** — if the retry also fails, each document in the
   batch is embedded individually, each with up to 3 attempts (10 s apart).
   If a document fails all attempts with "input length exceeds context length",
   it is skipped with a warning. All retries, fallbacks, and skips are logged.

Chunks that still exceed the context limit after the first retry are split
at the midpoint and both halves embedded separately (ids get `_a`/`_b` suffix).

### Chunking

`nomic-embed-text` has a context window of approximately 2000 tokens (~4000
characters of Fortran source). Many MITgcm subroutines are larger — the
largest exceeds 100 000 characters.

Each subroutine is split into overlapping chunks via `_chunk_text` in
`src/embed_utils.py` (`max_chars=4000, overlap=200`). Short subroutines
(≤ 4000 chars) produce a single chunk. Each chunk is stored as a separate
ChromaDB entry with id `"{db_id}_{chunk_index}"`.

### Document format

Every chunk document is prefixed with a context header:

```
SUBROUTINE <name> [<package>]
<chunk of source_text>
```

### Metadata schema — `subroutines`

| Field | Type | Content |
|---|---|---|
| `name` | str | subroutine name (uppercase) |
| `file` | str | path relative to project root |
| `package` | str | package name (`model`, `seaice`, …) |
| `db_id` | int | DuckDB `subroutines.id` for join-back |
| `chunk_index` | int | 0-based index within the subroutine |
| `n_chunks` | int | total chunks for this subroutine |

### Building the `subroutines` index

```sh
docker compose up -d            # Ollama must be running
pixi run mitgcm-embed
```

To rebuild from scratch:

```sh
rm -rf data/mitgcm/chroma
pixi run mitgcm-embed
```

`data/mitgcm/index.duckdb` must exist first (`pixi run mitgcm-index`).

### Querying

```python
import ollama
from src.mitgcm.embedder.store import get_subroutine_collection

collection = get_subroutine_collection()
q_vec = ollama.embed(model="nomic-embed-text", input=["non-hydrostatic pressure solve"])["embeddings"][0]

results = collection.query(
    query_embeddings=[q_vec],
    n_results=10,
    include=["metadatas", "distances", "documents"],
)
```

Deduplicate multiple chunks from the same subroutine by `db_id` before
presenting results.

---

### `mitgcm_docs` collection

Built by `src/mitgcm/docs_indexer/` and populated with
`pixi run mitgcm-embed-docs`.

#### RST parsing

`src/mitgcm/docs_indexer/parse.py` works in two passes:

1. **Section splitting** (`_split_sections`): walks lines looking for RST
   heading underline patterns (a line of repeated `=`, `-`, `~`, `+`, `#`,
   `*`, or `^` that is at least as long as the preceding text line). Each
   heading starts a new section.

2. **Markup stripping** (`_clean_text`): removes RST directives
   (`.. math::`, `.. code-block::`, `.. tabularcolumns::`, `.. _label:`,
   etc.) and their indented continuation blocks; strips inline roles
   (`:varlink:\`f0\`` → `f0`); removes grid-table border lines; collapses
   runs of blank lines.

Sections with no remaining text after cleaning are dropped (e.g. pure
toctree files).

#### Content

Two sources are merged:

- **RST documentation** (`MITgcm/doc/**/*.rst`): parsed into sections
  (heading + body), RST markup stripped.
- **Header files** (`.h`): raw content from three locations:
  - `MITgcm/verification/*/code/*.h` — experiment-level overrides
  - `MITgcm/model/inc/*.h` — core headers (`PARAMS.h`, `DYNVARS.h`, …)
  - `MITgcm/eesupp/inc/*.h` — execution environment headers

#### Metadata schema — `mitgcm_docs`

| Field | Type | Content |
|---|---|---|
| `file` | str | path relative to `MITgcm/` or the doc root |
| `section` | str | RST heading text, or filename for `.h` files |
| `chunk_index` | int | 0-based chunk index |
| `n_chunks` | int | total chunks for this section |
| `section_id` | str | stable id for join-back (`doc_N` or `hdr_N`) |

#### Limitations

- Table content is partially preserved: cell text is kept but column
  structure is lost after stripping grid-table borders.
- Mathematical equations in `.. math::` blocks are stripped entirely.
- Cross-reference role labels (`:numref:`, `:ref:`, `:cite:`) are reduced
  to their label text, which may be cryptic.

#### Building

```sh
docker compose up -d
pixi run mitgcm-embed-docs
```

---

### `mitgcm_verification` collection

Built by `src/mitgcm/verification_indexer/` and populated with
`pixi run mitgcm-embed-verification`.

#### Content

All text files under `MITgcm/verification/*/input/` and
`MITgcm/verification/*/code/`:

- **Namelists**: `input/data`, `input/data.pkg`, `input/data.*`, `input/eedata`
- **Headers and package config**: `code/*.h`, `code/packages.conf`
- **Source**: `code/*.F`, `code/*.F90`

Binary and generated files (`.bin`, `.nc`, `.data`, `.meta`, `.gz`) are
skipped.

#### Metadata schema — `mitgcm_verification`

| Field | Type | Content |
|---|---|---|
| `experiment` | str | experiment directory name |
| `file` | str | `<experiment>/input/<name>` or `<experiment>/code/<name>` |
| `filename` | str | bare filename |
| `chunk_index` | int | 0-based chunk index |

#### Building

```sh
docker compose up -d
pixi run mitgcm-embed-verification
```

---

## FESOM2 collections

Three collections, each built by a separate pipeline:

| Collection | Pipeline | Content |
|---|---|---|
| `fesom2_subroutines` | `pixi run fesom2-embed` | F90 subroutine source |
| `fesom2_docs` | `pixi run fesom2-embed-docs` | RST documentation from `FESOM2/docs/` |
| `fesom2_namelists` | `pixi run fesom2-embed-namelists` | Namelist parameter descriptions from config files |

All three share `data/fesom2/chroma/`. The `fesom2-embed` pipeline requires
`data/fesom2/index.duckdb` to exist first (`pixi run fesom2-index`).
`fesom2-embed-namelists` also reads from DuckDB (`namelist_descriptions`
table). `fesom2-embed-docs` reads RST files directly and has no DuckDB
dependency.

### `fesom2_subroutines` collection

Metadata schema mirrors the MITgcm subroutines collection but uses
`module_name` instead of `package`:

| Field | Type | Content |
|---|---|---|
| `name` | str | subroutine name (uppercase) |
| `file` | str | path relative to project root |
| `module_name` | str | enclosing F90 module name |
| `db_id` | int | DuckDB `subroutines.id` for join-back |
| `chunk_index` | int | 0-based chunk index |
| `n_chunks` | int | total chunks for this subroutine |

### `fesom2_docs` collection

RST sections from `FESOM2/docs/`. Parsed using the same shared
`src/rst_parser.py` as the MITgcm docs pipeline. Metadata:

| Field | Type | Content |
|---|---|---|
| `file` | str | path relative to `FESOM2/docs/` |
| `section` | str | RST heading text |
| `chunk_index` | int | 0-based chunk index |
| `n_chunks` | int | total chunks for this section |
| `section_id` | str | stable id for join-back (`doc_N`) |

### `fesom2_namelists` collection

Each document is a string of the form:

```
K_GM (namelist_oce in FESOM2/config/namelist.oce): GM diffusivity (m²/s)
```

This enables semantic search over parameter *meanings*, complementing the
source-level `namelist_refs` table (which tracks *where* parameters are
declared in source). Metadata:

| Field | Type | Content |
|---|---|---|
| `param_name` | str | parameter name as it appears in the config file |
| `namelist_group` | str | namelist group name (e.g. `namelist_oce`) |
| `config_file` | str | path to the config file |

Descriptions come from inline comments in `FESOM2/config/namelist.*` files,
collected by `src/fesom2/indexer/namelist_config.py` and stored in
`namelist_descriptions` in DuckDB before embedding.

### Building the FESOM2 index

```sh
docker compose up -d            # Ollama must be running

pixi run fesom2-index           # populate data/fesom2/index.duckdb first
pixi run fesom2-embed
pixi run fesom2-embed-docs
pixi run fesom2-embed-namelists
```

To rebuild from scratch:

```sh
rm -rf data/fesom2
pixi run fesom2-index
pixi run fesom2-embed
pixi run fesom2-embed-docs
pixi run fesom2-embed-namelists
```

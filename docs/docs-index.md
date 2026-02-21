# MITgcm Documentation Index (M8)

## Purpose

Indexes the MITgcm RST documentation into a ChromaDB collection (`mitgcm_docs`)
so that natural-language queries can retrieve relevant prose sections —
parameter descriptions, package tutorials, algorithm explanations — rather than
only Fortran source code.

## Components

```
src/docs_indexer/
├── __init__.py
├── parse.py       RST → plain-text sections
└── pipeline.py    sections → ChromaDB mitgcm_docs collection
```

`search_docs` in `src/tools.py` queries the collection.
`search_docs_tool` in `src/server.py` exposes it via MCP.

## RST parsing

`parse.py` works in two passes:

1. **Section splitting** (`_split_sections`): walks lines looking for RST
   heading underline patterns (a line of repeated `=`, `-`, `~`, `+`, `#`,
   `*`, or `^` that is at least as long as the preceding text line). Each
   heading starts a new section.

2. **Markup stripping** (`_clean_text`): removes RST directives (`.. math::`,
   `.. code-block::`, `.. tabularcolumns::`, `.. _label:`, etc.) and their
   indented continuation blocks; strips inline roles (`:varlink:\`f0\`` →
   `f0`); removes grid-table border lines; collapses runs of blank lines.

Sections with no remaining text after cleaning are dropped (e.g. pure toctree
files).

## Embedding

Same model and parameters as the subroutines pipeline:

- Model: `nomic-embed-text` via Ollama
- Chunk size: 4000 chars with 200-char overlap
- Batch size: 10
- Collection: `mitgcm_docs` in `data/chroma/` (same ChromaDB instance as
  `subroutines`, different collection name)

Each chunk's metadata carries `file` (path relative to `MITgcm/doc/`),
`section` (heading text), `chunk_index`, `n_chunks`.

## Running the pipeline

```
# Start the Ollama embedding server first:
docker compose up -d

# Parse and embed all RST docs:
pixi run embed-docs
```

Indexing ~85 RST files produces roughly 500–800 chunks and takes a few minutes
on CPU.

## `search_docs` tool

```python
from src.tools import search_docs

results = search_docs("how to set the Coriolis parameter", top_k=3)
# → [{"file": "getting_started/getting_started.rst",
#     "section": "Equation of state",
#     "snippet": "...f0=-2*Omega..."},
#    ...]
```

Each result has `file`, `section`, `snippet` (first 400 chars of the matched
section text).

## MCP tool

`search_docs_tool(query, top_k=5)` — registered in `src/server.py`. Takes a
natural-language query and returns matching documentation sections.

## Limitations

- Table content is partially preserved: cell text is kept but table structure
  (column alignment, headers) is lost after stripping grid-table borders.
- Mathematical equations in `.. math::` blocks are stripped entirely; the
  surrounding prose is kept.
- Cross-reference role labels (`:numref:`, `:ref:`, `:cite:`) are reduced to
  their label text, which may be cryptic (e.g. citation keys).
- The `references.rst` file (bibliography) is indexed but rarely useful for
  semantic search.

# MITgcm rotating-tank knowledge system

An AI-assisted system for configuring and extending MITgcm for idealised
rotating-tank experiments at tabletop scale. See `plans/` for the full design
and roadmap.

## Status

Working through the build roadmap (`plans/roadmap.md`). M0–M2 complete.

| Milestone | What | Status |
|---|---|---|
| M0 | Environment, MITgcm submodule, embeddings server | ✓ |
| M1 | DuckDB code graph (2505 subroutines indexed) | ✓ |
| M2 | ChromaDB semantic index | ✓ |
| M3 | Core query tools | — |
| M4 | MCP server | — |
| M5 | Domain knowledge layer | — |
| M6 | First real experiment | — |

## Quick start

```sh
# 1. Install dependencies
pixi install

# 2. Start the embedding server
docker compose up -d
docker compose exec ollama ollama pull nomic-embed-text  # first time only

# 3. Build the code index
pixi run index

# 4. Run tests
pixi run test
```

## Layout

```
.
├── src/
│   ├── indexer/       Fortran source parser and DuckDB pipeline
│   └── embedder/      ChromaDB embedding pipeline
├── tests/
│   ├── indexer/       Tests for the indexer
│   └── embedder/      Tests for the embedder
├── docs/              Implementation notes (one file per component)
├── plans/             Design sketch and milestone roadmap
├── MITgcm/            MITgcm source (git submodule, pinned)
├── compose.yml        Docker service for the ollama embedding server
└── data/              Generated artifacts — gitignored (index.duckdb, chroma/)
```

## Docs

| File | Covers |
|---|---|
| `docs/environment.md` | pixi setup, dependencies |
| `docs/mitgcm-source.md` | git submodule, source layout, updating |
| `docs/embeddings.md` | ollama Docker setup, embedding model |
| `docs/parsing.md` | Fortran extraction approach, fixed-form specifics |
| `docs/duckdb.md` | Code graph schema, example queries |
| `docs/indexer.md` | Indexer modules, data flow, extension guide |
| `docs/testing.md` | Test structure, conventions, adversarial tests |
| `docs/chromadb.md` | Embedding pipeline, chunking, query examples |

# MITgcm rotating-tank knowledge system

An AI-assisted system for configuring and extending MITgcm for idealised
rotating-tank experiments at tabletop scale. See `plans/` for the full design
and roadmap.

## Status

Working through the build roadmap (`plans/roadmap.md`).

| Milestone | What | Status |
|---|---|---|
| M0 | Environment, MITgcm submodule, embeddings server | ✓ |
| M1 | DuckDB code graph (2505 subroutines indexed) | ✓ |
| M2 | ChromaDB semantic index (4910 chunks) | ✓ |
| M3 | Core query tools | ✓ |
| M4 | MCP server | ✓ |
| M5 | Domain knowledge layer | — |
| M6 | First real experiment | — |

## Setup

### Prerequisites

- [pixi](https://pixi.sh) — Python environment and task runner
- Docker (with Compose) — runs the ollama embedding server
- git

### 1. Clone with the submodule

The MITgcm source is a git submodule — use `--recurse-submodules` or the
directory will be empty:

```sh
git clone --recurse-submodules https://github.com/willirath/2026_mitgcm_mcp
```

### 2. Install Python dependencies

```sh
pixi install
```

### 3. Start the embedding server

```sh
docker compose up -d
docker compose exec ollama ollama pull nomic-embed-text  # first time only
```

### 4. Build the indices

```sh
pixi run index   # parse Fortran → DuckDB code graph (~2 min)
pixi run embed   # embed chunks → ChromaDB semantic index (~45 min)
```

Both are required for the MCP tools to work. `embed` calls the Ollama server,
so Docker must be running.

### 5. Run the tests

```sh
pixi run test
```

### 6. Connect to Claude Code

The `.mcp.json` file tells Claude Code how to start the MCP server. No manual
startup is needed — open the project directory in Claude Code and the server
launches automatically via `pixi run serve`.

## Layout

```
.
├── src/
│   ├── indexer/       Fortran source parser and DuckDB pipeline
│   ├── embedder/      ChromaDB embedding pipeline
│   ├── tools.py       Plain Python callables over DuckDB + ChromaDB
│   └── server.py      FastMCP stdio server wrapping the tools
├── tests/
│   ├── indexer/       Tests for the indexer
│   ├── embedder/      Tests for the embedder
│   └── tools/         Tests for the query tools
├── docs/              Implementation notes (one file per component)
├── plans/             Design docs and milestone roadmap
├── MITgcm/            MITgcm source (git submodule, pinned)
├── .mcp.json          Claude Code MCP server auto-discovery
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
| `docs/tools.md` | Query tool functions, return shapes, example session |
| `docs/mcp-server.md` | MCP server, tool list, Claude Code integration |

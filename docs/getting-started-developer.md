# Getting started (developer)

How to go from a clean checkout to a running MCP server.

---

## Prerequisites

- [pixi](https://prefix.dev/docs/pixi/overview) — used for all Python
  dependencies; install once from the pixi website
- Docker Desktop — for the Ollama embedding server and Docker image builds
- `gh` CLI — for pushing releases to GHCR (not required for local development)

---

## 1. Clone and initialise submodules

```bash
git clone https://github.com/willirath/2026-dtg
cd 2026-dtg

git submodule update --init MITgcm    # ~200 MB shallow clone
git submodule update --init FESOM2    # ~50 MB
```

Only initialise the submodule(s) you plan to work with.

---

## 2. Install Python dependencies

```bash
pixi install
```

This creates a pixi-managed conda environment at `.pixi/envs/default/`.
All subsequent `pixi run` commands use this environment automatically.

---

## 3. Start the Ollama embedding server

All indexing and embedding pipelines require Ollama running on
`localhost:11434`.

**For indexing runs** (fast): use the native Ollama binary, which uses Metal
on Apple Silicon (~26× faster than Docker). See `docs/embeddings.md` for
install instructions, then:

```bash
OLLAMA_MODELS="$(pwd)/ollama_data/models" ~/.local/bin/ollama serve &
```

**For local MCP server development** (one query at a time, speed not
critical): Docker is fine:

```bash
docker compose up -d
docker compose exec ollama ollama pull nomic-embed-text   # first time only
```

The `ollama_data/` directory is shared between Docker and the native binary —
`ollama pull` only ever needs to run once.

---

## 4. Build the MITgcm index

```bash
pixi run mitgcm-index               # ~3 min; writes data/mitgcm/index.duckdb
pixi run mitgcm-embed               # ~30 min CPU / ~2 min Apple Silicon Metal
pixi run mitgcm-embed-docs          # ~5 min; RST docs + header files
pixi run mitgcm-embed-verification  # ~10 min; verification experiment files
```

**Task ordering:** `mitgcm-embed` reads from `data/mitgcm/index.duckdb`, so
`mitgcm-index` must run first. The docs and verification pipelines are
independent of each other and of `mitgcm-embed`.

Approximate sizes after completion:

| Path | Size |
|---|---|
| `data/mitgcm/index.duckdb` | ~33 MB |
| `data/mitgcm/chroma/` | ~145 MB |

---

## 5. Build the FESOM2 index

```bash
pixi run fesom2-index               # ~1 min; writes data/fesom2/index.duckdb
pixi run fesom2-embed               # ~10 min CPU / ~1 min Apple Silicon Metal
pixi run fesom2-embed-docs          # ~2 min; RST docs from FESOM2/docs/
pixi run fesom2-embed-namelists     # <1 min; namelist parameter descriptions
```

**Task ordering:** `fesom2-embed` and `fesom2-embed-namelists` both read from
`data/fesom2/index.duckdb`, so `fesom2-index` must run first. `fesom2-embed-docs`
reads RST files directly and can run independently.

---

## 6. Run the tests

```bash
pixi run test
```

All tests use synthetic fixtures and do not require the indices to be
populated. Tests should pass on a clean checkout without running any
indexing pipelines.

---

## 7. Start a local MCP server

```bash
pixi run mitgcm-serve     # MITgcm MCP server on stdio
pixi run fesom2-serve     # FESOM2 MCP server on stdio
```

Both servers start on stdio and are intended to be invoked by Claude Code
via `claude mcp add`. To test manually, pipe JSON-RPC messages to stdin.

To configure in Claude Code for local development:

```bash
claude mcp add --transport stdio --scope user mitgcm -- \
  pixi run --manifest-path /path/to/2026-dtg/pixi.toml mitgcm-serve

claude mcp add --transport stdio --scope user fesom2 -- \
  pixi run --manifest-path /path/to/2026-dtg/pixi.toml fesom2-serve
```

---

## 8. Build Docker images locally

```bash
pixi run build-mcp-image           # ogcmcp:latest (MITgcm MCP, amd64)
pixi run build-fesom2-mcp-image    # ogcmcp-fesom2:latest (FESOM2 MCP, amd64)
```

Both require the respective `data/<backend>/` directories to be populated
(steps 4–5 above). The images bake in the Ollama binary, model weights, and
pre-built indices.

---

## Data directory layout

```
data/
├── mitgcm/
│   ├── index.duckdb   MITgcm code graph  (from mitgcm-index)
│   └── chroma/        MITgcm embeddings  (from mitgcm-embed*)
└── fesom2/
    ├── index.duckdb   FESOM2 code graph  (from fesom2-index)
    └── chroma/        FESOM2 embeddings  (from fesom2-embed*)
```

`data/` is gitignored. Delete a subdirectory and re-run the relevant
pipelines to rebuild after updating a submodule.

---

## After updating a submodule

```bash
# Example: update FESOM2 to latest upstream
git submodule update --remote FESOM2
git add FESOM2
git commit -m "Update FESOM2 submodule to <sha>"

# Rebuild indices
rm -rf data/fesom2
pixi run fesom2-index
pixi run fesom2-embed
pixi run fesom2-embed-docs
pixi run fesom2-embed-namelists
```

The MCP server reads from `data/` at query time, so any running server
instance must be restarted after rebuilding.

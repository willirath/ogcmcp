# Release architecture — final decisions

## Distribution model

Ship a self-contained Docker image. No pip package for v0.1.

The image bundles:
- Ollama (embedding daemon)
- `nomic-embed-text` model (baked in at build time, ~274 MB)
- Python application (`src/`)
- Pre-built indices (`data/` — DuckDB 33 MB + ChromaDB 145 MB)

Total image size approximately 500–600 MB compressed.

The MITgcm source is **not** bundled. MIT licence makes it technically fine to
include, but the default is to keep it out. Users who need to build or run
MITgcm use the separate `docker/mitgcm/` image.

## User install experience

```
# Add to .mcp.json — that's it
{"command": "docker", "args": ["run", "--rm", "-i", "ghcr.io/willirath/mitgcm-mcp:latest"]}
```

No Ollama installation, no model pull, no compose setup, no Python environment.
One Docker pull, one config line.

## Image design

Two-stage build in `docker/mcp/Dockerfile`:

- **Stage 1** (`ollama/ollama` base): start Ollama daemon, pull
  `nomic-embed-text`, capture model files
- **Stage 2** (`ollama/ollama` base): copy model from stage 1, install Python
  3.13 + pip dependencies, copy `src/` and `data/`

Entrypoint (`docker/mcp/entrypoint.sh`): start `ollama serve` in the
background, wait ~2 s, then `exec python3 -m src.server` in the foreground.
MCP stdio transport attaches to the server process directly.

No `OLLAMA_HOST` environment variable change is needed — Ollama runs on
`localhost:11434` inside the container, which is already the client default.

## Repo structure changes

```
docker/
    mitgcm/
        Dockerfile          # moved from root — MITgcm runtime (gfortran/OpenMPI/NetCDF)
    mcp/
        Dockerfile          # new — self-contained user image
        entrypoint.sh       # starts ollama + MCP server
        .dockerignore       # excludes MITgcm/, .git/, experiments/, tests/, etc.
compose.yml                 # dev-only — Ollama service for pixi run embed/embed-docs
.mcp.json                   # updated — docker run one-liner
pixi.toml                   # build-image path updated; build-mcp-image task added
```

`compose.yml` is no longer user-facing. It exists so developers can run the
embedding pipelines (`pixi run embed`, `pixi run embed-docs`) against a local
Ollama service without the full bundled image.

## Build prerequisites

`docker build -f docker/mcp/Dockerfile` requires `data/` to be populated
(i.e. `pixi run index`, `pixi run embed`, `pixi run embed-docs` must have been
run first). This is intentional — the image publisher generates the indices;
users consume the pre-built image.

A `pixi run build-mcp-image` task will document and automate the build step.

## Index hosting

Indices travel inside the Docker image — no separate download step or external
host. Zenodo deposit is a far-future option if a paper requires a citable
artifact.

## Embedder

Ollama with `nomic-embed-text`. Switching to `sentence-transformers` is
deferred — it would require rebuilding all indices and is a separable task.

## Package scope

All three layers (code search, docs search, domain knowledge) in one image.
No graceful degradation — if the image is built without indices, the search
tools are broken. This is the expected failure mode; the fix is to rebuild with
populated `data/`.

## Academic evaluation angle

Deferred to backlog. Whether a paper results depends on performance.

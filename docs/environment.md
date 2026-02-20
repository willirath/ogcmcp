# Environment

## Package manager

[pixi](https://pixi.sh) manages the Python environment and dependencies.
All dependencies are declared in `pixi.toml`; the resolved versions are
locked in `pixi.lock`. Both files are committed to the repo.

```sh
pixi install        # create / sync the environment
pixi run python     # run Python inside the environment
```

## Dependencies

| Package | Source | Purpose |
|---|---|---|
| `python=3.13` | conda-forge | interpreter (3.14 lacks onnxruntime wheels) |
| `duckdb` | PyPI | code graph database |
| `chromadb>=1.0` | PyPI | vector store |
| `ollama` | PyPI | Python client for the embedding server |
| `tree-sitter` | PyPI | Fortran source parsing |
| `fastapi` | PyPI | MCP server |

## Services

The ollama embedding server runs as a Docker container:

```sh
docker compose up -d    # start
docker compose down     # stop
```

See `docs/embeddings.md` for details.

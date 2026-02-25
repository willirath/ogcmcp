# Embeddings

## Model

`nomic-embed-text` — a 768-dimensional general-purpose embedding model from
Nomic AI, served via Ollama. Used by **both** the MITgcm and FESOM2 backends
to embed source text, documentation, and namelist descriptions so that
natural-language queries can retrieve relevant code and configuration.

## Infrastructure

Two modes are supported. Both listen on `localhost:11434`; the `ollama`
Python client connects there by default with no configuration needed.

### Docker (MCP server query time)

```sh
docker compose up -d                                      # start server
docker compose exec ollama ollama pull nomic-embed-text  # first-time setup
```

The Docker container runs CPU-only — Docker Desktop on macOS has no Metal
passthrough. This is acceptable for the MCP server, which embeds only one
query string per tool call. It is too slow (~7 min/100 chunks) for indexing runs.

### Native binary (indexing)

The native Ollama binary uses Metal and is ~26× faster on Apple Silicon.
Install once from the GitHub releases page (no package manager needed):

```sh
mkdir -p ~/.local/lib/ollama ~/.local/bin
# download ollama-darwin.tgz from github.com/ollama/ollama/releases
tar -xzf ollama-darwin.tgz -C ~/.local/lib/ollama/
chmod +x ~/.local/lib/ollama/ollama
ln -sf ~/.local/lib/ollama/ollama ~/.local/bin/ollama
```

Run before embedding, pointing at the shared model store:

```sh
# Stop Docker Ollama first to free port 11434
docker compose down

OLLAMA_MODELS="$(pwd)/ollama_data/models" ~/.local/bin/ollama serve &
pixi run mitgcm-embed       # or mitgcm-embed-docs, fesom2-embed, fesom2-embed-namelists, …
```

The `ollama_data/` directory is shared between Docker and native Ollama,
so `ollama pull` only ever needs to run once.

## Usage

```python
import ollama

def embed(text: str) -> list[float]:
    return ollama.embed(model="nomic-embed-text", input=text)["embeddings"][0]
```

## Notes

- The server must be running before any embedding calls. Callers should treat
  connection errors as infrastructure failures, not application errors.
- The native binary ships with Metal shaders (`mlx.metallib`, `libmlx.dylib`)
  that must stay alongside the `ollama` executable — do not symlink the binary
  alone without the dylibs.

# Embeddings

## Model

`nomic-embed-text` — a 768-dimensional general-purpose embedding model from
Nomic AI, served via ollama. Used to embed MITgcm subroutine source text so
that natural-language queries can retrieve relevant code.

## Infrastructure

ollama runs as a Docker container (see `compose.yml`). This avoids the
conda-forge CPU-only binary, which crashes on Apple Silicon due to a missing
Metal-capable runner.

```sh
docker compose up -d                                      # start server
docker compose exec ollama ollama pull nomic-embed-text  # first-time setup
```

The server listens on `localhost:11434`. The `ollama` Python client connects
there by default — no configuration needed.

## Usage

```python
import ollama

def embed(text: str) -> list[float]:
    return ollama.embed(model="nomic-embed-text", input=text)["embeddings"][0]
```

## Notes

- The Docker container persists the model in a named volume (`ollama_data`),
  so `ollama pull` only needs to run once.
- The server must be running before any embedding calls. Callers should treat
  connection errors as infrastructure failures, not application errors.

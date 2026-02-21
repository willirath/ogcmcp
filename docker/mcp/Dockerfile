# Stage 1: pull nomic-embed-text into /root/.ollama at build time.
# The model weights (~274 MB) are baked into the image so users need no
# separate download or Ollama installation.
FROM ollama/ollama AS model-builder
RUN ollama serve & sleep 5 && ollama pull nomic-embed-text

# Stage 2: runtime image.
# Copy the Ollama binary and pre-pulled model from Stage 1, then add
# Python 3.13 + runtime dependencies + pre-built indices.
FROM python:3.13-slim

# Ollama binary (statically compiled Go — works on any Linux/glibc)
COPY --from=model-builder /usr/bin/ollama /usr/bin/ollama

# Pre-pulled model weights
COPY --from=model-builder /root/.ollama /root/.ollama

# libgomp is required by onnxruntime (a chromadb transitive dependency)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgomp1 \
    && rm -rf /var/lib/apt/lists/*

# Python runtime dependencies (indexer and test deps excluded)
RUN pip install --no-cache-dir \
    "duckdb>=1.4.4,<2" \
    "chromadb>=1.0" \
    "ollama>=0.6.1,<0.7" \
    "mcp>=1.0,<2" \
    "pint>=0.24,<1" \
    "fastapi>=0.129.0,<0.130"

WORKDIR /app

# Application source
COPY src/ /app/src/

# Pre-built indices (DuckDB + ChromaDB) — must exist before docker build
COPY data/ /app/data/

COPY docker/mcp/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

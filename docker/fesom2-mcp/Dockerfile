# Stage 1: pull nomic-embed-text into /root/.ollama at build time.
# The model weights (~274 MB) are baked into the image so users need no
# separate download or Ollama installation.
FROM ollama/ollama@sha256:0764cf55b4a33bcecca10f718394d097ef7d464b75669a14f0cd4ac1a8b9a0c5 AS model-builder
RUN ollama serve & sleep 5 && ollama pull nomic-embed-text

# Stage 2: runtime image.
# Copy the Ollama binary and pre-pulled model from Stage 1, then add
# Python 3.13 + runtime dependencies + pre-built FESOM2 indices.
FROM python:3.13-slim@sha256:3de9a8d7aedbb7984dc18f2dff178a7850f16c1ae7c34ba9d7ecc23d0755e35f

# Ollama binary (statically compiled Go — works on any Linux/glibc)
COPY --from=model-builder /usr/bin/ollama /usr/bin/ollama

# Pre-pulled model weights — copy to /opt/ollama so a non-root user can read them
COPY --from=model-builder /root/.ollama /opt/ollama

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
    "fastapi>=0.129.0,<0.130" \
    "f90nml>=1.4,<2"

# Non-root user: dedicated UID 1000, no login shell
RUN useradd -u 1000 -m -s /sbin/nologin fesom2

WORKDIR /app

# Application source
COPY src/ /app/src/

# Pre-built FESOM2 indices (DuckDB + ChromaDB) — must exist before docker build
# Build with: pixi run fesom2-index && pixi run fesom2-embed && pixi run fesom2-embed-docs
COPY data/fesom2/ /app/data/fesom2/

COPY docker/fesom2-mcp/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh && \
    chown -R fesom2:fesom2 /app /opt/ollama

# Tell Ollama where the pre-pulled model weights live
ENV OLLAMA_MODELS=/opt/ollama/models

USER fesom2

ENTRYPOINT ["/entrypoint.sh"]

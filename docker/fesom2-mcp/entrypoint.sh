#!/usr/bin/env bash
set -e

# Start Ollama embedding server in the background.
# The model weights are pre-baked into the image (no pull needed).
# Ollama starts in parallel with the MCP server; it is only needed when
# search_code_tool is called, by which time it will be ready.
ollama serve >/dev/null 2>&1 &

# Start the MCP server immediately (stdio transport).
# exec replaces this shell so Docker signals reach the Python process.
exec python3 -m src.fesom2.server

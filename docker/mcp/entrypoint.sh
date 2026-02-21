#!/usr/bin/env bash
set -e

# Start Ollama embedding server in the background.
# The model weights are pre-baked into the image (no pull needed).
ollama serve &

# Give Ollama a moment to become ready before the MCP server starts
# making embed() calls.
sleep 3

# Start the MCP server (stdio transport).
# exec replaces this shell so Docker signals reach the Python process.
exec python3 -m src.server

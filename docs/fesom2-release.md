# FESOM2 release process

How to build, publish, and verify a new `ghcr.io/willirath/ogcmcp` FESOM2
release. Two images are released: the runtime image (FESOM2 binary for
running experiments) and the MCP image (MCP server with pre-built indices).

---

## Prerequisites

- `data/fesom2/` populated — run the pipeline if needed:
  ```bash
  pixi run fesom2-index
  pixi run fesom2-embed
  pixi run fesom2-embed-docs
  pixi run fesom2-embed-namelists
  ```
- Docker Desktop running
- `gh` CLI authenticated (`gh auth status`)
- GHCR credentials: `echo $GITHUB_TOKEN | docker login ghcr.io -u willirath --password-stdin`

---

## Versioning

CalVer: `vYYYY.MM.MICRO` (MICRO resets each month).
FESOM2 images follow the consistent tag scheme:

| Purpose | Tag pattern |
|---|---|
| FESOM2 runtime image | `fesom2-runtime-vYYYY.MM.MICRO` |
| FESOM2 MCP image | `fesom2-mcp-vYYYY.MM.MICRO` |

---

## 1. Build and push the FESOM2 runtime image (multi-arch)

The runtime image (`ghcr.io/willirath/ogcmcp:fesom2-runtime-*`) contains the
FESOM2 binary compiled from the submodule source. Agents and users mount
mesh, input, and output directories at runtime.

```bash
VERSION=v2026.02.6
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t ghcr.io/willirath/ogcmcp:fesom2-runtime-${VERSION} \
  -t ghcr.io/willirath/ogcmcp:fesom2-runtime-latest \
  -f docker/fesom2/Dockerfile \
  --push .
```

Build time: ~50 s (FESOM2 compilation dominates).

---

## 2. Build and push the FESOM2 MCP image (multi-arch)

The MCP image (`ghcr.io/willirath/ogcmcp:fesom2-mcp-*`) bundles Ollama,
the embedding model, Python runtime, and the pre-built FESOM2 indices
from `data/fesom2/`.

```bash
VERSION=v2026.02.6
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t ghcr.io/willirath/ogcmcp:fesom2-mcp-${VERSION} \
  -t ghcr.io/willirath/ogcmcp:fesom2-mcp-latest \
  -f docker/fesom2-mcp/Dockerfile \
  --push .
```

The build bakes in:
- Ollama binary + `nomic-embed-text` model weights (~274 MB)
- Python 3.13 runtime + dependencies
- Pre-built FESOM2 indices from `data/fesom2/` (DuckDB + ChromaDB)

Build time: ~8 min on first run (model download), ~2 min on subsequent runs.

For local development only (single-arch, not pushed):
```bash
pixi run build-fesom2-mcp-image
```

### Set package visibility to public

GitHub → profile → Packages → `ogcmcp` → Package settings →
Change visibility → Public (if not already set from MITgcm MCP release).

---

## 3. Create the GitHub release

```bash
VERSION=v2026.02.6
gh release create ${VERSION} \
  --title "OGCMCP FESOM2 ${VERSION}" \
  --notes "FESOM2 MCP image: \`ghcr.io/willirath/ogcmcp:fesom2-mcp-${VERSION}\`
FESOM2 runtime image: \`ghcr.io/willirath/ogcmcp:fesom2-${VERSION}\`

Install FESOM2 MCP server:
\`\`\`bash
claude mcp add --transport stdio --scope user fesom2 -- \\
  docker run --rm -i ghcr.io/willirath/ogcmcp:fesom2-mcp-${VERSION}
\`\`\`

FESOM2 source: submodule pinned at \`1b58e7f\`."
```

---

## 4. Smoke test

On a clean machine (or after removing the local image):

```bash
docker rmi ghcr.io/willirath/ogcmcp:fesom2-mcp-v2026.02.6 2>/dev/null || true

claude mcp add --transport stdio --scope user fesom2 -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:fesom2-mcp-v2026.02.6
```

Then in a Claude Code session:

```
What controls GM diffusivity in FESOM2?
```

Expected: `namelist_to_code_tool` returns a result referencing `K_GM` in
`namelist.oce`.

---

## 5. Git tag

```bash
VERSION=v2026.02.6
git tag -a ${VERSION} -m "Release ${VERSION}"
git push origin ${VERSION}
```

---

## Image inventory

| Image tag prefix | Purpose | Base | Approx. size |
|---|---|---|---|
| `ghcr.io/willirath/ogcmcp:fesom2-runtime-*` | FESOM2 runtime (experiment execution) | `debian:bookworm-slim` | ~1.4 GB |
| `ghcr.io/willirath/ogcmcp:fesom2-mcp-*` | FESOM2 MCP server (Claude Code integration) | `python:3.13-slim` | ~600 MB |

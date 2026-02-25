# Release process

How to build, publish, and verify a new `ghcr.io/willirath/ogcmcp` release.


---

## Prerequisites

- `data/mitgcm/` populated — run if needed:
  ```bash
  pixi run mitgcm-index
  pixi run mitgcm-embed
  pixi run mitgcm-embed-docs
  pixi run mitgcm-embed-verification
  ```
- Docker Desktop running
- `gh` CLI authenticated (`gh auth status`)
- GHCR credentials: `echo $GITHUB_TOKEN | docker login ghcr.io -u willirath --password-stdin`

---

## Versioning

CalVer: `vYYYY.MM.MICRO` where MICRO is a sequential counter starting at 1,
reset each month. Examples: `v2026.02.1`, `v2026.02.2`.

Roadmap files use `vYYYY.MM` (no MICRO): `plans/roadmap-v2026.03.md`.
The MICRO is assigned when the release is actually cut.

---

## 1. Build and push the runtime image (multi-arch)

The runtime image (`ghcr.io/willirath/ogcmcp:runtime-*`) contains
gfortran + MPICH + NetCDF-Fortran + the MITgcm source tree baked in at
`/MITgcm`. Agents use it as a `FROM` base in their experiment Dockerfiles.

```bash
VERSION=v2026.02.5
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t ghcr.io/willirath/ogcmcp:runtime-${VERSION} \
  -t ghcr.io/willirath/ogcmcp:runtime-latest \
  -f docker/mitgcm/Dockerfile \
  --push .
```

Build time: ~3 min (shallow git clone of MITgcm + apt packages).

---

## 2. Build and push the MCP image (multi-arch)

The MCP image (`ghcr.io/willirath/ogcmcp:mcp-*`) bundles Ollama,
the embedding model, Python runtime, and pre-built indices from `data/`.

```bash
VERSION=v2026.02.5
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t ghcr.io/willirath/ogcmcp:mcp-${VERSION} \
  -t ghcr.io/willirath/ogcmcp:mcp-latest \
  -f docker/mitgcm-mcp/Dockerfile \
  --push .
```

The build bakes in:
- Ollama binary (from `ollama/ollama` Stage 1)
- `nomic-embed-text` model weights (~274 MB, pulled at build time)
- Python 3.13 runtime + dependencies
- Pre-built indices from `data/` (DuckDB ~33 MB + ChromaDB ~145 MB)

Build time: ~8 min on first run (model download), ~2 min on subsequent runs.

For local development only (single-arch, not pushed):

```bash
pixi run build-mcp-image
```

### Set package visibility to public

GitHub → profile → Packages → `ogcmcp` → Package settings →
Change visibility → Public.

This must be done once before the first release; after that the package
stays public across re-pushes.

---

## 3. Create the GitHub release

```bash
VERSION=v2026.02.5
gh release create ${VERSION} \
  --title "OGCMCP ${VERSION}" \
  --notes "MCP image: \`ghcr.io/willirath/ogcmcp:mcp-${VERSION}\`
Runtime image: \`ghcr.io/willirath/ogcmcp:runtime-${VERSION}\`

Install MCP server:
\`\`\`bash
claude mcp add --transport stdio --scope user mitgcm -- \\
  docker run --rm -i ghcr.io/willirath/ogcmcp:mcp-${VERSION}
\`\`\`

MITgcm source: submodule pinned at \`decd05a\` (checkpoint69k)."
```

---

## 4. Smoke test

On a clean machine (or after removing the local image):

```bash
docker rmi ghcr.io/willirath/ogcmcp:mcp-v2026.02.5 2>/dev/null || true

claude mcp add --transport stdio --scope user mitgcm -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:mcp-v2026.02.5
```

Then in a Claude Code session:

```
What namelist parameter controls the non-hydrostatic solver iteration limit?
```

Expected: `namelist_to_code_tool` returns a result referencing `cg3dMaxIters`.

---

## 5. Git tag

```bash
VERSION=v2026.02.5
git tag -a ${VERSION} -m "Release ${VERSION}"
git push origin ${VERSION}
```

---

## GitHub Actions (future)

A `build-and-push.yml` workflow triggered on `push --tags 'v*'` will
replace steps 1–4. It will use `docker/build-push-action` with
`platforms: linux/amd64,linux/arm64` and authenticate via `GITHUB_TOKEN`.
This removes the need for local GHCR credentials and solves the
amd64 QEMU GPG issue.

---

## Image inventory

| Image tag prefix | Purpose | Base | Approx. size |
|---|---|---|---|
| `ghcr.io/willirath/ogcmcp:mcp-*` | User-facing MCP server | `python:3.13-slim` | ~515 MB |
| `ghcr.io/willirath/ogcmcp:runtime-*` | MITgcm build environment for agent Dockerfiles | `debian:bookworm-slim` | ~300 MB |

Pinned digests for current images are in the respective Dockerfiles under `docker/`.

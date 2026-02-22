# Release process

How to build, publish, and verify a new `ghcr.io/willirath/mitgcm-mcp` release.

---

## Prerequisites

- `data/` populated — run `pixi run index`, `pixi run embed`, `pixi run embed-docs` if needed
- Docker Desktop running
- `gh` CLI authenticated (`gh auth status`)
- GHCR credentials: `echo $GITHUB_TOKEN | docker login ghcr.io -u willirath --password-stdin`

---

## Versioning

CalVer: `vYYYY.MM.MICRO` where MICRO is a sequential counter starting at 1,
reset each month. Examples: `v2026.02.1`, `v2026.02.2`.

---

## 1. Build and push the MCP image (multi-arch)

Build for both `linux/amd64` and `linux/arm64` and push directly to GHCR
in one step (multi-arch manifests cannot be loaded into the local daemon):

```bash
VERSION=v2026.02.2
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t ghcr.io/willirath/mitgcm-mcp:${VERSION} \
  -t ghcr.io/willirath/mitgcm-mcp:latest \
  -f docker/mcp/Dockerfile \
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

GitHub → profile → Packages → `mitgcm-mcp` → Package settings →
Change visibility → Public.

This must be done once before the first release; after that the package
stays public across re-pushes.

---

## 2. Package the experiment archive

```bash
pixi run package-rotating-convection
```

Creates `rotating_convection.tar.gz` (~8 MB) containing:

```
rotating_convection/
├── README.md
├── gen.py
├── plot.py
├── T_section.png
├── code/
└── input/
```

The archive does not include `build/` or `run/` (gitignored). Users
unpack it, run `gen.py` if they want to regenerate the input files,
then use `pixi run build-rotating-convection` /
`pixi run run-rotating-convection` against the `mitgcm:latest` image.

The `package-rotating-convection` pixi task runs:

```bash
tar -czf rotating_convection.tar.gz \
  -C experiments \
  --exclude rotating_convection/build \
  --exclude rotating_convection/run \
  rotating_convection
```

---

## 3. Create the GitHub release

```bash
VERSION=v2026.02.2
gh release create ${VERSION} \
  --title "MITgcm MCP ${VERSION}" \
  --notes "MCP image: \`ghcr.io/willirath/mitgcm-mcp:${VERSION}\`

Install:
\`\`\`bash
claude mcp add --transport stdio --scope user mitgcm -- \\
  docker run --rm -i ghcr.io/willirath/mitgcm-mcp:${VERSION}
\`\`\`

MITgcm source: submodule pinned at \`decd05a\` (checkpoint69k)." \
  rotating_convection.tar.gz
```

---

## 4. Smoke test

On a clean machine (or after removing the local image):

```bash
docker rmi ghcr.io/willirath/mitgcm-mcp:v2026.02.2 2>/dev/null || true

claude mcp add --transport stdio --scope user mitgcm -- \
  docker run --rm -i ghcr.io/willirath/mitgcm-mcp:v2026.02.2
```

Then in a Claude Code session:

```
What namelist parameter controls the non-hydrostatic solver iteration limit?
```

Expected: `namelist_to_code_tool` returns a result referencing `cg3dMaxIters`.

---

## 5. Git tag

```bash
VERSION=v2026.02.2
git tag -a ${VERSION} -m "Release ${VERSION}"
git push origin ${VERSION}
```

---

## GitHub Actions (future)

A `build-and-push.yml` workflow triggered on `push --tags 'v*'` will
replace steps 1–3. It will use `docker/build-push-action` with
`platforms: linux/amd64,linux/arm64` and authenticate via `GITHUB_TOKEN`.
This removes the need for local GHCR credentials and solves the
amd64 QEMU GPG issue.

---

## Image inventory

| Image | Purpose | Base | Approx. size |
|---|---|---|---|
| `ghcr.io/willirath/mitgcm-mcp` | User-facing MCP server | `python:3.13-slim` | ~515 MB |
| `mitgcm:latest` | MITgcm build environment | `debian:bookworm-slim` | ~150 MB (expected after Debian+MPICH rebuild) |

Pinned digests for current images are in the respective Dockerfiles under `docker/`.

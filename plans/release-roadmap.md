# Release roadmap — v2026.02.1

Actionable checklist for the first release. Detailed rationale and decisions
in `release-architecture.md` and `release-tasks.md`.

---

## R1 — Experiment directory

Consolidate the rotating_convection experiment into a self-contained,
documented unit.

- [x] Move `scripts/gen-rotating-convection.py` → `experiments/rotating_convection/gen.py`
- [x] Add `experiments/rotating_convection/plot.py` (coordinate-fixed cross-section script)
- [x] Generate and commit `experiments/rotating_convection/T_section.png`
- [x] Write `experiments/rotating_convection/README.md`
- [x] Update `pixi.toml` `gen-rotating-convection` task path

**Done when:** `experiments/rotating_convection/` contains README, gen.py,
plot.py, T_section.png, and all existing input/code files; plot.py produces
the PNG from a completed run.

---

## R2 — Docs and consistency

Bring all docs up to date with M7/M8 reality.

- [x] Update `README.md` (milestone table, layout table, install section)
- [x] Update `docs/domain-knowledge.md` (M7 gotchas, check_scales fix)
- [x] Update `docs/mcp-server.md` (search_docs_tool, M5 domain tools)
- [x] Update `docs/tools.md` (search_docs)
- [x] Update `docs/runtime.md` (rotating_convection, docker/ tree)
- [x] Review all MCP tool docstrings in `src/server.py`

**Done when:** `pixi run test` passes and a fresh reader of README.md
understands what the project does and how to install it.

---

## R3 — Docker restructuring

Reorganise the Docker setup and produce the self-contained MCP image.

- [x] Move `Dockerfile` → `docker/mitgcm/Dockerfile`
- [x] Create `docker/mcp/Dockerfile` (two-stage: pull model, assemble image)
- [x] Create `docker/mcp/entrypoint.sh`
- [x] Create `docker/mcp/.dockerignore`
- [x] Update `compose.yml` — named network `mitgcm-net`, dev-only comments
- [x] Update `pixi.toml` — fix `build-image` path, add `build-mcp-image` task
- [x] Update `.mcp.json` — `docker run --rm -i ghcr.io/willirath/mitgcm-mcp:v2026.02.1`
- [x] Pin all base images to sha256 digests
- [x] Switch `docker/mitgcm/Dockerfile` to `debian:bookworm-slim` + MPICH
- [x] Rebuild `mitgcm:latest` and `mitgcm-mcp:latest` after Debian+MPICH+pinning changes

**Done when:** `pixi run build-mcp-image` succeeds with populated `data/`;
`docker run --rm -i mitgcm-mcp:latest` starts without error.

---

## R4 — Release

Tag, publish, and verify.

- [x] Add `package-rotating-convection` task to `pixi.toml`
- [x] Write `docs/release.md` — release process documentation
- [x] Rebuild `mitgcm:latest` with Debian+MPICH; verify full rotating_convection build+run
- [x] Rebuild `mitgcm-mcp:latest`; smoke test `docker run --rm -i`
- [x] `docker login ghcr.io` with PAT (packages:write)
- [x] Push MCP image to GHCR (`v2026.02.1` + `latest`)
- [ ] Set GHCR package visibility to public
- [x] Package `rotating_convection.tar.gz` (`pixi run package-rotating-convection`)
- [ ] Create GitHub release `v2026.02.1` with tar.gz asset and image install note
- [ ] Smoke test on clean machine: install via `claude mcp add`/`codex mcp add`, verify tool responses
- [ ] Git tag `v2026.02.1` and push

**Done when:** A user on a clean machine can install the MCP server with one
command and get responses from all tools.

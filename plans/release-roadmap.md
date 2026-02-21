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

- [ ] Move `Dockerfile` → `docker/mitgcm/Dockerfile`
- [ ] Create `docker/mcp/Dockerfile` (two-stage: pull model, assemble image)
- [ ] Create `docker/mcp/entrypoint.sh`
- [ ] Create `docker/mcp/.dockerignore`
- [ ] Update `compose.yml` — named network `mitgcm-net`, dev-only comments
- [ ] Update `pixi.toml` — fix `build-image` path, add `build-mcp-image` task
- [ ] Update `.mcp.json` — `docker run --rm -i ghcr.io/willirath/mitgcm-mcp:v2026.02.1`

**Done when:** `pixi run build-mcp-image` succeeds with populated `data/`;
`docker run --rm -i mitgcm-mcp:latest` starts without error.

---

## R4 — Release

Tag, publish, and verify.

- [ ] Build MCP image with current `data/`
- [ ] Push image to GHCR (`v2026.02.1` + `latest`)
- [ ] Package `rotating_convection.tar.gz` release asset
- [ ] Create GitHub release `v2026.02.1` with both assets
- [ ] Set GHCR package visibility to public
- [ ] Smoke test: `claude mcp add --transport stdio --scope user mitgcm -- docker run --rm -i ghcr.io/willirath/mitgcm-mcp:v2026.02.1`
- [ ] Git tag `v2026.02.1`

**Done when:** A user on a clean machine can install the MCP server with one
command and get responses from all tools.

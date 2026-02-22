# Release tasks — v0.1

Checklist of concrete work to do before calling this a first release.
Open feedback sections marked `> **USER:**` — fill in inline.

Architecture decisions are settled in `plans/release-architecture.md`.

---

## Docs and consistency

### README.md update

The existing README.md is stale: M5/M6 show as incomplete, M7/M8 are absent,
`embed-docs` and `docs_indexer` are missing from the layout table, the docs
table is incomplete. Needs a pass to reflect the current state.

> **USER:**
>
> (Any additions to the README beyond updating the milestone table and layout?
> Should the README address the academic motivation / evaluation angle? Or keep
> it purely operational?)

./.

---

### docs/ consistency review

Each component has a doc in `docs/`. Some may be stale or missing after M7/M8.
Known gaps:

- `docs/domain-knowledge.md` — check it reflects the M7 additions (check_scales
  negative-Omega fix, new gotchas)
- `docs/mcp-server.md` — check tool list includes `search_docs_tool` and the
  M5 domain tools
- `docs/tools.md` — check `search_docs` is documented
- `docs/runtime.md` — covers Docker build/run but may not reflect the M7
  rotating_convection experiment; also needs updating for the docker/ tree
  reorganisation

> **USER:**
>
> (Any docs you know are obviously wrong or missing beyond the above? Should
> the diagrams in `docs/diagrams.md` be updated to include the docs index
> pipeline?)

./.

---

### Tool docstrings

The MCP tool descriptions are what the LLM reads when deciding which tool to
call. Worth a pass to check they are:
- Accurate (no stale parameter names or return shapes)
- Consistent in style and level of detail
- Include the "when to use this" guidance that helps the LLM choose correctly

The `namelist_to_code_tool` already has a note about declaration vs use sites
(added after the M7 INI_PARMS observation). The others should be checked for
similar real-world caveats.

> **USER:**
>
> (Any specific tools where the docstring felt misleading or incomplete during
> actual use? Any style preferences — terse vs verbose?)

./.

---

## Experiment documentation and scripts

One experiment README is sufficient for v0.1: `experiments/rotating_convection/`.

The experiment is already self-contained for build and run — binary input files
(`bathy.bin`, `init_T.bin`, `rbcs_T.bin`, `rbcs_mask.bin`) are committed
alongside the namelists. Users clone the repo, build, and run without needing
to regenerate anything. The gen script is there for transparency and
modification.

### Files to add or move

- **Move** `scripts/gen-rotating-convection.py` →
  `experiments/rotating_convection/gen.py` (co-locate with what it generates)
- **Add** `experiments/rotating_convection/plot.py` — coordinate-fixed
  cross-section script from the conversation (subtract 0.5 from x/y to centre
  on tank)
- **Add** `experiments/rotating_convection/T_section.png` — committed output
  from a completed run (referenced in README)
- **Update** `pixi.toml` — `gen-rotating-convection` task path

### README content

`experiments/rotating_convection/README.md` should cover:

- The original user prompt that motivated this experiment (verbatim or
  paraphrased — captures the scientific intent)
- Physical setup: rotating convection in a 1 m tank, parameter summary
- Scale analysis summary (Ro, Ek, Bu, δ from check_scales, key flags raised)
- Parameter choices with brief rationale (deltaT, grid, RBCS, tempAdvScheme)
- Gotchas encountered: RBCS mask, DIAGNOSTICS_SIZE, vertical CFL instability
- How to regenerate input files: `python experiments/rotating_convection/gen.py`
- How to build and run: `pixi run build-rotating-convection` /
  `pixi run run-rotating-convection`
- Cross-section plot: `python experiments/rotating_convection/plot.py`,
  with embedded `T_section.png`

### Standalone experiment archive

Package `experiments/rotating_convection/` (input/, code/, gen.py, plot.py,
README.md, T_section.png — no build/ or run/) as a GitHub release asset:
`rotating_convection.tar.gz` (~8 MB).

Include `scripts/build-experiment.sh` and `scripts/run-experiment.sh` in the
archive so users have everything needed without cloning the repo. The
experiment README documents the raw docker commands as well for users who
prefer not to use the scripts.

Users download, unpack, and run — no repo clone required.

---

## Docker packaging

Architecture is settled: self-contained image with Ollama, model, and indices
baked in. See `plans/release-architecture.md`.

- [ ] Move `Dockerfile` → `docker/mitgcm/Dockerfile`
- [ ] Create `docker/mcp/Dockerfile` (two-stage: pull model, then assemble image)
- [ ] Create `docker/mcp/entrypoint.sh` (start `ollama serve`, then `python -m src.server`)
- [ ] Create `docker/mcp/.dockerignore` (exclude `MITgcm/`, `.git/`, `tests/`, etc.)
- [ ] Update `compose.yml` — add named network `mitgcm-net`; mark as dev-only in comments
- [ ] Update `pixi.toml` — fix `build-image` path; add `build-mcp-image` task
- [ ] Update `.mcp.json` — `docker run --rm -i ghcr.io/willirath/mitgcm-mcp:latest`
- [ ] Build image locally (`pixi run build-mcp-image`) with populated `data/`
- [ ] Publish image to GHCR; set package visibility to public
- [ ] Smoke test: `claude mcp add --transport stdio --scope user mitgcm -- docker run --rm -i ghcr.io/willirath/mitgcm-mcp:latest`, verify tools respond

---

## Backlog triage

| Item | Disposition |
|---|---|
| Gotchas → JSON | Post-v0.1 |
| Experiment README | **Include** |
| Forcing file gen as tool | Post-v0.1 — rotating_convection gen.py is the worked example |
| Visualisation script | **Include** — in experiment dir |
| Tool use enforcement | Done — CLAUDE.md updated |
| CPP guard line attribution | Post-v0.1 — known limitation, documented |
| GPU Ollama | Post-v0.1 — performance only |
| INI_PARMS use vs declaration | Post-v0.1 — caveat already in tool docstring |

---

## Release checklist (summary)

- [ ] All tests pass (`pixi run test`)
- [ ] README.md reflects current state
- [ ] All docs/ files consistent with implementation
- [ ] Tool docstrings reviewed
- [ ] `scripts/gen-rotating-convection.py` moved to `experiments/rotating_convection/gen.py`
- [ ] `experiments/rotating_convection/plot.py` added
- [ ] `experiments/rotating_convection/T_section.png` committed
- [ ] `experiments/rotating_convection/README.md` written
- [ ] `docker/mitgcm/Dockerfile` (moved from root)
- [ ] `docker/mcp/Dockerfile` + `entrypoint.sh` + `.dockerignore`
- [ ] `compose.yml` updated (named network, dev-only)
- [ ] `pixi.toml` updated (paths + `build-mcp-image` task)
- [ ] `.mcp.json` updated to `docker run` one-liner
- [ ] Image built, published to GHCR, set public
- [ ] `rotating_convection.tar.gz` release asset built and uploaded
- [ ] Smoke test passes
- [ ] Git tag `v2026.02.1`

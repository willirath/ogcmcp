# Release tasks — v0.1

Checklist of concrete work to do before calling this a first release.
Feedback sections marked `> **MW:**` — fill in inline.

The architecture decisions in `plans/release-architecture.md` should be
settled before starting the packaging items here.

---

## Docs and consistency

### README.md update

The existing README.md is stale: M5/M6 show as incomplete, M7/M8 are absent,
`embed-docs` and `docs_indexer` are missing from the layout table, the docs
table is incomplete. Needs a pass to reflect the current state.

> **MW:**
>
> (Any additions to the README beyond updating the milestone table and layout?
> Should the README address the academic motivation / evaluation angle? Or keep
> it purely operational?)

---

### docs/ consistency review

Each component has a doc in `docs/`. Some may be stale or missing after M7/M8.
Known gaps:

- `docs/domain-knowledge.md` — check it reflects the M7 additions (check_scales
  negative-Omega fix, new gotchas)
- `docs/mcp-server.md` — check tool list includes `search_docs_tool` and the
  M5 domain tools
- `docs/tools.md` — check `search_docs` is documented
- No doc exists yet for the runtime (`docs/runtime.md` covers Docker build/run
  but may not reflect the M7 rotating_convection experiment)

> **MW:**
>
> (Any docs you know are obviously wrong or missing beyond the above? Should
> the diagrams in `docs/diagrams.md` be updated to include the docs index
> pipeline?)

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

> **MW:**
>
> (Any specific tools where the docstring felt misleading or incomplete during
> actual use? Any style preferences — terse vs verbose?)

---

## Experiment documentation

The backlog item "experiment README" is a pre-release task. The
`rotating_convection` experiment has no documentation beyond the namelist
comments and the gen script docstring. A `experiments/rotating_convection/README.md`
should cover:

- Scientific motivation and physical setup
- Scale analysis summary (key numbers from check_scales)
- Grid and parameter choices with brief justification
- Gotchas encountered during setup (RBCS mask, DIAGNOSTICS_SIZE, vertical CFL)
- How to regenerate, build, and run

> **MW:**
>
> (Is one experiment README sufficient for v0.1, or should the tutorial
> experiment also get one? Any specific sections you'd want included beyond
> the above?)

---

## Visualisation script

The fixed cross-section plotting script (coordinate-corrected version) exists
only in the conversation. The original in-conversation script had a coordinate
bug. The fixed version should live somewhere in the repo — either as a
`scripts/plot-experiment.py` or as a pixi task.

> **MW:**
>
> (Should this be a general `plot-experiment.py` that works for any experiment,
> or a specific `plot-rotating-convection.py`? Should it be a pixi task?)

---

## Packaging (depends on release-architecture.md decisions)

These items cannot be started until the architecture questions are settled.

- [ ] Add `pyproject.toml` (package metadata, entry points, dependencies)
- [ ] Add `mitgcm-mcp download` CLI command (fetch pre-built indices)
- [ ] Decide Ollama vs sentence-transformers; implement if switching
- [ ] Pin MITgcm checkpoint in package metadata
- [ ] Update `.mcp.json` to use packaged entry point
- [ ] Upload pre-built indices to chosen host
- [ ] Smoke test: `pip install .` + `mitgcm-mcp download` + `mitgcm-mcp serve`
  in a clean environment

> **MW:**
>
> (Any packaging constraints not covered in release-architecture.md? Is there
> a target Python version range? Any concern about the chromadb or duckdb
> dependency footprint?)

---

## Backlog triage

Current backlog has 8 items. Proposed disposition for v0.1:

| Item | Disposition |
|---|---|
| Gotchas → JSON | Post-v0.1 |
| Experiment README | **Include** |
| Forcing file gen as tool | Post-v0.1 — out of scope per architecture discussion |
| Visualisation script | **Include** (simple fix) |
| Tool use enforcement | Done — CLAUDE.md updated |
| CPP guard line attribution | Post-v0.1 — known limitation, documented |
| GPU Ollama | Post-v0.1 — performance only |
| INI_PARMS use vs declaration | Post-v0.1 — caveat already in tool docstring |

> **MW:**
>
> (Any items you'd move from post-v0.1 to included? Anything in the backlog
> that feels blocking?)

---

## Release checklist (summary)

Once the above is done and architecture decisions are made:

- [ ] All tests pass (`pixi run test`)
- [ ] README.md reflects current state
- [ ] All docs/ files consistent with implementation
- [ ] Tool docstrings reviewed
- [ ] `experiments/rotating_convection/README.md` written
- [ ] Visualisation script committed
- [ ] `pyproject.toml` added
- [ ] Pre-built indices uploaded and `download` command works
- [ ] `.mcp.json` updated
- [ ] Clean-environment smoke test passes
- [ ] Git tag `v0.1.0`

# Backlog

---

## Codex CLI support

**Where**: `.mcp.json`, `README.md`, `docs/release.md`

**Problem**: `codex mcp add mitgcm -- docker run --rm -i ghcr.io/willirath/mitgcm-mcp:v2026.02.1`
causes "Transport closed" on every tool call. Claude Code works fine with the
same image and command. Root cause not yet diagnosed — possibly a difference in
how Codex CLI handles stdio subprocess lifecycle or connection timeouts.

**Fix**: Reproduce the failure, capture Codex stderr, and identify whether the
issue is in Codex's MCP client, the entrypoint, or an incompatibility in the
fastmcp stdio transport. Once fixed, add `codex mcp add` install instructions
back to README and release docs.

Known limitations and deferred improvements, in no particular priority order.

---

## Gotcha catalogue: expose as JSON

**Where**: `src/domain/gotcha.py`

**Problem**: The catalogue is a Python list-of-dicts, useful for `lookup_gotcha`
but awkward to query from outside Python (e.g. from the MCP server, a web UI,
or a shell script).

**Fix**: Serialise the catalogue to `data/gotchas.json` at build time (or
lazily on first call). The MCP server could expose a `list_gotchas` tool
returning the full catalogue, and `lookup_gotcha` could operate on the JSON
representation. Same pattern could apply to other static knowledge structures.

---

## Experiment readme

**Where**: Each `experiments/<name>/` directory

**Problem**: There is no record of why an experiment was set up — the user
request, the scale analysis, the design choices, or the gotchas encountered
during configuration.

**Fix**: Write a `README.md` inside each experiment directory when the
experiment is created. Should cover: scientific motivation, parameter choices
and their justification, scale analysis summary, known issues or limitations.
Could be generated (or at least scaffolded) by the system from the conversation
that produced the experiment.

---

## Forcing file generation as a tool

**Where**: `scripts/gen-*.py`, `src/tools.py`

**Problem**: Binary forcing file generation (bathymetry, initial conditions,
RBCS masks) is currently done by standalone scripts that are not accessible
via the MCP interface.

**Fix**: Wrap forcing file generation as a callable tool (or family of tools)
exposed through the MCP server. Would allow an LLM to request "generate
forcing files for this configuration" without needing to know the script path.
Needs a well-defined interface for passing grid and physics parameters.

---

## Standard visualisation for experiment output

**Where**: `scripts/` or a new `src/viz/` module

**Problem**: Plotting experiment output (e.g. temperature cross-sections) is
done ad hoc in a Python one-off. There is no standard way to produce a
diagnostic plot for a completed experiment.

**Fix**: A `plot_experiment.py` script (or pixi task) that reads MNC output
and produces a standard set of plots: surface map, radius–depth cross-section,
time series of domain-mean temperature. Output goes to `experiments/<name>/fig/`.

---

## Tool use vs. direct file reads

**Where**: System prompt / CLAUDE.md

**Problem**: Claude sometimes reads MITgcm source directly via Bash (`sed`,
`cat`) rather than through the MCP tools (`get_source_tool`,
`get_subroutine_tool`). The MCP tools provide structured context and respect
the indexed representation; direct reads bypass them and can silently mislead.

**Example**:
```
Bash(sed -n '80,130p' MITgcm/pkg/rbcs/rbcs_add_tendency.F)
```
should be replaced by `get_source_tool("rbcs_add_tendency")`.

**Fix**: Strengthen CLAUDE.md instructions to prohibit direct MITgcm source
reads when an MCP tool exists for the same purpose. Could also add a hook that
warns when `MITgcm/` appears in a Bash command.

---

## CPP guard attribution is per-subroutine, not per-line

**Where**: `src/indexer/extract.py`, `cpp_guards` table

**Problem**: The extractor attributes every `#ifdef` / `#ifndef` found anywhere
in a subroutine to the subroutine as a whole. For large routines like
`INI_PARMS` (~1500 lines), this produces many unrelated flags —
`SHORTWAVE_HEATING`, `ALLOW_EXCH2`, etc. — none of which guard the specific
line being queried.

**Observed via**: `get_cpp_requirements_tool("INI_PARMS")` after asking which
CPP flag guards the reading of `cg3dMaxIters`. The flags returned are
technically present in the subroutine but irrelevant to that parameter.

**Fix**: Track the active `#ifdef` stack at each line and store
`(subroutine_id, cpp_flag, line_start, line_end)` rather than just
`(subroutine_id, cpp_flag)`. `get_cpp_requirements` could then accept an
optional line range to filter to guards that actually wrap the target code.
This is a meaningful scope increase — the regex extractor would need to
maintain a CPP nesting stack across lines.

---

## Embedding pipeline: GPU Ollama for larger context and faster indexing

**Where**: `src/embedder/pipeline.py`, `compose.yml`

**Problem**: The CPU Docker Ollama container has an effective context window
that is too small for some dense free-form Fortran chunks (~4000 chars).
At least one chunk (`diffusivity_free` chunk 0, `atm_phys`) is skipped with
a "context length exceeded" warning on each run. Indexing 4910 chunks takes
~45 minutes on CPU.

**Opportunity**: The embedding pipeline only needs to run once (or on MITgcm
updates). Running it against a GPU-backed Ollama instance (V100/H100) would:
- Reduce indexing time from ~45 min to minutes
- Allow a larger `num_ctx` (e.g. 8192) to eliminate the skipped chunks
- Enable larger, higher-quality embedding models (e.g. `mxbai-embed-large`)

The MCP server itself requires only a single embedding call per `search_code`
query and runs fine on a laptop. The natural split is: build `data/chroma/`
on a GPU host, ship it to the local machine, query locally.

**Wiring change needed**: make the Ollama base URL configurable (currently
hardcoded to localhost via the `ollama` client default). One-liner in
`pipeline.py` and `tools.py`.

---

## INI_PARMS conflates "reads from namelist" with "uses the parameter"

**Where**: `namelist_refs` table, `namelist_to_code` tool

**Problem**: `INI_PARMS` reads every namelist parameter in MITgcm into COMMON
blocks. A query for `cg3dMaxIters` correctly returns `INI_PARMS` (it appears
in a `&PARM02` block there), but the semantically useful answer is often
`CG3D` — the subroutine that *uses* the parameter.

**Fix**: No schema change needed. Callers should be aware that
`namelist_to_code` finds declaration sites, not use sites. A follow-on
`get_callers` / semantic search step is needed to find the subroutine that
acts on the value. Could also add a `uses_variable` tool that searches
`source_text` for bare references to a variable name (imprecise but useful).

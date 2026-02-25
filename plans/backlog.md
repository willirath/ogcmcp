# Backlog

Known limitations and deferred improvements, in no particular priority order.

---

## Inbox

- **Namelist read/write tool**: Robust Python namelist parsers for MITgcm exist (e.g. `f90nml`) and have been used with MITgcm before. Wrap one as a tool: read a namelist string → structured dict, and write dict → properly-formatted Fortran namelist. Would catch silent formatting errors agents currently make.

- **Expose all MITgcm files via tools**: In every test session so far, agents eventually bypassed the MCP tools and parsed MITgcm sources directly from the Docker image because key files (SIZE.h, CPP_OPTIONS.h, COMMON block headers) are not in the index. The code graph only covers subroutine-containing `.F` files. Fix: extend indexer to cover `.h` files.

- **Agent smoke-test / incremental validation workflow**: How should agents incrementally validate an experiment while designing it? Need a suggested strategy: e.g. start with minimal domain, check model starts, then scale up. Goes in `get_workflow_tool` or a dedicated gotcha entry. Distinct from release smoke tests.

- **Tutorial-first workflow**: Extend `get_workflow_tool` with a `design_experiment` workflow that starts from "find the closest verification experiment to your goal, study its namelist choices, then diverge." Pairs naturally with the verification experiment index already in place.

---

## Search quality fixes

**Where**: `src/mitgcm/server.py`, `src/mitgcm/domain/gotcha.py`

**Source**: test session 2026-02-23

- **`lookup_gotcha_tool` deduplication**: The "rigid lid vs free surface" entry was returned for
  three different queries in one session. Add deduplication or result diversity logic so the same
  entry is not returned repeatedly for different topics.

- **`namelist_to_code_tool` for internal variables**: `namelist_to_code_tool("tauX")` returns
  empty because `tauX` is an internal COMMON block variable, not a namelist parameter. The tool
  should return a hint redirecting to the corresponding namelist parameter (`zonalWindFile`).

- **Wind stress search buried by EXF docs**: `search_docs_tool` queries for wind stress return
  EXF/bulk-formula package docs, burying the simpler `PARM05 zonalWindFile` approach for
  non-EXF runs. Consider boosting or separately tagging the basic forcing section.

---

## Gotcha catalogue: expose as JSON

**Where**: `src/mitgcm/domain/gotcha.py`

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

## CPP guard attribution is per-subroutine, not per-line

**Where**: `src/mitgcm/indexer/extract.py`, `cpp_guards` table

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

**Where**: `src/mitgcm/embedder/pipeline.py`, `compose.yml`

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

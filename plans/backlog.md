# Backlog

Known limitations and deferred improvements, in no particular priority order.

---

---

## Tool listing in README

**Where**: `README.md`

**Problem**: The README installs the server but gives little signal about what
it actually does before the reader commits to the install. A user skimming
GitHub has no fast way to judge whether the tools match their workflow.

**Fix**: Add a concise table of all MCP tools (name, one-line description,
example query) to the README, placed before the install section. Keep it in
sync with docstrings in `src/tools.py` — could be auto-generated at release
time from the tool registry to avoid drift.

---

## README as entry point for a new user

**Where**: `README.md`

**Problem**: The README is written for someone who has already decided to
install. It does not answer the prior question: "what kind of problems does
this help with, and would it help *me*?" A domain scientist unfamiliar with
MCP or LLM tooling may not recognise the value proposition from the current
text.

**Fix**: Open with a concrete before/after: what question did an agent fail to
answer without the tools, and what did it produce with them? One worked example
of a full agent session (lab parameters → namelist → scale check → gotcha
warning) makes the value tangible. The example already exists in the README;
it may just need to be surfaced earlier and framed around the user's problem
rather than the tool's features.

---

## Agent entry point: tool discoverability and suggested workflow

**Where**: `src/server.py`, tool docstrings

**Problem**: When an agent first sees the tool list, it gets names and
docstrings but no suggested workflow or ordering. An agent asked to design a
rotating-tank experiment may reach for `search_code_tool` (wrong first step)
rather than `translate_lab_params_tool` → `check_scales_tool` →
`suggest_experiment_config_tool`. The tools are individually documented but the
*workflow* is implicit.

**Fix**: Two options, not mutually exclusive:
- Add a `get_workflow` tool (or `describe_capabilities`) that returns a
  short natural-language description of the intended usage flow for common
  tasks ("designing a new experiment", "debugging a configuration", "understanding
  a package"). Zero implementation cost; high value for cold-start sessions.
- Strengthen individual docstrings with "call this after X" / "combine with Y"
  cross-references so the workflow emerges from the tool descriptions alone.

---

## Index verification `.h` files for tool-driven discovery

**Where**: `src/docs_indexer/pipeline.py`, `src/docs_indexer/parse.py`

**Problem**: `SIZE.h`, `DIAGNOSTICS_SIZE.h`, `CPP_OPTIONS.h`, and other
headers from `MITgcm/verification/*/code/` are not indexed anywhere. The
subroutine index only covers `.F`/`.F90`; the docs index only covers `.rst`.
An agent asked to write a `SIZE.h` must rely on memory, which is the most
common source of decomposition errors (wrong `OLx`, confused `nSx` vs `nPx`,
etc.).

The docs index already surfaces SIZE.h *prose* from tutorial RSTs, but only
the customised lines, not complete files.

**Fix**: In `docs_indexer/pipeline.py`, add a second walk over
`MITgcm/verification/*/code/*.h`. Treat each file as a document with metadata
`{file, experiment, section=filename}` and feed it through the same
`_doc_chunks` → `collection.upsert` path already used for RST sections. No
new collection, no new tool, no schema changes. After reindexing,
`search_docs_tool("SIZE.h rotating tank")` returns the actual compilable
header from a known-working experiment rather than prose describing it.

This is the right pattern for the project's scope: the tools help agents
*explore MITgcm resources*, and verification headers are a primary resource.
Generating `SIZE.h` from a tool would narrow creative space; surfacing real
examples lets the agent adapt them.

---

## Quickstart in suggest_experiment_config_tool: Docker recipe without a fixed template

**Where**: `src/domain/suggest.py`, `docker/mitgcm/Dockerfile`

**Problem**: Two related issues:

1. The `suggest_experiment_config_tool` output currently has no "how to build
   and run" section. An agent that produces correct namelists and CPP options
   still cannot complete the task without knowing the build recipe.
2. Any recipe that references `rotating_convection.tar.gz` or another
   concrete template anchors the agent to that setup, narrowing creative space.

**Fix — two parts**:

*Part A (Dockerfile)*: Bake a pinned MITgcm source checkout into
`docker/mitgcm/Dockerfile` (shallow clone + `rm -rf .git`). The image grows
~300–400 MB but users no longer need `git clone MITgcm`. The pinned checkout
also ensures reproducibility: the experiment was designed against the same
source that will compile it.

*Part B (tool output)*: Add a `quickstart` key to the dict returned by
`suggest_experiment_config_tool`. Content should describe the *generic
directory contract* — `code/` files come from the tool, `input/` files come
from `translate_lab_params_tool`, binary fields come from agent-written Python
— followed by the two Docker commands (build, run). No reference to any
specific experiment or tarball. An agent reading this understands the
file-layout contract and the exact Docker invocation without being steered
toward any particular physical setup.

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

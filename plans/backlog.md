# Backlog

Known limitations and deferred improvements, in no particular priority order.

---

## Inbox

- **Namelist read/write tool**: Robust Python namelist parsers for MITgcm exist (e.g. `f90nml`) and have been used with MITgcm before. Wrap one as a tool: read a namelist string → structured dict, and write dict → properly-formatted Fortran namelist. Would catch silent formatting errors agents currently make.

- **Namelist structure map**: Agents and new users don't know what lives where. Add a `get_namelist_structure_tool` (or gotcha entry) with a two-level map: (1) file → group (e.g. `input/data` contains PARM01–05, `input/eedata` contains EEPARMS, each package gets `input/data.<pkgname>`); (2) group → domain for the opaque numbered groups (PARM01 = basic numerics, PARM02 = elliptic solvers, PARM03 = time stepping, PARM04 = grid, PARM05 = I/O; EEPARMS = parallel tile layout). Package-specific groups can defer to `get_package_flags_tool` and `search_docs_tool`. Also cover the `data.pkg` dependency: packages must be enabled there before their `data.<pkg>` file has any effect.

- **Expose all MITgcm files via tools**: In every test session so far, agents eventually bypassed the MCP tools and parsed MITgcm sources directly from the Docker image because key files (SIZE.h, CPP_OPTIONS.h, COMMON block headers) are not in the index. The code graph only covers subroutine-containing `.F` files. Fix: extend indexer to cover `.h` files.

- **Verification experiment index**: 68 experiments under `MITgcm/verification/`, consistent structure (`input/data*`, `input/eedata`, `code/*.h`, `code/packages.conf`). Typical `input/data` is ~70 LOC — small enough to embed whole. A searchable index of verification namelist files would let agents answer "how does tutorial_rotating_tank configure PARM01?" directly. Likely bigger scope than the `.h` header work above — assess separately.

- **Docker arch strategy — be explicit**: Docker workflow has caused struggles in N=2 sessions (100%). Root cause identified from session logs (see `plans/test-sessions/`). Concrete bugs in the `suggest_experiment_config_tool` Dockerfile template — see "Dockerfile template fixes" entry below.

- **Agent smoke-test / incremental validation workflow**: How should agents incrementally validate an experiment while designing it? Need a suggested strategy: e.g. start with minimal domain, check model starts, then scale up. Goes in `get_workflow_tool` or a dedicated gotcha entry. Distinct from release smoke tests.

- **Tutorial-first workflow**: Drop the idea of a canned call-graph overview. Instead, extend `get_workflow_tool` with a `design_experiment` workflow that starts from "find the closest verification experiment to your goal, study its namelist choices, then diverge." Pairs naturally with the verification experiment index above — together they make the starting point much more grounded.

---

## Dockerfile template fixes (suggest_experiment_config_tool)

**Where**: `src/domain/suggest.py`, `_DOCKERFILE_TEMPLATE` and `_QUICKSTART`

**Source**: Both test sessions (2026-02-23). Every user hits at least two of these on first attempt.

**Bugs**, in order of encounter:

1. **`dpkg-architecture` not in runtime image** — Build fails immediately with `command not found`.
   Fix: switch to `uname -m` (returns `x86_64` / `aarch64`), update `case` branches accordingly.

2. **`--allow-run-as-root` is OpenMPI syntax** — Run fails immediately; image uses MPICH (hydra),
   which does not recognise this flag. Fix: remove the flag entirely.

3. **`-mpi` flag missing from `genmake2` call** — Without `-mpi`, genmake2 expands a different
   `comm_stats` template where the `SIZE.h → EXCH.h` PARAMETER chain is broken, causing
   `L_WBUFFER` compile errors even for NP=1 runs. Fix: always include `-mpi`.

4. **`mpif77` wrapper breaks `fcVers` detection** — The arm64 optfile conditionally adds
   `-fallow-argument-mismatch` based on gfortran version, detected by parsing `$FC --version`.
   When `FC=mpif77`, the wrapper does not emit a parseable version string → `fcVers=0` → flag
   omitted → gfortran 12 strict type-checking causes MPI argument mismatch compile errors.
   Fix: inject `-fallow-argument-mismatch` via a `genmake_local` file before calling `genmake2`,
   or document the workaround prominently in the template comments.

---

## Gotcha additions from test sessions

**Where**: `src/domain/gotcha.py`

**Source**: `plans/test-sessions/2026-02-23-claude-lake-triangle.md` §4–5

### Critical (silent wrong results)

- **`readBinaryPrec` mismatch**: Python/NumPy writes 64-bit floats by default; MITgcm reads
  binary input as 32-bit by default (`readBinaryPrec=32`). The mismatch causes silently wrong
  results — model runs to `STOP NORMAL END` with no warning. Symptoms: `n3dWetPts == n2dWetPts`
  (only surface layer wet), `theta_mean = tRef(1)`, no circulation.
  Diagnostic: compare expected wet-cell count from `gendata.py` against `n2dWetPts` in
  `STDOUT.0000`. Fix: add `readBinaryPrec = 64` to `&PARM01`, or write files with `dtype='>f4'`.

### High (runtime aborts requiring full rebuild)

- **`INCLUDE_PHIHYD_CALCULATION_CODE` required**: Hydrostatic runs require
  `#define INCLUDE_PHIHYD_CALCULATION_CODE` in `CPP_OPTIONS.h`. Without it, `CONFIG_CHECK`
  aborts with "missing code to calculate pressure (totPhiHyd)". Not included by default.

- **`gfd` required in `packages.conf`**: Standard ocean/lake runs need the `gfd` package group
  (`mom_fluxform`, `mom_common`, `mom_vecinv`, `generic_advdiff`, `debug`, `mdsio`, `rw`,
  `monitor`). Listing only `mom_fluxform` is insufficient. `PACKAGES_CHECK` aborts at runtime.
  Rebuild required after fixing.

### Medium (compile errors)

- **`SIZE.h` must include `MAX_OLX`/`MAX_OLY` PARAMETER block**: Custom `SIZE.h` overrides
  must reproduce the `MAX_OLX`, `MAX_OLY` PARAMETER declarations at the bottom of the standard
  file. `EXCH.h` depends on these via `MAX_OLX_EXCH = MAX_OLX`. Without them, `L_WBUFFER` is
  not a PARAMETER and cannot be used as an array dimension. Verification: compare against
  `tutorial_barotropic_gyre/code/SIZE.h`.

- **`-mpi` required even for NP=1**: See Dockerfile template fixes §3 above. Also applies
  when users write their own `genmake2` invocation.

### Search quality fixes

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

## Verification experiment namelist index

**Where**: new `src/verification_indexer/` module, new ChromaDB collection

**Source**: both test sessions; agents went to `docker run ... cat` when this was missing.

**Problem**: `input/data`, `input/eedata`, `input/data.pkg`, and `input/data.*` files from
MITgcm's 68 verification experiments are not indexed. These are the canonical reference for
how MITgcm is actually configured — far more grounding than abstract documentation. Agents
that cannot find them via MCP tools resort to reading files directly from the Docker image.

**Scope**: 68 experiments × ~4 namelist files each ≈ ~270 files, typical size 4–70 LOC.
Small enough to embed whole. `code/*.h` files from verification experiments are already
partially indexed (via the docs ChromaDB); this extends that coverage to the `input/` side.

**Pairs with**: tutorial-first workflow (Inbox). Together they make "find the closest
verification experiment, study its choices, diverge" a real tool-supported workflow.

---

## Codex CLI support

**Where**: `docker/mcp/entrypoint.sh`, `README.md`, `docs/mcp-server.md`

**Status**: Fixed in v2026.02.4.

**Root cause**: `ollama serve &` in the entrypoint inherited the container's
stdout, which is the MCP stdio pipe. Any bytes Ollama wrote to stdout before
Python took over corrupted the JSON-RPC framing. Codex CLI is stricter than
Claude Code about non-JSON on the pipe. Fix is one line:

```bash
ollama serve >/dev/null 2>&1 &
```

Cold-start latency is 0.73 s — well under Codex's 10-second default timeout,
so no `startup_timeout_sec` tuning is needed.

**Install** (once merged and pushed as `mitgcm-mcp-v2026.02.4`):

```bash
codex mcp add mitgcm -- docker run --rm -i ghcr.io/willirath/ogcmcp:mitgcm-mcp-v2026.02.4
```

**Remaining**: add `codex mcp add` install line to README and `docs/mcp-server.md`
after the merge.

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

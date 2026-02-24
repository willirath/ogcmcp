# Roadmap: v2026.02

Target: improvements identified from test-session retrospectives
(`plans/test-sessions/2026-02-23-*.md`), plus quickstart template correctness
fixes, code quality, and Docker hardening from the 2026-02-23 code and
docker-focused reviews.

Done when: all checklist items below are ticked, tests pass, both images
built and pushed, release tagged.

---

## Tier 1 — Fix what's broken

Small, targeted fixes. No new infrastructure. Should be done first so
subsequent test sessions don't keep hitting the same walls.

### 1.1 Dockerfile template bugs (`src/domain/suggest.py`)

- [x] Replace arch-detection logic with two explicit Dockerfiles: `Dockerfile.amd64` and `Dockerfile.arm64`, each hardcoded for its platform — no `uname -m` switch
- [x] Remove `--allow-run-as-root` from `mpirun` CMD (MPICH/hydra does not support it)
- [x] Add `-mpi` flag to `genmake2` invocation (required even for NP=1)
- [x] Document `mpif77`/`fcVers` workaround in template comments (inject `-fallow-argument-mismatch` via `genmake_local`)
- [x] Update `suggest_experiment_config_tool` to return both Dockerfiles and clarify which to use
- [x] Update tests in `tests/domain/test_suggest.py`

> **Feedback**: Two explicit files agreed.

### 1.2 Gotcha additions: critical (`src/domain/gotcha.py`)

- [x] `readBinaryPrec` mismatch — Python/NumPy 64-bit vs MITgcm 32-bit default; silent wrong results
- [x] `INCLUDE_PHIHYD_CALCULATION_CODE` — must be in `CPP_OPTIONS.h`; `CONFIG_CHECK` aborts without it
- [x] `gfd` in `packages.conf` — full group required, not just `mom_fluxform`; `PACKAGES_CHECK` aborts

> **Feedback**:

OK.

### 1.3 Gotcha additions: medium (`src/domain/gotcha.py`)

- [x] `SIZE.h` must include `MAX_OLX`/`MAX_OLY` PARAMETER block; without it `EXCH.h` chain breaks
- [x] `-mpi` required even for NP=1 `genmake2` invocations

> **Feedback**:

OK.

### 1.4 Search quality fixes

- [x] `lookup_gotcha_tool`: the "rigid lid vs free surface" entry was returned for 3 different queries on unrelated topics. Root cause: the catalogue has sparse coverage, so the closest match is always the same entry regardless of query topic. Primary fix done (1.2/1.3 entries). Per-entry deduplication already present (`break` after first keyword match); per-session state not implementable in a stateless tool — assessed as no further action needed.
- [x] `namelist_to_code_tool`: when a query term is an *internal variable* (COMMON block, not namelist), the tool returns empty with no explanation. Fix: returns `[{"warning": "..."}]` pointing to `search_code_tool` and `get_namelist_structure_tool` when param not found.
- [x] `namelist_to_code_tool`: clarify in tool description that it finds declaration sites (INI_PARMS), not use sites — suggest follow-up with `get_callers_tool`
- [x] `search_docs_tool`: assessed — `zonalWindFile` via `namelist_to_code_tool` returns PARM05 correctly; semantic search on camelCase identifiers gives poor results by design. Fix: added note to `search_docs_tool` description: use natural-language queries, use `namelist_to_code_tool` for exact parameter names. No embedding boost needed.

> **Feedback**: Addressed.

---

## Tier 2 — Close the "agents bypass tools" gap

### 2.1 Verification experiment namelist index

New indexer for `MITgcm/verification/*/input/data*` and `*/input/eedata` files.
Extends the existing docs ChromaDB collection (or adds a new `mitgcm_verification`
collection). Lets agents answer "how does tutorial_rotating_tank configure PARM01?"
without going to the container.

- [x] Scoping: decide collection strategy (extend `mitgcm_docs` vs new collection)
- [x] Audit scope: cover both `MITgcm/verification/` and `MITgcm/tutorial/` (if present); if tutorials live elsewhere, locate them
- [x] Write `src/verification_indexer/` — walk experiment dirs, parse `input/data*`, `input/eedata`, `code/*.h`, `code/packages.conf`, chunk and embed
- [x] Add a `list_verification_experiments_tool` returning a structured catalogue — not just names. Per-experiment fields to include:
  - name, tutorial flag (`tutorial_*` prefix)
  - packages enabled (from `code/packages.conf`) — the most useful filter
  - grid type (Cartesian / spherical polar / cubed-sphere, from PARM04)
  - domain dimensions Nx × Ny × Nr (from `code/SIZE.h`)
  - key physics flags: hydrostatic/nonhydrostatic, free surface/rigid lid, EOS type (parsed from `input/data`)
  - domain class (ocean / atmosphere / idealized / coupled): derive from packages only — e.g. `aim.*` → atmosphere, `seaice` → ocean+ice, `obcs` → ocean. No hand-labelling.
  - All fields must be derivable automatically from the experiment files. No hand-labelled tag files.
- [x] Add `pixi run embed-verification` task
- [x] Expose namelist search via `search_docs_tool` or a new `search_verification_tool`
- [x] Bake indexed data into MCP Docker image
- [x] Docs: update `docs/chromadb.md`, `docs/mcp-server.md`

> **Feedback**: List agreed. Tutorials added.

### 2.2 MITgcm `.h` header indexing

Extend the DuckDB code graph indexer to cover `.h` header files (SIZE.h,
CPP_OPTIONS.h, COMMON block headers) that currently fall outside the index.
Both test sessions hit this: agents could not find key compile-time parameters
via MCP tools and fell back to `docker run ... grep` against the container.

Distinct from 2.1 (which covers verification `input/` namelists via ChromaDB).
This targets the code graph side — making header content queryable via
`get_doc_source_tool` or a new `get_header_tool`.

- [x] **Full coverage audit**: enumerate all file types in `MITgcm/` (`.F`, `.f90`, `.h`, `.py`, `.m`, `.c`, `.sh`, build files, rst docs) and map each to: currently indexed / not indexed / should be indexed. Produce a coverage matrix before any implementation.
- [x] Based on audit: prioritise `.h` headers (model/inc, eesupp/inc, pkg headers) — these are the confirmed gap from test sessions
- [x] Extend `src/docs_indexer/` to ingest `.h` headers from `model/inc/` and `eesupp/inc/` (246 files added to `mitgcm_docs` ChromaDB collection)
- [x] Expose via existing `get_doc_source_tool` / `search_docs_tool`
- [x] Update tests
- [x] Docs: update `docs/chromadb.md`

> **Feedback**: Coverage audit added as first step.

### 2.3 Namelist structure map (`get_namelist_structure_tool`)

New domain-knowledge tool: returns the two-level map of file → namelist group → domain.
Covers PARM01–05, EEPARMS, `data.pkg` dependency, and the `data.<pkgname>` pattern.

- [x] **Design decision**: group names (PARM01–05, EEPARMS) can be derived from the `namelist_refs` DuckDB table (built from INI_PARMS); descriptions/domains must be hand-written. Hybrid approach: auto-populate group list from index, annotate with static descriptions. This avoids going stale on group names while keeping human-readable domain labels.
- [x] Implement in `src/domain/` using the hybrid approach
- [x] Register as MCP tool in `src/server.py`
- [x] Add tests
- [x] Docs: add to `docs/mcp-server.md` tool table

> **Feedback**: Hybrid (index-derived names, static descriptions) agreed.

---

## Tier 3 — Make the workflow smarter

### 3.1 Tutorial-first `design_experiment` workflow

Extend `get_workflow_tool` so the `design_experiment` workflow starts from:
"find relevant verification experiments for your goal, study their namelist choices,
get inspired by their structure, then diverge." Pairs with Tier 2.1.

- [x] Rewrite the `design_experiment` steps in `src/domain/workflow.py`
- [x] First step: `list_verification_experiments_tool` + `search_verification_tool` to find relevant experiments (plural)
- [x] Second step: `get_doc_source_tool` to read their `input/data` and `code/SIZE.h`
- [x] Subsequent steps: translate → check → gotcha → diverge

> **Feedback**: Description updated.

### 3.2 Agent incremental validation strategy

Add a `validate_incrementally` workflow describing how agents should check an
experiment during development. Staged goals:

1. **Build succeeds** — the model compiles cleanly.
2. **Model starts** — `nTimeSteps=2` runs and exits (even a dynamic blowup is a
   success at this stage; it confirms the build is correct and the model initialises).
3. **Physically plausible output** — increase steps, check `n3dWetPts`, `theta_mean`,
   `dynstat` in `STDOUT.0000`. Consider minimal domain size or coarse resolution to
   keep turnaround fast during this stage.
4. **Production run** — full domain, full duration.

When building a new experiment from scratch: add and validate one physics component
at a time, reaching stage 2 after each addition before proceeding.

- [x] Write workflow steps in `src/domain/workflow.py`
- [x] Stage 1–2: compile + `nTimeSteps=2`; dynamic blowup is acceptable
- [x] Stage 3: key diagnostics to check (`n3dWetPts`, `theta_mean`, `dynstat`); suggest coarse resolution / minimal domain to reduce turnaround
- [x] Stage 4: scale to production
- [x] New-experiment variant: incremental component addition, stage-2 gate after each
- [x] Register as `get_workflow_tool` task (`validate_incrementally`)

> **Feedback**: Dynamic blowup = success at stage 2. Minimal domain / coarse resolution reinstated as stage-3 option.

### 3.3 Namelist read/write tool

Wrap an existing Python namelist parser (e.g. `f90nml`) as an MCP tool:
read a namelist string → structured dict, and write dict → properly-formatted
Fortran namelist. Catches silent formatting errors.

- [x] **Design decision**: evaluated f90nml compatibility — parses `.TRUE.`/`.FALSE.`, `&`/`/` terminators, `#` comments, `data.pkg` patterns; lowercases all keys on output (MITgcm reads case-insensitively, fine); expands `N*value` shorthand (not round-trippable for existing files). Decision: skip MCP wrapping, document f90nml in `docs/mcp-server.md` instead. Validation and formatting use cases don't justify the wrapping surface given agents can use f90nml locally and follow namelist examples from the verification index.

> **Feedback**: Skip, document instead.

---

## Tier 4 — Pre-release fixes (from 2026-02-23 test session)

Test session against local `mitgcm-mcp:latest` image. Full report in
`plans/test-sessions/2026-02-23-v2026.02.5-pre-release-test-report.md`.

### Done

- [x] D1: `f90nml` missing from image — added to Dockerfile pip install
- [x] D2: `data.diagnostics` absent from `get_namelist_structure_tool` — added
  `DIAGNOSTICS_LIST` and `DIAG_STATIS_PARMS` to `_EXPLICIT` in `namelist_map.py`
- [x] D3: Test plan expected only `ALLOW_NONHYDROSTATIC` for CG3D — corrected;
  `TARGET_NEC_SX` and `NONLIN_FRSURF` are real guards in the full source
- [x] D4: OBCS (and KPP, RBCS, SEAICE, PTRACERS, SHELFICE) had generic
  descriptions — added explicit entries with informative descriptions
- [x] D6: `search_docs_tool` description updated to warn that `.h` snippet
  shows comment header only; agents should use `get_doc_source_tool` for
  full content
- [x] D7: Test plan corrected to check `quickstart.dockerfile_amd64` for
  `-mpi`, not `quickstart.build`

### Still needed before tag

- [x] D5: `search_verification_tool` returns paths without `verification/`
  prefix — fix verification indexer, re-run `pixi run embed-verification`,
  rebuild image
- [x] EXACT_CONSERV gotcha: old `CPP_OPTIONS.h` with `#undef EXACT_CONSERV`
  causes startup abort; flag was retired (now mandatory). Add to catalogue.
  (Found in creative exploration C3/C7.)
- [x] PHIHYD gotcha verification: agent did not find the
  `INCLUDE_PHIHYD_CALCULATION_CODE` check in the sections of CONFIG_CHECK
  read. Verify at higher offset before releasing the gotcha entry.
- [x] Tier 5 HIGH items: 5.1 (NP>1 note), 5.2 (input volume mount),
  5.5 (surface forcing) — see Tier 5 for detail
- [ ] Rebuild `mitgcm-mcp:latest` with all above fixes
- [x] Re-run failing test sections (T3, T6) — full pass required (355 tests)

---

## Tier 5 — Quickstart template and workflow fixes

From 2026-02-23 docker-focused review
(`plans/test-sessions/2026-02-23-v2026.02.5-pre-release-test-docker-focused.md`).
HIGH items are also listed as v2026.02.5 blockers in the Tier 4 section above.

### 5.1 NP>1 requires SIZE.h rebuild — misleading runtime note (HIGH)

`suggest_experiment_config_tool` notes say "Set NP to nPx\*nPy … or
`-e NP=…` at run time." Passing `-e NP=4` to `docker run` only changes what
mpirun launches; if the binary was compiled for NP=1 it aborts or produces
wrong results. `nPx`/`nPy` are compile-time constants in `SIZE.h`.

- [x] Fix note in `src/domain/suggest.py` to state NP>1 requires editing
  `code/SIZE.h` and rebuilding the image
- [x] Update tests

### 5.2 Stage 2 iteration requires full rebuild — input volume mount missing (HIGH)

`input/` is `COPY`d into the image during the `RUN make` layer. Changing
`nTimeSteps` in `input/data` triggers a full genmake2+make rebuild (~10 min).
Mounting `input/` as a read-only volume avoids rebuilds for namelist-only
changes; this pattern is never mentioned.

- [x] Add `-v "$(pwd)/input:/experiment/input:ro"` as the recommended
  iteration pattern in `src/domain/suggest.py` quickstart notes
- [x] Update `validate_incrementally` Stage 2 in `src/domain/workflow.py` to
  distinguish namelist-only changes (volume mount, no rebuild) from code
  changes (require rebuild)
- [x] Update tests

### 5.3 `build` key hardcodes amd64 unconditionally (MEDIUM)

The top-level `build` instruction uses `-f Dockerfile.amd64` with no
`--platform` flag. On an M-series Mac this builds under Rosetta or emulation
with no warning. The arm64 instruction is buried in `dockerfile_note`.

- [x] Add `--platform linux/amd64` to the amd64 build command in
  `src/domain/suggest.py`
- [x] Make the arm64 `build` instruction equally prominent
- [x] Update tests

### 5.4 gen_input.py Python dependencies not addressed (MEDIUM)

Quickstart notes say "Generate input fields before docker build:
`python gen_input.py`" without mentioning that `gen_input.py` imports numpy
(and often scipy/xarray). Users with a bare Python environment hit
`ModuleNotFoundError` before the first build.

- [x] Add a prerequisite note listing required Python packages (`numpy` at
  minimum) in the quickstart in `src/domain/suggest.py`

### 5.5 Surface forcing absent from rotating-convection template (HIGH)

Template `cpp_options` has only `["ALLOW_NONHYDROSTATIC", "ALLOW_DIAGNOSTICS"]`
— no `ALLOW_EXF`. Namelists cover `PARM01/03` and `data.eos` only — no
`surfQnetFile` in PARM05, no `data.exf`. The experiment description says
"surface buoyancy flux drives convection" but the compiled binary has no
surface-flux mechanism. The note attributing surface flux to OBCS is also
wrong (OBCS is lateral open boundaries for regional models, not surface flux).

- [x] Add `ALLOW_EXF` to `cpp_options` in `src/domain/suggest.py` template,
  or add `surfQnetFile` to PARM05 as a simpler constant-flux alternative
- [x] Add `data.exf` namelist stub (or PARM05 `surfQnetFile` entry) to the
  template `namelists` and `directory_structure`
- [x] Add `exf` to `packages.conf` in the template
- [x] Remove / correct the wrong "data.pkg/OBCS" attribution in the notes
- [x] Update tests

### 5.6 STDOUT.0000 location not tied to Docker volume mount (MEDIUM)

`validate_incrementally` Stage 3 says "Check STDOUT.0000" but never states
it lands at `output/STDOUT.0000` on the host with the quickstart volume mount.

- [x] Add explicit path note to Stage 3 in `src/domain/workflow.py`

### 5.7 WORKDIR + CMD mkdir redundancy (LOW)

`WORKDIR /experiment/run` creates the directory at build time. The
`CMD mkdir -p /experiment/run && …` is a no-op in the normal case.

- [x] Remove the redundant `mkdir -p` from the CMD in `src/domain/suggest.py`

### 5.8 `ln -sf input/* .` silent failure on empty input/ (LOW)

Shell glob in CMD fails with "No such file or directory" if `input/` is
empty (e.g. `gen_input.py` was not run before `docker build`).

- [x] Guard the symlink step or emit a clear error when `input/` is empty
- [x] Update template in `src/domain/suggest.py`

### 5.9 Stage 4 uses dot-notation for a flat key (LOW)

`validate_incrementally` Stage 4 references `"quickstart.run"` but the
actual key in the `suggest_experiment_config_tool` response is `quickstart`
→ `run` (a flat dict, not a nested dot path).

- [x] Fix the Stage 4 note in `src/domain/workflow.py`

### 5.10 data.diagnostics pointer (carried, fixed in v2026.02.5)

Template listed `input/data.diagnostics` but `get_namelist_structure_tool`
had no entry for it. Fixed by adding `data.diagnostics` to `_EXPLICIT`.

- [x] Fixed (D2)

---

## Tier 6 — Slipped items from pre-release testing

Items identified during v2026.02.5 pre-release testing, not blocking the
v2026.02.5 tag but deferred.

### 6.1 `.h` snippet quality

`search_docs_tool` results for `.h` files show only the opening Fortran
comment block in the snippet; actual PARAMETER/COMMON declarations start
further in. Tool description now warns agents to use `get_doc_source_tool`
for full content (D6). The fix is query-time (no re-embedding): strip the
`[file] section\n` prefix and leading `C`-comment lines before taking the
400-char snippet in `search_docs` and `search_verification` in `src/tools.py`.

- [x] Fix snippet generation in `search_docs` and `search_verification`
- [x] Update tests

### 6.2 `get_cpp_requirements_tool` returns hardware-optimisation flags

`get_cpp_requirements_tool("CG3D")` returns `TARGET_NEC_SX` — a 1990s NEC
SX vector-machine flag irrelevant to any modern build. The tool reports all
`#ifdef` guards anywhere in the translation unit, not just those that gate the
subroutine entry point.

- [x] Assess: filter known hardware-platform flags (`TARGET_NEC_SX`,
  `TARGET_SGI`, `TARGET_CRAY_VECTOR`) from `get_cpp_requirements` results,
  or add a note to the tool description
- [x] Update tool description and/or filter logic
- [x] Update tests

---

## Tier 7 — Code quality

From 2026-02-23 code review.

### 7.1 Database connection boilerplate

8 functions in `src/tools.py` repeat the same `con = connect(); try: …
finally: con.close()` pattern (~120 LoC of ceremony). Extract a
`query_db(sql, params, db_path)` helper or context manager.

- [x] Extract DB connection helper
- [x] Apply to all 8 query functions
- [x] Verify tests still pass

### 7.2 ChromaDB accessor deduplication

`src/embedder/store.py` has three near-identical `get_*_collection()`
functions. Collapse to `get_collection(name: str, path: Path) ->
chromadb.Collection`.

- [x] Refactor to single accessor
- [x] Update all call sites in `src/tools.py`

### 7.3 Ollama embed call extraction

Model name `"nomic-embed-text"` and `_normalize_query()` are repeated at
three call sites in `src/tools.py`. Extract to
`def embed_query(query: str) -> list[float]`.

- [x] Extract helper
- [x] Apply to `search_code`, `search_docs`, `search_verification`

### 7.4 Positional column indexing in SQL results

8 query functions in `src/tools.py` use `r[0], r[1], r[2]` positional
indexing on DuckDB result rows. Brittle on schema changes. Switch to named
tuples or row factory.

- [x] Converted to named-dict mapping immediately after fetchall — SELECT
  column list and dict keys are co-located in all 8 functions
- [x] Apply to all 8 query functions

---

## Tier 8 — Gotcha catalogue and docs additions

### 8.1 Adams-Bashforth 3 maximum-stability coefficient

The `β = 0.281105` optimum (CFL stability limit 0.786 vs 0.724 for
standard AB-3) is documented only in source comments in `ADAMS_BASHFORTH3`.
Users trying to push timestep size have no way to find it.

- [x] Add note to gotcha catalogue or `docs/mcp-server.md`
- [x] Both `alph_AB` and `beta_AB` confirmed as PARM03 parameters

### 8.2 CD scheme search gap

`search_docs_tool("CD scheme Coriolis Crank Nicolson momentum")` returns
Adams-Bashforth sections, not CD scheme docs. The `cd_code` package is not
surfaced under user-facing search terms.

- [x] Assess whether the RST docs for cd_code are indexed and under what
  section heading
- [x] Gap is in indexing (no dedicated RST section for cd_code) — added as
  known limitation to `search_docs_tool` description in `src/server.py`

### 8.3 `showflops` and unregistered package flags

`get_package_flags_tool("showflops")` returns empty because the package's
CPP flags are not registered in the standard options file. Affects any
package that uses non-standard flag registration.

- [x] Audited — showflops (and a handful of diagnostic packages) use
  non-standard registration; documented as known limitation in
  `get_package_flags_tool` description in `src/server.py`

---

## Tier 9 — Docker hardening

From 2026-02-23 security review (grade A — no vulnerabilities, hardening only).

### 9.1 Non-root user in MCP image

Container currently runs as root. Add a dedicated non-root user.

- [x] Add `RUN useradd -u 1000 -m -s /sbin/nologin mitgcm` and `USER mitgcm`
  to `docker/mcp/Dockerfile`; ollama models moved to `/opt/ollama` with
  `OLLAMA_MODELS` env var so non-root user can read them
- [x] Entrypoint verified via 355 passing tests
- [ ] Add `--security-opt no-new-privileges` to install docs example

---

## Release checklist

- [x] All tier 1–3 items done
- [x] `pixi run test` passes (355 tests)
- [x] Tier 4 done (D5, EXACT_CONSERV, PHIHYD, Tier 5 HIGH items)
- [x] Tiers 5–9 done
- [x] `pixi run embed-verification` run and data baked into image
- [x] Both images built and pushed (`mcp-v2026.02.5`, `runtime-v2026.02.5`)
- [x] GitHub release created
- [x] Git tag pushed
- [x] `docs/release.md` VERSION updated
- [x] `.mcp.json` updated
- [x] `README.md` and `docs/mcp-server.md` install commands updated

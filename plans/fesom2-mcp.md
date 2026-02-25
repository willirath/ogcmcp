# FESOM2 MCP Server — Design Plan

Design document for a FESOM2 MCP server analogous to the existing MITgcm MCP.

---

## What is FESOM2

FESOM2 (Finite-volumE Sea ice–Ocean Model) is an unstructured-mesh ocean–sea-ice
model developed at AWI. Key differences from MITgcm that shape the MCP design:

| Property | MITgcm | FESOM2 |
|---|---|---|
| LOC | ~2 M (submodule) | ~255 K (src/ only) |
| Source files | ~2 K `.F` files | ~209 `.F90` files |
| Code unit | Subroutines inside **packages** | Subroutines inside **Fortran 90 modules** |
| Build system | `make` + CPP package flags | CMake; minimal CPP (`ENABLE_OPENACC`, `DEBUG`) |
| Namelists | Scattered across `verification/` | Canonical in `config/namelist.*` (well-commented) |
| Docs | RST (~400 pages) | RST (~22 files, shorter) |
| Test configs | `verification/*/input/` rich namelists | `setups/*.yml` YAML CI overrides |
| Grid | Structured (Cartesian/spherical) | Unstructured triangular FEM mesh |

The smaller codebase and cleaner module structure make FESOM2 easier to index.
The unstructured-mesh physics introduces new gotchas that MITgcm's domain
knowledge layer doesn't cover.

_Feedback:_

---

## FESOM2 repo survey (submodule at 1b58e7f)

```
FESOM2/
├── src/                  125 files — F90 modules (one module per file)
├── config/               31 namelist.* files (Fortran namelists, well-commented)
├── setups/               19 setup.yml (YAML CI regression test overrides)
├── docs/                 22 .rst files (Sphinx)
├── test/                 pFUnit tests (.pf), mesh files (.out), NetCDF (.nc)
├── lib/metis-5.1.0/      mesh partitioning C library — skip entirely
└── visualization/        Python plotting scripts — skip
```

**Source modules** — representative groupings in `src/`:

| Prefix | Examples | Domain |
|---|---|---|
| `oce_` | `oce_dyn`, `oce_ale`, `oce_adv_tra_*`, `oce_fer_gm` | Ocean dynamics |
| `ice_` | `ice_EVP`, `ice_maEVP`, `ice_thermo_*`, `ice_modules` | Sea ice |
| `icb_` | `icb_dyn`, `icb_thermo`, `icb_step` | Icebergs |
| `gen_` | `gen_comm`, `gen_forcing_*`, `gen_modules_config` | Generic/infrastructure |
| `io_` | `io_netcdf_*`, `io_restart`, `io_meandata` | I/O |
| `MOD_` | `MOD_DYN`, `MOD_ICE`, `MOD_MESH`, `MOD_PARTIT` | Shared data structures |
| `toy_` | `toy_channel_*`, `toy_channel_neverworld2` | Idealised configurations |

**Namelists** in `config/`:

`namelist.config`, `namelist.dyn`, `namelist.oce`, `namelist.tra`,
`namelist.ice`, `namelist.forcing`, `namelist.io`, `namelist.cvmix`,
`namelist.icepack`, `namelist.transit` — plus forcing-specific variants
(`.CORE2`, `.JRA`, `.era5`, `.toy_*`).

All namelists are heavily commented inline (more self-documenting than MITgcm).

**Setups** in `setups/`: YAML files that override namelist parameters for CI.
Each setup pairs a mesh + forcing configuration with a set of namelist overrides
and expected output values (`fcheck`). They require external mesh data to run,
but the namelist overrides show which parameters distinguish each physics
configuration — useful as starting points for experiment design, analogous to
MITgcm tutorials. Indexed via `list_setups_tool`.

**Docs**: RST Sphinx covering geometry, discretisation (spatial, temporal,
vertical ALE), forcing, ocean/seaice/cvmix configuration, output, getting
started. Shorter than MITgcm docs but denser per page.

_Feedback:_

---

## What transfers from MITgcm MCP

| Component | Transfer | Notes |
|---|---|---|
| `src/domain/` — scales, dimensionless numbers | Full | Physics is model-agnostic |
| `src/domain/` — `translate_lab_params_tool` | Full | Geometry → namelist math unchanged |
| `src/domain/` — `check_scales_tool` | Full | Ro, Ek, Bu, CFL — same formulas |
| `src/embedder/pipeline.py` | ~80% | Change collection name; F90 chunk extraction |
| `src/docs_indexer/` | ~80% | RST parser reusable; paths differ |
| Docker patterns | Full | Same buildx workflow, base images |
| `pixi.toml` task structure | Full | Same `index`, `embed`, `serve`, etc. |
| FastMCP server pattern | Full | Same `@mcp.tool()` registration |
| `src/tools.py` helper layer | ~30% | DuckDB queries need module-based schema |
| `src/indexer/` | ~10% | F77-style parser must be rewritten for F90 |

**Namespacing** (same repo, two models): Python source uses `fesom2_` prefix
(`src/fesom2_indexer/`, `src/fesom2_tools.py`, `src/fesom2_server.py`).
Data artifacts land in `data/fesom2/` (DuckDB, ChromaDB). Docker image is
separate (`ghcr.io/willirath/ogcmcp:fesom2-mcp-*`). Within each
server the tool names are not prefixed — namespace comes from which server
(MCP entry) the user is connected to. Shared Python code (`src/domain/`,
embedding utilities) stays in place with no prefix.

_Feedback:_

---

## What needs new work

### 1. Fortran 90 module parser

MITgcm's indexer targets F77-style `.F` files with `SUBROUTINE` at top level.
FESOM2 uses F90 `MODULE ... CONTAINS ... END MODULE`. The parser must:

- Detect `MODULE <name>` / `END MODULE`
- Detect `SUBROUTINE` and `FUNCTION` inside `CONTAINS` blocks (with their parent module)
- Extract `USE <module>` statements (inter-module dependencies)
- Extract `CALL <name>` statements within subroutines
- Store file path, start/end line, parent module

CPP preprocessing: minimal; `#ifdef ENABLE_OPENACC` and `#ifdef DEBUG` blocks
can be stripped with a simple pass (no per-package CPP flag combinatorics).

`associate_mesh_*.h` Fortran include files: indexed as part of the module that
includes them, not as standalone entries.

_Feedback:_

### 2. DuckDB schema — module-based

Replace the `packages`+`subroutines` schema with:

```sql
modules(name, file, start_line, end_line)
subroutines(name, module_name, file, start_line, end_line, total_lines)
uses(module_name, used_module)         -- USE statements
calls(caller_name, callee_name, caller_module)  -- CALL statements
namelist_params(param, group, file, module_name, line)
```

_Feedback:_

### 3. Namelist → code linker

Read `config/namelist.*` and the subroutines that contain `NAMELIST / <group> /`
declarations referencing each parameter. Simpler than MITgcm because:
- All namelists are in one directory
- Inline comments give descriptions — parseable without docs lookup
- The reading modules follow a consistent pattern (e.g. `gen_modules_config.F90`)

The inline comments are rich enough to build a dedicated search collection:
embed each parameter as `{name} ({group} in {file}): {comment}` into a
`fesom2_namelists` ChromaDB collection. `search_docs_tool` queries both
`fesom2_docs` and `fesom2_namelists` so that "what does `K_GM_max` do?"
returns the namelist description alongside any RST coverage.

_Feedback:_

### 4. FESOM2-specific domain knowledge

New gotchas for:
- Unstructured mesh: node-vs-element variables, coastal boundary node sets
- ALE vertical coordinate: layer thickness constraints, tidal forcing with ALE
- Forcing: ERA5/CORE2/JRA conventions, interpolation onto unstructured mesh
- Sea ice: EVP vs mEVP rheology choices, icepack coupling
- Output: FEM-native vs regridded output, `namelist.io` diagnostic list

New `suggest_experiment_config_tool` entries:
- `baroclinic_channel` — Soufflet-type / Neverworld2 double-gyre channel
- `pi_control` — global pre-industrial control setup pointers
- `rotating_convection` — transferable from MITgcm domain knowledge

_Feedback:_

### 5. `list_setups_tool`

Parse `setups/*/setup.yml` to catalogue CI setups with their mesh, forcing,
namelist overrides, and validation fields (`fcheck`). Shows which parameters
distinguish each physics configuration — useful as config starting points.

_Feedback:_

### 6. `src/` subdirectory audit

`int_recom/`, `cvmix_driver/`, `icepack_drivers/`, `async_threads_cpp/` are
subdirectories under `src/`. Inspect each before deciding what to index:
some may be third-party libraries (CVMix, Icepack) where only the FESOM2
driver/interface files should be indexed, not the library internals.

_Feedback:_

### 7. pFUnit tests

8 `.pf` files (~300 lines) covering infrastructure modules only (forcing I/O,
MPI topology, NetCDF, string utils) — no ocean/sea-ice physics. pFUnit syntax
is F90 with `@test`/`@assertEqual` macros.

Decision: no separate collection. Strip the pFUnit macros (treat as comments)
and run `.pf` files through the regular F90 module parser. Test subroutines
land in `fesom2_subroutines` alongside production code; call graph picks up
their `USE`/`CALL` statements naturally. No extra embedding pass.

_Feedback:_

---

## Repo strategy

**Decision: same repo, `fesom2` branch, namespaced Python source.**

- `src/fesom2_indexer/` — F90 module parser
- `src/fesom2_tools.py` — DuckDB + ChromaDB query layer
- `src/fesom2_server.py` — FastMCP server (separate from `src/server.py`)
- `data/fesom2/` — DuckDB index + ChromaDB collections
- `docker/fesom2-mcp/Dockerfile` — separate Docker image
- `.mcp.json` — two entries: `mitgcm` and `fesom2`

Shared: `src/domain/`, embedding pipeline base, `pixi.toml` tasks.

If the repo grows too complex, the FESOM2 server can be split to
`2026-fesom2-mcp` with a clean cut at that point.

_Feedback:_

---

## Tool inventory

### Code navigation

| Tool | What it does |
|---|---|
| `search_code_tool` | Semantic search over FESOM2 subroutine embeddings |
| `find_modules_tool` | Find F90 modules by name |
| `find_subroutines_tool` | Find subroutines by name (across modules) |
| `get_module_tool` | Metadata for a module (file, contained routines) |
| `get_source_tool` | Paginated source for a subroutine |
| `get_callers_tool` | What calls this subroutine |
| `get_callees_tool` | What this subroutine calls |
| `get_module_uses_tool` | Which modules does module X USE |
| `namelist_to_code_tool` | Which module reads a namelist parameter |

_Feedback:_

### Documentation search

| Tool | What it does |
|---|---|
| `search_docs_tool` | Semantic search over FESOM2 RST docs + namelist descriptions |
| `get_doc_source_tool` | Full paginated text of a doc section |
| `list_setups_tool` | Catalogue of CI setups with mesh/forcing/overrides |

_Feedback:_

### Domain knowledge

| Tool | What it does |
|---|---|
| `translate_lab_params_tool` | Physical parameters → namelist values (reused) |
| `check_scales_tool` | Ro, Ek, Bu, CFL checks (reused) |
| `lookup_gotcha_tool` | FESOM2-specific configuration traps |
| `suggest_experiment_config_tool` | Skeleton config for idealised experiments |
| `get_namelist_structure_tool` | Map of namelist files → groups → descriptions |

_Feedback:_

### Workflow guidance

`get_workflow_tool` is model-specific — each server registers its own version
with FESOM2-appropriate task names (`understand_module` instead of
`understand_package`, etc.). The underlying Python function is parametrised by
model but the registered tool carries no prefix.

| Tool | What it does |
|---|---|
| `get_workflow_tool` | Recommended tool sequence per task (FESOM2-specific) |

**Total: ~19 tools** (vs 21 in MITgcm MCP — no package flags tool, no
diagnostics fill tool; adds `find_modules_tool`, `get_module_tool`,
`get_module_uses_tool`).

_Feedback:_

---

## Implementation tiers

### Tier 1 — F90 module parser → DuckDB

- [x] Audit `src/` subdirs (`int_recom/`, `cvmix_driver/`, `icepack_drivers/`, `async_threads_cpp/`)
- [x] Parse `.F90` files: extract modules, subroutines, functions
- [x] Handle `associate_mesh_*.h` includes as part of parent module
- [x] Extract USE statements; build module dependency graph
- [x] Extract CALL statements within subroutines
- [x] DuckDB schema: `modules`, `subroutines`, `uses`, `calls`
- [x] Strip minimal CPP (`#ifdef` blocks)
- [x] Tests: synthetic F90 fixtures (29 tests)
- [x] `pixi run fesom2-index` task
- Done when: `find_modules_tool`, `find_subroutines_tool`, `get_source_tool`,
  `get_callers_tool`, `get_callees_tool`, `get_module_uses_tool` all pass tests

_Feedback:_

### Tier 2 — Namelist → code linker

- [x] Parse `config/namelist.*`: groups → parameters → inline comments
- [x] Detect `NAMELIST / <group> / ...` declarations in source
- [x] Link parameters to reading modules → DuckDB `namelist_refs` table
- [x] `namelist_descriptions` table from config files (460 refs, 405 descriptions)
- [x] Tests: synthetic namelist + F90 fixture (16 new tests)
- Done when: `namelist_to_code_tool("step_per_day")` returns `g_config` ✓

_Feedback:_

### Tier 3 — Embedding pipeline

- [x] Adapt embedder for FESOM2 subroutines → ChromaDB `fesom2_subroutines`
- [x] Adapt docs indexer for FESOM2 RST → ChromaDB `fesom2_docs`
- [x] Embed namelist parameter descriptions → ChromaDB `fesom2_namelists`
- [x] Include `.pf` files in F90 parser (strip pFUnit macros; no separate collection) — done in Tier 1
- [x] `pixi run fesom2-embed`, `pixi run fesom2-embed-docs`, `pixi run fesom2-embed-namelists` tasks
- [x] Synthetic tests: 22 new tests (chunking + nml doc building); 430 total pass
- Done when: semantic search returns sensible results for "GM eddy parameterization"

_Feedback:_

### Tier 4 — Domain knowledge layer

- [x] FESOM2 gotchas (unstructured mesh, ALE, forcing, sea ice)
- [x] FESOM2 `suggest_experiment_config_tool` entries
- [x] Reuse `translate_lab_params_tool` and `check_scales_tool` unchanged
- [x] Tests: keyword-based gotcha tests (synthetic)
- Done when: `lookup_gotcha_tool("ALE")` returns meaningful entries

_Feedback:_

### Tier 5 — Setups catalogue

- [x] Parse `setups/*/setup.yml` → `list_setups_tool` response
- [x] Parse `config/namelist.X.suffix` → reference_namelist records with `{value, comment}` leaves
- [x] Tests: parse synthetic YAML and Fortran namelists (35 tests)
- Done when: `list_setups_tool()` returns all setups with mesh/forcing/override fields

_Feedback:_

### Tier 6 — MCP server + FastMCP wiring

- [x] `src/fesom2/server.py` with all 19 tools registered
- [x] Tool descriptions tuned for FESOM2 concepts (modules not packages, namelist.io, ALE, METIS)
- [x] `get_workflow_tool` workflows: `design_experiment`, `debug_configuration`,
  `understand_module`, `explore_code`
- [x] `tests/fesom2/test_server.py` — tool count + name coverage (5 tests)
- [x] Update `.mcp.json` with `fesom2` entry (`pixi run fesom2-serve`)
- Done when: all tests pass with `pixi run test`

_Feedback:_

### Tier 7 — Docker packaging + release

- [x] `docker/fesom2-mcp/Dockerfile` — Python + Ollama + indices
- [ ] Multi-arch buildx push to GHCR
- [x] `docs/fesom2-release.md` written
- [ ] Smoke test on clean machine
- Done when: `docker run --rm -i ghcr.io/willirath/ogcmcp:fesom2-mcp-v2026.02.6`
  answers "What controls GM diffusivity?" correctly

_Feedback:_

---

## Open questions

1. **`int_recom/`, `cvmix_driver/`, `icepack_drivers/`, `async_threads_cpp/`**:
   Inspect each subdirectory before deciding scope. Index FESOM2 driver/interface
   files; determine whether library internals (CVMix, Icepack) should be included
   or skipped.

   _Feedback:_ Yes, dig in before we decide anything.


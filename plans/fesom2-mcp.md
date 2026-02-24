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
Not analogous to MITgcm verification experiments — they reference external mesh
data and do not contain standalone physics configurations. Minimal indexing value.

**Docs**: RST Sphinx covering geometry, discretisation (spatial, temporal,
vertical ALE), forcing, ocean/seaice/cvmix configuration, output, getting
started. Shorter than MITgcm docs but denser per page.

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

### 2. DuckDB schema — module-based

Replace the `packages`+`subroutines` schema with:

```sql
modules(name, file, start_line, end_line)
subroutines(name, module_name, file, start_line, end_line, total_lines)
uses(module_name, used_module)         -- USE statements
calls(caller_name, callee_name, caller_module)  -- CALL statements
namelist_params(param, group, file, module_name, line)
```

### 3. Namelist → code linker

Read `config/namelist.*` and the subroutines that contain `NAMELIST / <group> /`
declarations referencing each parameter. Simpler than MITgcm because:
- All namelists are in one directory
- Inline comments give descriptions — parseable without docs lookup
- The reading modules follow a consistent pattern (e.g. `gen_modules_config.F90`)

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

### 5. `list_setups_tool`

Parse `setups/*/setup.yml` to catalogue CI setups with their mesh, forcing,
namelist overrides, and validation fields (`fcheck`). Lightweight analog of
`list_verification_experiments_tool`.

---

## Repo strategy

Two options:

**A. New repo `2026-fesom2-mcp`** (recommended)
- Clean separation; independent releases and Docker images
- FESOM2 submodule lives there
- Copy shared infrastructure (domain knowledge, embedding base) at branch point
- Risk: duplication; shared fixes must be applied twice

**B. Extend this repo**
- Add `src/fesom2_indexer/`, `src/fesom2_tools.py`, `src/fesom2_server.py`
- Single pixi environment; shared domain knowledge naturally
- Risk: entanglement; single Docker image serving two models gets complex
- Not recommended: two very different models → two Docker images → two repos cleaner

Decision: **new repo** once the FESOM2 branch here is validated as a design
prototype. The `fesom2` branch in this repo is the staging area for the parser
and schema work; once Tier 1–2 are passing, migrate to `2026-fesom2-mcp`.

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

### Documentation search

| Tool | What it does |
|---|---|
| `search_docs_tool` | Semantic search over FESOM2 RST docs |
| `get_doc_source_tool` | Full paginated text of a doc section |
| `list_setups_tool` | Catalogue of CI setups with mesh/forcing/overrides |

### Domain knowledge

| Tool | What it does |
|---|---|
| `translate_lab_params_tool` | Physical parameters → namelist values (reused) |
| `check_scales_tool` | Ro, Ek, Bu, CFL checks (reused) |
| `lookup_gotcha_tool` | FESOM2-specific configuration traps |
| `suggest_experiment_config_tool` | Skeleton config for idealised experiments |
| `get_namelist_structure_tool` | Map of namelist files → groups → descriptions |

### Workflow guidance

| Tool | What it does |
|---|---|
| `get_workflow_tool` | Recommended tool sequence per task |

**Total: ~18 tools** (vs 21 in MITgcm MCP — fewer because no package flags,
no verification source tool, no diagnostics fill tool).

---

## Implementation tiers

### Tier 1 — F90 module parser → DuckDB

- [ ] Parse `.F90` files: extract modules, subroutines, functions
- [ ] Extract USE statements; build module dependency graph
- [ ] Extract CALL statements within subroutines
- [ ] DuckDB schema: `modules`, `subroutines`, `uses`, `calls`
- [ ] Strip minimal CPP (`#ifdef` blocks)
- [ ] Tests: synthetic F90 fixtures
- [ ] `pixi run index` task
- Done when: `find_modules_tool`, `find_subroutines_tool`, `get_source_tool`,
  `get_callers_tool`, `get_callees_tool`, `get_module_uses_tool` all pass tests

### Tier 2 — Namelist → code linker

- [ ] Parse `config/namelist.*`: groups → parameters → inline comments
- [ ] Detect `NAMELIST / <group> / ...` declarations in source
- [ ] Link parameters to reading modules → DuckDB `namelist_params` table
- [ ] Build `get_namelist_structure_tool` response from DuckDB
- [ ] Tests: synthetic namelist + F90 fixture
- Done when: `namelist_to_code_tool("step_per_day")` returns `gen_modules_config`

### Tier 3 — Embedding pipeline

- [ ] Adapt embedder for FESOM2 subroutines → ChromaDB `fesom2_subroutines`
- [ ] Adapt docs indexer for FESOM2 RST → ChromaDB `fesom2_docs`
- [ ] `pixi run embed`, `pixi run embed-docs` tasks
- [ ] `search_code_tool`, `search_docs_tool` tests with real Ollama
- Done when: semantic search returns sensible results for "GM eddy parameterization"

### Tier 4 — Domain knowledge layer

- [ ] FESOM2 gotchas (unstructured mesh, ALE, forcing, sea ice)
- [ ] FESOM2 `suggest_experiment_config_tool` entries
- [ ] Reuse `translate_lab_params_tool` and `check_scales_tool` unchanged
- [ ] Tests: keyword-based gotcha tests (synthetic)
- Done when: `lookup_gotcha_tool("ALE")` returns meaningful entries

### Tier 5 — Setups catalogue

- [ ] Parse `setups/*/setup.yml` → `list_setups_tool` response
- [ ] Tests: parse synthetic YAML
- Done when: `list_setups_tool()` returns all 14 setups with mesh/forcing fields

### Tier 6 — MCP server + FastMCP wiring

- [ ] `src/fesom2_server.py` with all 18 tools registered
- [ ] Tool descriptions tuned for FESOM2 concepts
- [ ] `get_workflow_tool` workflows: `design_experiment`, `debug_configuration`,
  `understand_module`, `explore_code`
- [ ] `test_server.py` — tool count + name coverage
- Done when: all tests pass with `pixi run test`

### Tier 7 — Docker packaging + release

- [ ] `docker/fesom2-mcp/Dockerfile` — Python + Ollama + indices
- [ ] Multi-arch buildx push to GHCR
- [ ] `docs/release.md` updated
- [ ] Smoke test on clean machine
- Done when: `docker run --rm -i ghcr.io/willirath/2026-fesom2-mcp:mcp-v2026.03.1`
  answers "What controls GM diffusivity?" correctly

---

## Open questions

1. **Repo migration timing**: At what tier does the `fesom2` branch become its
   own repo? Recommendation: after Tier 1 tests pass, fork to
   `2026-fesom2-mcp` and continue there.

2. **`associate_mesh_*.h` files**: Fortran include files in `src/` that define
   mesh accessor macros. Should be indexed as part of the module that includes
   them (not as standalone files). Treat as part of the parent module.

3. **`int_recom/`, `cvmix_driver/`, `icepack_drivers/`, `async_threads_cpp/`**:
   These are subdirectories in `src/`. Recurse into them for F90 files, but
   inspect first — some may be third-party (e.g. CVMix, Icepack are external
   libraries). Index FESOM2 driver/interface files only, skip pure library code.

4. **pFUnit tests** (`.pf` files): Parseable as F90 (pFUnit extends F90 with
   macros). Low value for the MCP; skip for now.

5. **Security flag**: Should the FESOM2 MCP image add `--security-opt
   no-new-privileges` to install docs? Carry from MITgcm v2026.02 open item.

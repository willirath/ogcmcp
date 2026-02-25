# Smoke test results — v2026.02.6

**Date:** 2026-02-25
**Agent:** Claude Code (claude-sonnet-4-6)
**Test plan:** `smoke-test-v2026.02.6.md`

---

## Executive summary

| Part | Status | Critical failures |
|------|--------|------------------|
| A — MITgcm MCP | **PARTIAL PASS** | A6 (verification experiment list empty) |
| B — FESOM2 MCP | **FAIL** | B1 (tool count), B2 (K_GM not indexed), B3 (find_modules broken), B5 (setups catalogue empty) |
| C — Runtime images | **PASS** | — |

**Release verdict: NOT READY TO TAG.** Four critical FESOM2 tests fail.

---

## Part A — MITgcm MCP server (`mitgcm-mcp-v2026.02.6`)

### A1. Tool discovery

**Result: PASS**

Counted 23 tools from the MCP server's exported tool list:

| # | Tool name |
|---|-----------|
| 1 | `search_code_tool` |
| 2 | `find_subroutines_tool` |
| 3 | `get_subroutine_tool` |
| 4 | `get_source_tool` |
| 5 | `get_callers_tool` |
| 6 | `get_callees_tool` |
| 7 | `namelist_to_code_tool` |
| 8 | `diagnostics_fill_to_source_tool` |
| 9 | `get_cpp_requirements_tool` |
| 10 | `get_package_flags_tool` |
| 11 | `find_packages_tool` |
| 12 | `get_package_tool` |
| 13 | `translate_lab_params_tool` |
| 14 | `check_scales_tool` |
| 15 | `lookup_gotcha_tool` |
| 16 | `suggest_experiment_config_tool` |
| 17 | `get_namelist_structure_tool` |
| 18 | `get_workflow_tool` |
| 19 | `list_verification_experiments_tool` |
| 20 | `search_verification_tool` |
| 21 | `get_verification_source_tool` |
| 22 | `search_docs_tool` |
| 23 | `get_doc_source_tool` |

All required names (`search_code_tool`, `get_source_tool`, `namelist_to_code_tool`, `translate_lab_params_tool`, `check_scales_tool`, `lookup_gotcha_tool`, `find_packages_tool`, `get_package_tool`, `get_namelist_structure_tool`) present.

---

### A2. Namelist parameter lookup

**Result: PASS**

`namelist_to_code_tool("cg3dMaxIters")` returned:

```json
[{"id": 216, "name": "INI_PARMS", "file": "MITgcm/model/src/ini_parms.F",
  "package": "model", "namelist_group": "PARM02"}]
```

- Parameter: `cg3dMaxIters` ✓
- Reading subroutine: `INI_PARMS` ✓
- Namelist group: `PARM02` ✓

---

### A3. Code search + source

**Result: PASS**

`search_code_tool("GM Gent-McWilliams eddy parameterisation")` returned results in the `gmredi` package:

- `GMREDI_DIAGNOSTICS_INIT` (`pkg/gmredi/gmredi_diagnostics_init.F`)
- `GMREDI_CALC_BATES_K` (`pkg/gmredi/gmredi_calc_bates_k.F`)
- `CALC_EDDY_STRESS` (model/src)

`get_source_tool("GMREDI_CALC_BATES_K", package="gmredi")` returned real Fortran source (1157 total lines), starting:

```fortran
      SUBROUTINE GMREDI_CALC_BATES_K(
     I                  iMin, iMax, jMin, jMax,
     I                  sigmaX, sigmaY, sigmaR,
     I                  bi, bj, myTime, myIter, myThid )
C     | SUBROUTINE GMREDI_CALC_BATES_K
C     | o Calculates the 3D diffusivity as per Bates et al. (2014)
```

Source non-empty, no error. ✓

---

### A4. Call graph (CG3D)

**Result: PASS**

`get_callees_tool("CG3D")` returned 4 callees:

- `WRITE_FLD_S3D_RL`
- `GLOBAL_SUM_TILE_RL`
- `PRINT_MESSAGE`
- `EXCH_S3D_RL`

`get_callers_tool("CG3D")` returned:

- `SOLVE_FOR_PRESSURE` (`MITgcm/model/src/solve_for_pressure.F`) ✓ (pressure-solve subroutine)

Both lists non-empty. ✓

---

### A5. Documentation search

**Result: PASS**

`search_docs_tool("Adams-Bashforth time-stepping scheme")` returned 5 results:

| File | Section |
|------|---------|
| `algorithm/algorithm.rst` | Explicit time-stepping: Adams-Bashforth |
| `examples/cfc_offline/cfc_offline.rst` | Time-stepping of tracers |
| `algorithm/algorithm.rst` | Adams-Bashforth III |
| `getting_started/getting_started.rst` | Time-Discretization |
| `algorithm/algorithm.rst` | Synchronous time-stepping: variables co-located in time |

All results have `file` and `section` fields. Snippet content clearly describes the AB-II method and its context in the pressure solve. ✓

---

### A6. Verification experiment lookup

**Result: FAIL** *(Critical)*

`list_verification_experiments_tool()` returned an **empty list** `{"result": []}`. The tool is expected to return a structured catalogue of all verification experiments including a `nonhydrostatic` boolean field. This tool is non-functional.

`search_verification_tool("nonHydrostatic true non-hydrostatic pressure solver")` returned 5 results, but all snippets showed `CPP_OPTIONS.h` content with `#undef ALLOW_CG2D_NSA` style comments — none showed `nonHydrostatic = .TRUE.` or `ALLOW_NONHYDROSTATIC` defined. The search did not surface an experiment with non-hydrostatic dynamics enabled.

- `list_verification_experiments_tool` broken (empty) ✗
- `search_verification_tool` returns no experiment explicitly using nonHydrostatic ✗

---

### A7. Lab parameter translation + scales

**Result: PASS**

`translate_lab_params_tool(Lx=0.8, Ly=0.8, depth=0.2, Omega=0.314, delta_T=2, Nx=80, Ny=80, Nz=20)` returned:

```json
{
  "PARM01": {"f0": 0.628, "beta": 0.0, "viscAh": 1e-6, "diffKhT": 1.4e-7, ...},
  "EOS_PARM01": {"eosType": "LINEAR", "tAlpha": 0.0002, ...},
  "PARM04": {"delX": 0.01, "delY": 0.01, "delZ": 0.01},
  "notes": ["Aspect ratio 0.25 — consider nonHydrostatic = .TRUE. in PARM01"]
}
```

- `f0 = 0.628 = 2 × 0.314` ✓
- `viscAh` and `diffKhT` present ✓

`check_scales_tool(...)` returned:

```json
{
  "numbers": {"f0": 0.628, "aspect_ratio": 0.25, "N": 0.14, "Ek_v": 3.98e-5, ...},
  "flags": [
    {"level": "warning", "message": "Aspect ratio 0.25 — non-hydrostatic effects likely significant"},
    {"level": "warning", "message": "Ekman layer depth 1.3 mm < dz 10.0 mm — Ekman layer not resolved"},
    {"level": "info", "message": "Spin-up requires ~158 rotation periods"}
  ]
}
```

- Dimensionless numbers returned ✓
- Aspect ratio warning raised (depth/L = 0.2/0.8 = 0.25 > 0.1) ✓
- No errors ✓

---

### A8. Gotcha lookup

**Result: PASS**

`lookup_gotcha_tool("nonHydrostatic")` returned 1 entry:

```
Title: Non-hydrostatic pressure solve requires CPP flag
Summary: nonHydrostatic = .TRUE. requires ALLOW_NONHYDROSTATIC in CPP_OPTIONS.h
         and useNHMTerms = .TRUE. in &PARM01.
Detail: Setting nonHydrostatic = .TRUE. in data &PARM01 is not sufficient on its own.
        Add '#define ALLOW_NONHYDROSTATIC' to CPP_OPTIONS.h and set
        'useNHMTerms = .TRUE.' in &PARM01. Without the CPP flag the non-hydrostatic
        terms are silently omitted.
```

- Entry returned ✓
- Mentions `ALLOW_NONHYDROSTATIC` CPP flag ✓
- Mentions `useNHMTerms` ✓

---

### A9. Namelist structure

**Result: PASS**

`get_namelist_structure_tool()` returned a large dict. Checking `data.obcs`:

```json
"data.obcs": {
  "OBCS_PARM01": "Open boundary conditions core: which boundaries are open ...",
  "OBCS_PARM02": "OBC sponge-layer relaxation: ...",
  "OBCS_PARM03": "OBC tidal forcing: ...",
  "OBCS_PARM04": "OBC passive tracer boundary conditions: ...",
  "OBCS_PARM05": "OBC runoff forcing: ..."
}
```

- `data.obcs` present ✓
- `OBCS_PARM01` listed with description ✓

---

### A10. Package navigation (kpp)

**Result: PASS**

`get_package_tool("kpp")` returned:

- `subroutine_count: 27`
- Files: all in `MITgcm/pkg/kpp/` ✓
- Subroutines include: `KPP_CALC`, `KPPMIX`, `bldepth`, `wscale`, `Ri_iwmix`, `blmix`, `KPP_READPARMS`, `KPP_INIT_FIXED`, etc.
- CPP flags: `KPP_SMOOTH_SHSQ`, `KPP_SMOOTH_DBLOC`, `KPP_GHAT`

Non-empty subroutine list, all paths reference `pkg/kpp/`. ✓

---

## Part B — FESOM2 MCP server (`fesom2-mcp-v2026.02.6`)

### B1. Tool discovery

**Result: FAIL** *(Critical)*

Counted **19** tools from the MCP server's exported tool list (expected: 20):

| # | Tool name |
|---|-----------|
| 1 | `search_code_tool` |
| 2 | `find_modules_tool` |
| 3 | `find_subroutines_tool` |
| 4 | `get_module_tool` |
| 5 | `get_source_tool` |
| 6 | `get_callers_tool` |
| 7 | `get_callees_tool` |
| 8 | `get_module_uses_tool` |
| 9 | `namelist_to_code_tool` |
| 10 | `search_docs_tool` |
| 11 | `get_doc_source_tool` |
| 12 | `list_setups_tool` |
| 13 | `translate_lab_params_tool` |
| 14 | `check_scales_tool` |
| 15 | `lookup_gotcha_tool` |
| 16 | `get_run_interface_tool` |
| 17 | `suggest_experiment_config_tool` |
| 18 | `get_namelist_structure_tool` |
| 19 | `get_workflow_tool` |

Required names all present. Tool count 19 ≠ 20. ✗

---

### B2. Namelist parameter lookup (K_GM)

**Result: FAIL** *(Critical)*

`namelist_to_code_tool("K_GM")` returned:

```json
{"warning": "'K_GM' was not found as a namelist parameter. It may be an internal
  model variable rather than a namelist input."}
```

`namelist_to_code_tool("K_gm")` (case variant) also returned the same not-found warning.

`search_docs_tool("K_GM GM diffusivity Gent McWilliams namelist parameter")` confirmed GM is implemented via the Ferrari (2010) algorithm in FESOM2, but no namelist parameter named `K_GM` was found in the index. The namelist controlling GM diffusivity is likely under a different name not surfaced by the index.

- `K_GM` not returned ✗
- Namelist group not identified ✗

---

### B3. Module navigation (GM/Redi)

**Result: FAIL** *(Critical)*

`find_modules_tool("oce_fer_gm")` → empty
`find_modules_tool("fer")` → empty
`find_modules_tool("oce")` → empty (despite `search_code_tool` returning subroutines with `module_name="oce_ale"`)
`find_modules_tool("oce_ale")` → empty

`find_modules_tool` returns empty for every query tested. The module index in the database appears to be empty or the lookup logic is broken. (Note: `search_code_tool` does surface subroutine results with valid module names, confirming the subroutine index works but the module index does not.)

`search_code_tool("GM Redi eddy parameterisation")` and `search_code_tool("Gent McWilliams bolus velocity skew flux")` did not return a clearly GM-specific module.

- No relevant GM module found ✗
- `find_modules_tool` non-functional ✗
- `get_module_uses_tool` could not be tested (no module name to pass) ✗

---

### B4. Docs + namelist search (ALE)

**Result: PASS**

`search_docs_tool("ALE vertical coordinate")` returned 5 results:

| Source | File/param | Section/group |
|--------|-----------|---------------|
| doc | `vertical_discretization.rst` | Vertical discretization: Layer thicknesses |
| doc | `temporal_discretization.rst` | Current options for the vertical coordinate |
| namelist | `which_ALE` in `ale_def` | `FESOM2/config/namelist.config` |
| doc | `vertical_discretization.rst` | (density/pressure section) |
| namelist | `A_ver` in `oce_dyn` | `FESOM2/config/namelist.oce` |

- Results have `file` and `section` fields ✓
- At least one result explicitly mentions ALE (`which_ALE` parameter, ALE docs) ✓

---

### B5. Setups catalogue

**Result: FAIL** *(Critical)*

All variants of `list_setups_tool` returned empty:

- `list_setups_tool(names_only=True)` → `{"result": []}`
- `list_setups_tool(names_only=True, source="reference_namelist")` → `{"result": []}`
- `list_setups_tool(names_only=True, source="ci_setup")` → `{"result": []}`
- `list_setups_tool(name="neverworld2")` → `{"result": []}`

The setups catalogue database is empty or not populated. The pass criteria requires multiple setups returned and `namelist.oce`/`namelist.config` stanzas for `toy_neverworld2`.

- `list_setups_tool(names_only=True)` returns empty ✗
- `list_setups_tool(name="neverworld2")` returns empty ✗

---

### B6. Lab parameter translation + scales

**Result: PASS** *(with note)*

`translate_lab_params_tool(Lx=4e6, Ly=2e6, depth=4000, Omega=5.1e-5, delta_T=5, Nx=200, Ny=100, Nz=40)` returned:

```json
{
  "PARM01": {"f0": 0.000102, "viscAh": 1e-6, "diffKhT": 1.4e-7, ...},
  "EOS_PARM01": {"eosType": "LINEAR", "tAlpha": 0.0002},
  "PARM04": {"delX": 20000.0, "delY": 20000.0, "delZ": 100.0},
  "derived": {"f0": 0.000102, "L": 2000000.0, "aspect_ratio": 0.002, ...}
}
```

- `f0 = 0.000102 = 2 × 5.1e-5` ✓
- No Python exception ✓

`check_scales_tool(...)` returned:

```json
{
  "numbers": {"f0": 1.02e-4, "aspect_ratio": 0.002, "N": 0.00157, "Bu": 9.43e-4, ...},
  "flags": [
    {"level": "warning", "message": "Ekman layer depth 99.0 mm < dz 100000.0 mm — Ekman layer not resolved"},
    {"level": "info", "message": "Spin-up requires ~40398 rotation periods"}
  ]
}
```

- Burger number (`Bu`) returned ✓
- Flags returned ✓
- **Note:** Rossby number not returned (requires `U` parameter not provided). Pass criteria lists "Rossby number" but it cannot be computed without a velocity scale; this is a design limitation, not a bug.

---

### B7. Experiment skeleton

**Result: PASS**

`suggest_experiment_config_tool("baroclinic_channel")` returned:

```json
{
  "experiment_type": "baroclinic_channel",
  "description": "Neverworld2-style re-entrant baroclinic channel...",
  "namelists": {
    "namelist.config": {"step_per_day": "...", "run_length": "...", "toy_ocean": ".true.", ...},
    "namelist.oce": {"ALE_transform": "'zstar'", "min_hnode": "1.0", ...}
  },
  "notes": ["toy_ocean=.true. bypasses the bulk forcing pipeline...", ...]
}
```

- Returns a dict ✓
- Keys include `namelists` with `namelist.config` and `namelist.oce` ✓
- `notes` list non-empty (8 notes) ✓

---

### B8. Gotcha lookup (ALE)

**Result: PASS**

`lookup_gotcha_tool("ALE")` returned 1 entry:

```
Title: ALE minimum layer thickness: min_hnode prevents layer collapse
Keywords: ale, min_hnode, layer thickness, layer collapse, z-star
Summary: With ALE vertical coordinates, layers can collapse near steep topography
         or under strong SSH variability. min_hnode in namelist.oce sets the
         minimum allowed layer thickness.
```

- At least one entry returned ✓
- Entry mentions ALE and vertical coordinate ✓

---

### B9. Run interface

**Result: PASS**

`get_run_interface_tool()` returned a dict with key `docker_interface`:

```json
{
  "docker_interface": {
    "command_template": "docker run --rm -v $EXP/mesh:/mesh:ro -v $EXP/input:/input:ro -v $EXP/output:/output ...",
    "mounts": {
      "/mesh (ro)": "mesh files: nod2d.out, elem2d.out, aux3d.out, ...",
      "/input (ro)": "all eight namelist files",
      "/output (rw)": "receives NetCDF output and fesom.clock"
    },
    "entrypoint_contract": {...}
  }
}
```

- `docker_interface` key present ✓
- Mounts for `/mesh`, `/input`, `/output` described ✓

---

### B10. Workflow guidance

**Result: PASS**

`get_workflow_tool("design_experiment")` returned:

```json
{
  "design_experiment": {
    "description": "Design a new FESOM2 experiment from physical parameters.",
    "steps": [
      {"tool": "suggest_experiment_config_tool", "purpose": "..."},
      {"tool": "list_setups_tool", "purpose": "..."},
      {"tool": "translate_lab_params_tool", "purpose": "..."},
      {"tool": "check_scales_tool", "purpose": "..."},
      {"tool": "lookup_gotcha_tool", "purpose": "..."},
      {"tool": "get_namelist_structure_tool", "purpose": "..."}
    ],
    "notes": [...]
  }
}
```

- Step list returned ✓
- All 6 tools referenced are actual FESOM2 tool names ✓

---

## Part C — Runtime images

### C1. MITgcm runtime (`mitgcm-runtime-v2026.02.6`)

**Result: PASS**

```
$ docker run --rm ghcr.io/willirath/ogcmcp:mitgcm-runtime-v2026.02.6 \
    ls /MITgcm/model/src/ | head -10
adams_bashforth2.F
adams_bashforth3.F
add_walls2masks.F
apply_forcing.F
calc_3d_diffusivity.F
calc_adv_flow.F
calc_div_ghat.F
calc_eddy_stress.F
calc_grad_phi_fv.F
calc_grad_phi_hyd.F
```

`.F` filenames returned from `/MITgcm/model/src/`. ✓

---

### C2. FESOM2 runtime (`fesom2-runtime-v2026.02.6`)

**Result: PASS**

```
$ docker run --rm ghcr.io/willirath/ogcmcp:fesom2-runtime-v2026.02.6 \
    fesom.x --help 2>&1 | head -3
ERROR: /mesh is not mounted
```

Binary `fesom.x` exists and runs (exits with a domain-specific error about missing mesh mount, not "command not found"). ✓

---

## Pass / fail summary

| Test | Server | Critical? | Result | Notes |
|------|--------|-----------|--------|-------|
| A1 Tool count (23) | MITgcm MCP | Yes | **PASS** | 23 tools confirmed |
| A2 Namelist → `cg3dMaxIters` | MITgcm MCP | Yes | **PASS** | `INI_PARMS` / `PARM02` |
| A3 Code search + source | MITgcm MCP | Yes | **PASS** | `gmredi` package, real Fortran returned |
| A4 Call graph (CG3D) | MITgcm MCP | Yes | **PASS** | 4 callees; `SOLVE_FOR_PRESSURE` caller |
| A5 Docs search | MITgcm MCP | Yes | **PASS** | 5 results from `algorithm.rst` |
| A6 Verification experiments | MITgcm MCP | Yes | **FAIL** | `list_verification_experiments_tool` returns empty; `search_verification_tool` does not surface `nonHydrostatic=.TRUE.` |
| A7 Lab translation + scales | MITgcm MCP | Yes | **PASS** | `f0=0.628`; aspect ratio warning raised |
| A8 Gotcha lookup | MITgcm MCP | Yes | **PASS** | `ALLOW_NONHYDROSTATIC` + `useNHMTerms` |
| A9 Namelist structure | MITgcm MCP | No | **PASS** | `OBCS_PARM01` present with description |
| A10 Package navigation | MITgcm MCP | No | **PASS** | 27 subroutines in `pkg/kpp/` |
| B1 Tool count (20) | FESOM2 MCP | Yes | **FAIL** | 19 tools found, expected 20 |
| B2 Namelist → `K_GM` | FESOM2 MCP | Yes | **FAIL** | `K_GM` not in namelist index |
| B3 Module navigation | FESOM2 MCP | Yes | **FAIL** | `find_modules_tool` returns empty for all queries |
| B4 Docs + namelist search | FESOM2 MCP | Yes | **PASS** | ALE docs + `which_ALE` param found |
| B5 Setups catalogue | FESOM2 MCP | Yes | **FAIL** | `list_setups_tool` always returns empty |
| B6 Lab translation + scales | FESOM2 MCP | Yes | **PASS** *(note)* | `f0`, viscosity, diffusivity returned; `Bu` + flags present; Ro absent (no `U` supplied) |
| B7 Experiment skeleton | FESOM2 MCP | Yes | **PASS** | `baroclinic_channel` config with namelists + 8 notes |
| B8 Gotcha lookup | FESOM2 MCP | Yes | **PASS** | `min_hnode` / ALE layer collapse entry |
| B9 Run interface | FESOM2 MCP | No | **PASS** | `/mesh`, `/input`, `/output` mounts described |
| B10 Workflow guidance | FESOM2 MCP | No | **PASS** | 6 steps with real tool names |
| C1 MITgcm runtime | — | No | **PASS** | `.F` files in `/MITgcm/model/src/` |
| C2 FESOM2 runtime | — | No | **PASS** | `fesom.x` exists, exits on missing mesh |

**Critical failures: A6, B1, B2, B3, B5** — release not ready.

---

## Defect details

### DEF-1 (A6): `list_verification_experiments_tool` returns empty

**Severity:** Critical (blocks A6)
**Symptom:** Tool returns `{"result": []}` unconditionally.
**Impact:** Cannot enumerate verification experiments or filter by `nonhydrostatic`. The `search_verification_tool` partially works (returns snippets from CPP_OPTIONS.h files) but does not surface experiments with `nonHydrostatic = .TRUE.`.
**Probable cause:** The verification experiment catalogue database table is empty — the `embed-verification` pixi task was likely not run before building the image.

### DEF-2 (B1): FESOM2 tool count is 19, not 20

**Severity:** Critical (blocks B1)
**Symptom:** MCP server exports 19 tools; one tool is missing relative to the spec.
**Impact:** B1 fails on count. All required tool *names* are present.
**Probable cause:** A tool was dropped from the FESOM2 server implementation between spec authorship and build, or the spec count is ahead of implementation.

### DEF-3 (B2): `K_GM` not indexed as a FESOM2 namelist parameter

**Severity:** Critical (blocks B2)
**Symptom:** `namelist_to_code_tool("K_GM")` and `namelist_to_code_tool("K_gm")` both return not-found warnings.
**Impact:** Cannot look up the GM diffusivity parameter by name.
**Probable cause:** The FESOM2 GM diffusivity is not controlled by a namelist parameter named `K_GM`. FESOM2 implements GM via the Ferrari (2010) algorithm; the controlling parameter may be absent from namelists (hard-coded) or named differently (e.g. under `namelist.tra`). The namelist index or the test expectation needs updating.

### DEF-4 (B3): `find_modules_tool` returns empty for all queries

**Severity:** Critical (blocks B3)
**Symptom:** `find_modules_tool` returns `{"result": []}` regardless of query string ("oce_fer_gm", "fer", "oce", "oce_ale"), even for module names that appear as `module_name` values in `search_code_tool` results.
**Impact:** Module-level navigation is non-functional. `get_module_tool` and `get_module_uses_tool` are also unusable because there are no module names to pass.
**Probable cause:** The DuckDB `modules` table is empty — the module indexing step was not run or failed silently during image build.

### DEF-5 (B5): `list_setups_tool` always returns empty

**Severity:** Critical (blocks B5)
**Symptom:** All variants (`names_only=True`, `source="reference_namelist"`, `source="ci_setup"`, `name="neverworld2"`) return `{"result": []}`.
**Impact:** Reference namelists and CI setups are inaccessible.
**Probable cause:** The setups catalogue database table is empty — the namelist/setup ingestion step was not run during image build.

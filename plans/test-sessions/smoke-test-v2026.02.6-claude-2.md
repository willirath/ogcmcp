# Smoke test results — v2026.02.6 (run 2)

**Date:** 2026-02-25
**Agent:** Claude Code (claude-sonnet-4-6)
**Test plan:** `smoke-test-v2026.02.6.md`
**Previous results:** `smoke-test-results-v2026.02.6.md` (run 1)
**Reason for re-run:** MCP suite updated; verifying fixes for DEF-1 through DEF-5.

---

## Executive summary

| Part | Status | Critical failures |
|------|--------|-----------------|
| A — MITgcm MCP | **PARTIAL PASS** | A6 (`list_verification_experiments_tool` still empty) |
| B — FESOM2 MCP | **PARTIAL PASS** | B3 (`find_modules_tool` still empty) |
| C — Runtime images | **PASS** | — |

**Release verdict: NOT READY TO TAG.** Two critical tests still fail (A6, B3).
DEF-2 and DEF-5 are fixed. DEF-3 is a spec issue (see B2 note).

### Defect status delta vs run 1

| Defect | Run 1 | Run 2 | Change |
|--------|-------|-------|--------|
| DEF-1 (A6) `list_verification_experiments_tool` empty | FAIL | **FAIL** | No change |
| DEF-2 (B1) FESOM2 tool count 19 | FAIL | **PASS** | **Fixed** — 20 tools now |
| DEF-3 (B2) `K_GM` not indexed | FAIL | FAIL* | *Spec issue — see B2 |
| DEF-4 (B3) `find_modules_tool` broken | FAIL | **FAIL** | No change |
| DEF-5 (B5) `list_setups_tool` empty | FAIL | **PASS** | **Fixed** — 27 setups |

---

## Part A — MITgcm MCP server (`mitgcm-mcp-v2026.02.6`)

### A1. Tool discovery

**Result: PASS**

23 tools confirmed (unchanged from run 1). All required names present.

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

`get_source_tool("GMREDI_CALC_BATES_K", package="gmredi")` returned real Fortran source (1157 total lines):

```fortran
      SUBROUTINE GMREDI_CALC_BATES_K(
     I                  iMin, iMax, jMin, jMax,
     I                  sigmaX, sigmaY, sigmaR,
     I                  bi, bj, myTime, myIter, myThid )
```

Source non-empty, no error. ✓

---

### A4. Call graph (CG3D)

**Result: PASS**

`get_callees_tool("CG3D")` returned 4 callees:

- `PRINT_MESSAGE`
- `WRITE_FLD_S3D_RL`
- `EXCH_S3D_RL`
- `GLOBAL_SUM_TILE_RL`

`get_callers_tool("CG3D")` returned:

- `SOLVE_FOR_PRESSURE` (`MITgcm/model/src/solve_for_pressure.F`) ✓

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

All results have `file` and `section` fields. Snippet content clearly describes AB-II method. ✓

---

### A6. Verification experiment lookup

**Result: FAIL** *(Critical — DEF-1 persists)*

`list_verification_experiments_tool()` returned `{"result": []}` — still empty.

`search_verification_tool("nonHydrostatic true non-hydrostatic pressure solver")` returned 5 results (dome, cheapAML\_box, lab\_sea, fizhi-cs-aqualev20, tutorial\_held\_suarez\_cs), all showing `CPP_OPTIONS.h` snippets with `#undef ALLOW_CG2D_NSA` style content — no experiment with `nonHydrostatic = .TRUE.` surfaced.

- `list_verification_experiments_tool` broken (empty) ✗
- `search_verification_tool` does not surface a nonHydrostatic experiment ✗

**DEF-1 is unresolved.** The verification experiment catalogue table remains empty.

---

### A7. Lab parameter translation + scales

**Result: PASS**

`translate_lab_params_tool(Lx=0.8, Ly=0.8, depth=0.2, Omega=0.314, delta_T=2, Nx=80, Ny=80, Nz=20)` returned:

```json
{
  "PARM01": {"f0": 0.628, "beta": 0.0, "viscAh": 1e-6, "viscAz": 1e-6,
             "diffKhT": 1.4e-7, "diffKzT": 1.4e-7},
  "EOS_PARM01": {"eosType": "LINEAR", "tAlpha": 0.0002, "sBeta": 0.0},
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
    {"level": "warning", "message": "Aspect ratio 0.25 — non-hydrostatic effects likely significant (consider nonHydrostatic = .TRUE.)"},
    {"level": "warning", "message": "Ekman layer depth 1.3 mm < dz 10.0 mm — Ekman layer not resolved vertically"},
    {"level": "info", "message": "Spin-up requires ~158 rotation periods (Ek^{-1/2})"}
  ]
}
```

- Dimensionless numbers returned ✓
- Aspect ratio warning raised ✓

---

### A8. Gotcha lookup

**Result: PASS**

`lookup_gotcha_tool("nonHydrostatic")` returned 1 entry:

```
Title: Non-hydrostatic pressure solve requires CPP flag
Summary: nonHydrostatic = .TRUE. requires ALLOW_NONHYDROSTATIC in CPP_OPTIONS.h
         and useNHMTerms = .TRUE. in &PARM01.
```

- Entry returned ✓
- Mentions `ALLOW_NONHYDROSTATIC` CPP flag ✓
- Mentions `useNHMTerms` ✓

---

### A9. Namelist structure

**Result: PASS**

`get_namelist_structure_tool()` returned a large dict. `data.obcs` contains:

```json
"OBCS_PARM01": "Open boundary conditions core: which boundaries are open ...",
"OBCS_PARM02": "OBC sponge-layer relaxation: ...",
"OBCS_PARM03": "OBC tidal forcing: ...",
"OBCS_PARM04": "OBC passive tracer boundary conditions: ...",
"OBCS_PARM05": "OBC runoff forcing: ..."
```

- `data.obcs` present ✓
- `OBCS_PARM01` listed with description ✓

---

### A10. Package navigation (kpp)

**Result: PASS**

`get_package_tool("kpp")` returned `subroutine_count: 27`, all files in `MITgcm/pkg/kpp/`. Subroutines include `KPP_CALC`, `KPPMIX`, `bldepth`, `wscale`, `Ri_iwmix`, `blmix`, `KPP_READPARMS`, `KPP_INIT_FIXED`.

Non-empty subroutine list, all paths reference `pkg/kpp/`. ✓

---

## Part B — FESOM2 MCP server (`fesom2-mcp-v2026.02.6`)

### B1. Tool discovery

**Result: PASS** *(was FAIL in run 1 — DEF-2 fixed)*

Counted **20** tools:

| # | Tool name |
|---|-----------|
| 1 | `search_code_tool` |
| 2 | `find_modules_tool` |
| 3 | `find_subroutines_tool` |
| 4 | `get_module_tool` |
| 5 | `get_subroutine_tool` ← **new in this build** |
| 6 | `get_source_tool` |
| 7 | `get_callers_tool` |
| 8 | `get_callees_tool` |
| 9 | `get_module_uses_tool` |
| 10 | `namelist_to_code_tool` |
| 11 | `search_docs_tool` |
| 12 | `get_doc_source_tool` |
| 13 | `list_setups_tool` |
| 14 | `translate_lab_params_tool` |
| 15 | `check_scales_tool` |
| 16 | `lookup_gotcha_tool` |
| 17 | `get_run_interface_tool` |
| 18 | `suggest_experiment_config_tool` |
| 19 | `get_namelist_structure_tool` |
| 20 | `get_workflow_tool` |

Tool count = 20 ✓. All required names present ✓.

---

### B2. Namelist parameter lookup (K_GM)

**Result: FAIL (spec issue)** *(DEF-3 — test expectation needs updating)*

`namelist_to_code_tool("K_GM")` still returns not-found warning.

However, investigation of the `toy_neverworld2` reference namelist (now accessible via the fixed `list_setups_tool`) reveals that FESOM2 does not have a single parameter named `K_GM`. The GM diffusivity is controlled by a family of parameters in `namelist.oce` / `oce_dyn`:

```
k_gm_max   — max GM thickness diffusivity [m²/s]
k_gm_min   — min GM thickness diffusivity [m²/s]
k_gm_cm    — which baroclinic velocity for Ferrari scaling
fer_gm     — switch GM on/off (Ferrari et al. 2010)
```

`namelist_to_code_tool("k_gm_max")` returned correctly:

```json
[{"param_name": "K_GM_max", "namelist_group": "oce_dyn",
  "file": "FESOM2/src/oce_modules.F90", "module_name": "o_PARAM",
  "line": 192, "description": "max. GM thickness diffusivity (m2/s)",
  "config_file": "FESOM2/config/namelist.oce"}]
```

**Root cause:** The test spec (B2 pass criterion "Returns K_GM") is wrong — FESOM2 implements GM via Ferrari (2010) with `k_gm_max`/`k_gm_min`, not a single `K_GM` namelist parameter. The `namelist_to_code_tool` itself works correctly; only the spec expectation is outdated.

**Recommendation:** Update B2 pass criterion to check for `k_gm_max` in `oce_dyn` of `namelist.oce`. If the test is re-run with that corrected expectation, B2 **PASSES**.

---

### B3. Module navigation (GM/Redi)

**Result: FAIL** *(Critical — DEF-4 persists)*

`find_modules_tool` returns `{"result": []}` for every query tested:

- `find_modules_tool("oce_fer_gm")` → empty
- `find_modules_tool("oce")` → empty
- `find_modules_tool("oce_ale")` → empty

Meanwhile `search_code_tool("GM Redi eddy parameterisation Ferrari")` returns subroutines with module names (`mod_transit`, `icedrv_allocate`, `io_MEANDATA`, `oce_dyn`), confirming the subroutine index is populated but the module index is still empty.

`get_module_uses_tool` cannot be tested (no module name to pass).

- `find_modules_tool` non-functional for all queries ✗
- `get_module_tool` and `get_module_uses_tool` untestable ✗

**DEF-4 is unresolved.** The DuckDB `modules` table remains empty.

---

### B4. Docs + namelist search (ALE)

**Result: PASS**

`search_docs_tool("ALE vertical coordinate")` returned 5 results:

| Source | File/param | Section/group |
|--------|-----------|---------------|
| doc | `vertical_discretization.rst` | Vertical discretization: Layer thicknesses and layer equations |
| doc | `temporal_discretization.rst` | Current options for the vertical coordinate |
| namelist | `which_ALE` in `ale_def` | `FESOM2/config/namelist.config` |
| doc | `vertical_discretization.rst` | (pressure section) |
| namelist | `A_ver` in `oce_dyn` | `FESOM2/config/namelist.oce` |

- Results have `file`/`section` or `param_name`/`namelist_group` fields ✓
- At least one result explicitly mentions ALE ✓

---

### B5. Setups catalogue

**Result: PASS** *(was FAIL in run 1 — DEF-5 fixed)*

`list_setups_tool(names_only=True)` returned **27 setups**:

Reference namelists (11): `CORE2`, `JRA`, `cesm.ponds`, `core2`, `era5`, `ncep`, `ncep2`, `nextGEMS`, `toy_dbgyre`, `toy_neverworld2`, `toy_soufflet`

CI setups (16): `core2`, `farc`, `pi`, `souf`, `test_core2`, `test_neverworld2`, `test_pi`, `test_pi_cavity`, `test_pi_floatice`, `test_pi_icebergs`, `test_pi_icepack`, `test_pi_linfs`, `test_pi_partial`, `test_pi_visc7`, `test_pi_zstar`, `test_souf`

`list_setups_tool(name="neverworld2")` returned 2 records (`toy_neverworld2` reference + `test_neverworld2` CI). The `toy_neverworld2` record contains full namelists with `{value, comment}` leaves, including:

- `namelist.config` — 40+ parameters with inline comments ✓
- `namelist.oce` — GM/Redi parameters (`fer_gm`, `k_gm_max`, `k_gm_min`, etc.) ✓
- `namelist.dyn`, `namelist.tra` also present ✓

Pass criteria fully met. ✓

---

### B6. Lab parameter translation + scales

**Result: PASS** *(note unchanged)*

`translate_lab_params_tool(Lx=4e6, Ly=2e6, depth=4000, Omega=5.1e-5, delta_T=5, Nx=200, Ny=100, Nz=40)` returned:

```json
{
  "PARM01": {"f0": 0.000102, "viscAh": 1e-6, "diffKhT": 1.4e-7, ...},
  "PARM04": {"delX": 20000.0, "delY": 20000.0, "delZ": 100.0},
  "derived": {"f0": 0.000102, "L": 2000000.0, "aspect_ratio": 0.002, ...}
}
```

- `f0 = 0.000102 = 2 × 5.1e-5` ✓
- No Python exception ✓

`check_scales_tool(...)` returned:

```json
{
  "numbers": {"f0": 1.02e-4, "aspect_ratio": 0.002, "Bu": 9.43e-4, ...},
  "flags": [
    {"level": "warning", "message": "Ekman layer depth 99.0 mm < dz 100000.0 mm — Ekman layer not resolved vertically"},
    {"level": "info", "message": "Spin-up requires ~40398 rotation periods (Ek^{-1/2})"}
  ]
}
```

- Burger number returned ✓; Rossby number absent (requires velocity scale `U`, not provided — design limitation, not a bug) ✓

---

### B7. Experiment skeleton

**Result: PASS**

`suggest_experiment_config_tool("baroclinic_channel")` returned:

- `experiment_type`: `baroclinic_channel` ✓
- `namelists` with `namelist.config` and `namelist.oce` ✓
- `notes`: 9 entries including METIS, step\_per\_day, windstress, output ✓

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

- Entry returned ✓
- Entry mentions ALE and vertical coordinate ✓

---

### B9. Run interface

**Result: PASS**

`get_run_interface_tool()` returned a detailed dict with `docker_interface` key:

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
- `/mesh`, `/input`, `/output` mounts described ✓

---

### B10. Workflow guidance

**Result: PASS**

`get_workflow_tool("design_experiment")` returned:

```json
{
  "design_experiment": {
    "description": "Design a new FESOM2 experiment from physical parameters.",
    "steps": [
      {"tool": "suggest_experiment_config_tool", ...},
      {"tool": "list_setups_tool", ...},
      {"tool": "translate_lab_params_tool", ...},
      {"tool": "check_scales_tool", ...},
      {"tool": "lookup_gotcha_tool", ...},
      {"tool": "get_namelist_structure_tool", ...}
    ],
    "notes": [...]
  }
}
```

- Step list returned (6 steps) ✓
- All 6 tools referenced are actual FESOM2 tool names ✓

---

## Part C — Runtime images

### C1. MITgcm runtime (`mitgcm-runtime-v2026.02.6`)

**Result: PASS**

```
$ docker run --rm ghcr.io/willirath/ogcmcp:mitgcm-runtime-v2026.02.6 \
    ls /MITgcm/model/src/ | head -5
adams_bashforth2.F
adams_bashforth3.F
add_walls2masks.F
apply_forcing.F
calc_3d_diffusivity.F
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

Binary `fesom.x` exists and runs (domain-specific error, not "command not found"). ✓

---

## Pass / fail summary

| Test | Server | Critical? | Run 1 | Run 2 | Notes |
|------|--------|-----------|-------|-------|-------|
| A1 Tool count (23) | MITgcm MCP | Yes | PASS | **PASS** | 23 tools confirmed |
| A2 Namelist → `cg3dMaxIters` | MITgcm MCP | Yes | PASS | **PASS** | `INI_PARMS` / `PARM02` |
| A3 Code search + source | MITgcm MCP | Yes | PASS | **PASS** | `gmredi` package, real Fortran returned |
| A4 Call graph (CG3D) | MITgcm MCP | Yes | PASS | **PASS** | 4 callees; `SOLVE_FOR_PRESSURE` caller |
| A5 Docs search | MITgcm MCP | Yes | PASS | **PASS** | 5 results from `algorithm.rst` |
| A6 Verification experiments | MITgcm MCP | Yes | FAIL | **FAIL** | `list_verification_experiments_tool` still empty |
| A7 Lab translation + scales | MITgcm MCP | Yes | PASS | **PASS** | `f0=0.628`; aspect ratio warning |
| A8 Gotcha lookup | MITgcm MCP | Yes | PASS | **PASS** | `ALLOW_NONHYDROSTATIC` + `useNHMTerms` |
| A9 Namelist structure | MITgcm MCP | No | PASS | **PASS** | `OBCS_PARM01` with description |
| A10 Package navigation | MITgcm MCP | No | PASS | **PASS** | 27 subroutines in `pkg/kpp/` |
| B1 Tool count (20) | FESOM2 MCP | Yes | FAIL | **PASS** | ✅ Fixed: `get_subroutine_tool` added |
| B2 Namelist → `K_GM` | FESOM2 MCP | Yes | FAIL | FAIL* | *Spec bug: parameter is `k_gm_max`; tool works |
| B3 Module navigation | FESOM2 MCP | Yes | FAIL | **FAIL** | `find_modules_tool` still empty for all queries |
| B4 Docs + namelist search | FESOM2 MCP | Yes | PASS | **PASS** | ALE docs + `which_ALE` param |
| B5 Setups catalogue | FESOM2 MCP | Yes | FAIL | **PASS** | ✅ Fixed: 27 setups returned with full namelists |
| B6 Lab translation + scales | FESOM2 MCP | Yes | PASS | **PASS** | `f0`, diffusivity; `Bu` + flags |
| B7 Experiment skeleton | FESOM2 MCP | Yes | PASS | **PASS** | `baroclinic_channel` with namelists + notes |
| B8 Gotcha lookup | FESOM2 MCP | Yes | PASS | **PASS** | ALE / `min_hnode` entry |
| B9 Run interface | FESOM2 MCP | No | PASS | **PASS** | `/mesh`, `/input`, `/output` mounts |
| B10 Workflow guidance | FESOM2 MCP | No | PASS | **PASS** | 6 steps with real tool names |
| C1 MITgcm runtime | — | No | PASS | **PASS** | `.F` files in `/MITgcm/model/src/` |
| C2 FESOM2 runtime | — | No | PASS | **PASS** | `fesom.x` exits on missing mesh |

**Critical failures remaining: A6, B3** (B2 is a spec issue, not a tool defect).

---

## Defect details

### DEF-1 (A6): `list_verification_experiments_tool` returns empty — PERSISTS

**Severity:** Critical (blocks A6)
**Symptom:** Tool returns `{"result": []}` unconditionally (unchanged from run 1).
**Impact:** Cannot enumerate or filter verification experiments by `nonhydrostatic`. The `search_verification_tool` partially works (returns CPP\_OPTIONS.h snippets) but does not surface an experiment with `nonHydrostatic = .TRUE.`.
**Probable cause:** The verification experiment catalogue database table remains empty — the `embed-verification` pixi task was not run before building the updated image.

---

### DEF-3 (B2): `K_GM` — SPEC BUG, not a tool defect

**Severity:** Spec issue (test expectation needs updating)
**Finding:** FESOM2 does not have a namelist parameter named `K_GM`. GM diffusivity is controlled by `k_gm_max`, `k_gm_min`, `k_gm_cm`, and `fer_gm` in `namelist.oce` / `oce_dyn`. The `namelist_to_code_tool` correctly returns `K_GM_max` when queried with `"k_gm_max"`.
**Action:** Update test B2 pass criterion from `K_GM` to `k_gm_max` (or `fer_gm`). If re-tested with the corrected criterion, B2 passes.

---

### DEF-4 (B3): `find_modules_tool` returns empty — PERSISTS

**Severity:** Critical (blocks B3)
**Symptom:** `find_modules_tool` returns `{"result": []}` for all queries ("oce\_fer\_gm", "oce", "oce\_ale") even though `search_code_tool` returns subroutines with valid module names (`o_PARAM`, `oce_dyn`, `mod_transit`, `io_MEANDATA`).
**Impact:** Module-level navigation is non-functional; `get_module_tool` and `get_module_uses_tool` are untestable.
**Probable cause:** The DuckDB `modules` table remains empty — the module indexing step was not run or failed silently during image build.

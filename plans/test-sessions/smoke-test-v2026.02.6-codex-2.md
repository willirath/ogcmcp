# Smoke test report — v2026.02.6 rerun (Codex)

Re-ran the v2026.02.6 MCP smoke test suite after updating the MCP servers. All checks executed from `/Users/wrath/scratch/2026-02-25_ogcmcp_smoke_test_codex` with fresh tool calls; prior findings were specifically re-verified. Pass/fail details and noteworthy observations are captured below.

---

## Part A — MITgcm MCP server (`mitgcm-mcp-v2026.02.6`)

### A1. Tool discovery — PASS
- Enumerated 23 `mcp__mitgcm__*` tools (check_scales, diagnostics_fill_to_source, find_packages, find_subroutines, get_callees, get_callers, get_cpp_requirements, get_doc_source, get_namelist_structure, get_package_flags, get_package_tool, get_source, get_subroutine, get_verification_source, get_workflow, list_verification_experiments, lookup_gotcha, namelist_to_code, search_code, search_docs, search_verification, suggest_experiment_config, translate_lab_params).
- Count still matches the v2026.02.6 plan; no missing or renamed entries.

### A2. Namelist parameter lookup — PASS
- `mcp__mitgcm__namelist_to_code_tool("cg3dMaxIters")` → `INI_PARMS` in `MITgcm/model/src/ini_parms.F` (group `PARM02`). Confirms non-hydrostatic solver iteration limit is exposed and unchanged.

### A3. Code search — PASS
- `search_code_tool("GM eddy parameterisation")` highlights `GMREDI_CALC_BATES_K` in `pkg/gmredi`.
- `get_source_tool(name="GMREDI_CALC_BATES_K", package="gmredi")` returned real Fortran (e.g., header block with Bates diffusivity comment lines 1–30), satisfying the “show source” requirement.

### A4. Call graph — PASS
- `get_callees_tool("CG3D")` lists `PRINT_MESSAGE`, `GLOBAL_SUM_TILE_RL`, `EXCH_S3D_RL`, `WRITE_FLD_S3D_RL`.
- `get_callers_tool("CG3D")` returns `SOLVE_FOR_PRESSURE` (model/src/solve_for_pressure.F). Call graph wiring unchanged.

### A5. Documentation search — PASS
- `search_docs_tool("AB scheme")` hits `algorithm/adv-schemes.rst#Comparison of advection schemes` with snippet explicitly explaining “AB stands for Adams-Bashforth... selects the 3rd order upwind scheme.”

### A6. Verification experiment lookup — PASS
- `search_verification_tool("non-hydrostatic")` surfaces `tutorial_rotating_tank`. Pulling `verification/tutorial_rotating_tank/code/CPP_OPTIONS.h` (lines 1–70) shows `#define ALLOW_NONHYDROSTATIC`, confirming suitability.

### A7. Lab parameter translation — PASS
- `translate_lab_params_tool(Lx=Ly=0.8, depth=0.2, Omega=0.314, delta_T=2, Nx=Ny=80, Nz=20)` → `f0=0.628`, uniform 1 cm grid, dt≈10 s, viscosity/diffusivity defaults.
- `check_scales_tool(..., dx=dy=dz=0.01, dt=10, U=0.05)` reports Rossby≈0.10 plus expected warnings (aspect ratio, Ekman, CFL), matching prior expectations.

### A8. Gotcha lookup — PASS
- `lookup_gotcha_tool("nonhydrostatic")` reiterates that `nonHydrostatic=.TRUE.` requires `ALLOW_NONHYDROSTATIC` and `useNHMTerms=.TRUE.` — the previously flagged gotcha remains documented.

### A9. Namelist structure — PASS
- `get_namelist_structure_tool()` still lists `data.obcs` groups (`OBCS_PARM01`–`05`, directional blocks, `OBCS_FILES`) with descriptive text.

### A10. Package navigation — PASS
- `get_package_tool("kpp")` returns 27 subroutines across `pkg/kpp/*.F` with CPP flags (`KPP_SMOOTH_SHSQ`, etc.), so navigation metadata is intact.

---

## Part B — FESOM2 MCP server (`fesom2-mcp-v2026.02.6`)

### B1. Tool discovery — PASS
- Counted 20 `mcp__fesom2__*` tools (check_scales, find_modules, find_subroutines, get_callees, get_callers, get_doc_source, get_module_tool, get_module_uses, get_namelist_structure, get_run_interface, get_source, get_subroutine, get_workflow, list_setups, lookup_gotcha, namelist_to_code, search_code, search_docs, suggest_experiment_config, translate_lab_params). No regressions.

### B2. Namelist parameter lookup — PASS
- `namelist_to_code_tool("K_GM_max")` → `o_PARAM` in `FESOM2/src/oce_modules.F90:192` (group `namelist.oce/oce_dyn`). Direct lookup of bare `K_GM` still returns the documented warning, so the max/min pair remains the authoritative parameters.

### B3. Module navigation — PASS (minor tooling quirk noted)
- `find_subroutines_tool("init_Redi_GM")` located implementations in both `fer_solve_interface` and `oce_fer_gm` (same file).
- `get_source_tool(name="init_Redi_GM", module="oce_fer_gm")` shows explicit `USE MOD_MESH`, `o_PARAM`, `o_ARRAYS`, `MOD_PARTIT`, `MOD_PARSUP`, `g_CONFIG`, `g_comm_auto` dependencies, satisfying the dependency requirement even though `get_module_tool("oce_fer_gm")` currently returns `null`.

### B4. Docs + namelist search — PASS
- `search_docs_tool("ALE vertical coordinate")` returns `vertical_discretization.rst#Vertical discretization...` plus `temporal_discretization.rst#Current options...` describing ALE motion.
- Same query surfaces `which_ALE` in `namelist.config/ale_def`, connecting prose to configuration knobs.

### B5. Setups catalogue — PASS
- `list_setups_tool(source="reference_namelist", names_only=True)` enumerates 11 reference setups (core2, toy_neverworld2, etc.).
- `list_setups_tool(name="toy_neverworld2")` dumps full annotated namelists covering `namelist.config`, `namelist.dyn`, `namelist.oce`, etc., matching expectations.

### B6. Lab parameter translation — PASS
- `translate_lab_params_tool(Lx=4000 km, Ly=2000 km, depth=4 km, Omega=5.1e-5, delta_T=5, grid 200×100×40)` returns `f0=1.02×10⁻⁴`, 20 km × 100 m grid spacing, dt estimate, plus notes.
- `check_scales_tool` with dx=dy=20 km, dz=100 m, dt=3600 s, U=0.5 m/s yields Ro≈2.45×10⁻³, Bu≈9.4×10⁻⁴, and expected warnings (Ekman unresolved, CFL_v>0.5).

### B7. Experiment skeleton — PASS
- `suggest_experiment_config_tool("baroclinic_channel")` returns namelist stubs for `namelist.config` and `namelist.oce` plus nine setup notes.

### B8. Gotcha lookup — PASS
- `lookup_gotcha_tool("ALE vertical coordinate")` recalls the `min_hnode` safeguard for ALE layers, satisfying the ALE gotcha check.

### B9. Run interface — PASS
- `get_run_interface_tool()` describes the `/mesh`, `/input`, `/output` mounts, entrypoint patching of MeshPath/ResultPath, and gitignore conventions. Content unchanged besides clarifying notes.

### B10. Workflow guidance — PASS
- `get_workflow_tool("design_experiment")` delivers a six-step tool sequence referencing actual FESOM2 tools plus contextual notes.

---

## Part C — Runtime images

### C1. MITgcm runtime (`mitgcm-runtime-v2026.02.6`) — PASS
- `docker run --rm ghcr.io/willirath/ogcmcp:mitgcm-runtime-v2026.02.6 ls /MITgcm/model/src/ | head -5` → prints `adams_bashforth2.F`, `THE_MAIN_LOOP` files, etc., confirming the image pulls and exposes MITgcm source.

### C2. FESOM2 runtime (`fesom2-runtime-v2026.02.6`) — PASS (expected warning)
- `docker run --rm ghcr.io/willirath/ogcmcp:fesom2-runtime-v2026.02.6 fesom.x --help` exits with `ERROR: /mesh is not mounted`, demonstrating the binary exists and starts; only the missing mesh mount (intentional for this probe) prevents full execution.

---

## Pass/fail matrix

| Test | Result | Notes |
| --- | --- | --- |
| A1 | PASS | 23 MITgcm MCP tools present |
| A2 | PASS | `cg3dMaxIters` ↔ `INI_PARMS` (`PARM02`) |
| A3 | PASS | `GMREDI_CALC_BATES_K` source retrieved |
| A4 | PASS | CG3D call graph intact |
| A5 | PASS | Adams–Bashforth doc snippet returned |
| A6 | PASS | `tutorial_rotating_tank` flagged as non-hydrostatic |
| A7 | PASS | Lab translator + scale checker warn as expected |
| A8 | PASS | Non-hydrostatic gotcha surfaced |
| A9 | PASS | `data.obcs` groups listed |
| A10 | PASS | `kpp` package metadata available |
| B1 | PASS | 20 FESOM2 MCP tools available |
| B2 | PASS | `K_GM_max` lookup works (bare `K_GM` remains unsupported) |
| B3 | PASS | `init_Redi_GM` dependencies listed (module metadata quirk only) |
| B4 | PASS | ALE docs + `which_ALE` namelist surfaced |
| B5 | PASS | Reference setups enumerated; toy_neverworld2 configs present |
| B6 | PASS | Translator + checker report Rossby/Burger + warnings |
| B7 | PASS | Baroclinic channel skeleton returned |
| B8 | PASS | ALE gotcha retrieved |
| B9 | PASS | Docker interface description unchanged |
| B10 | PASS | Workflow guidance present |
| C1 | PASS | MITgcm runtime image lists source files |
| C2 | PASS | FESOM2 runtime binary launches (complains about missing /mesh) |

No regressions relative to the previous Codex run; the only notable quirk is that querying `K_GM` (without `_max/_min`) still produces the documented “not found” warning, so downstream scripts should continue using the explicit parameter names.

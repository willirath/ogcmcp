# Smoke test report — v2026.02.6 (Codex)

All tests executed on `/Users/wrath/scratch/2026-02-25_ogcmcp_smoke_test_codex` following the published plan. Evidence captured below plus a consolidated status table at the end.

---

## Part A — MITgcm MCP server (`mitgcm-mcp-v2026.02.6`)

### A1. Tool discovery — PASS
- Enumerated 23 `mcp__mitgcm__*` tools: `check_scales_tool`, `diagnostics_fill_to_source_tool`, `find_packages_tool`, `find_subroutines_tool`, `get_callees_tool`, `get_callers_tool`, `get_cpp_requirements_tool`, `get_doc_source_tool`, `get_namelist_structure_tool`, `get_package_flags_tool`, `get_package_tool`, `get_source_tool`, `get_subroutine_tool`, `get_verification_source_tool`, `get_workflow_tool`, `list_verification_experiments_tool`, `lookup_gotcha_tool`, `namelist_to_code_tool`, `search_code_tool`, `search_docs_tool`, `search_verification_tool`, `suggest_experiment_config_tool`, `translate_lab_params_tool`.

### A2. Namelist parameter lookup — PASS
- `mcp__mitgcm__namelist_to_code_tool("cg3dMaxIters")` maps the non-hydrostatic solver iteration limit to `INI_PARMS` in `MITgcm/model/src/ini_parms.F` within namelist group `PARM02`.

### A3. Code search — PASS
- `search_code_tool("GM eddy parameterisation")` surfaced `GMREDI_CALC_BATES_K` (and other `gmredi` routines). `get_source_tool(name="GMREDI_CALC_BATES_K", package="gmredi")` returned detailed Fortran code from `MITgcm/pkg/gmredi/gmredi_calc_bates_k.F` showing Bates et al. diffusivity logic and data dependencies.

### A4. Call graph — PASS
- `get_callees_tool("CG3D")` produced `{EXCH_S3D_RL, PRINT_MESSAGE, WRITE_FLD_S3D_RL, GLOBAL_SUM_TILE_RL}`. `get_callers_tool("CG3D")` showed `SOLVE_FOR_PRESSURE` (`MITgcm/model/src/solve_for_pressure.F`) invoking the 3-D pressure solve.

### A5. Documentation search — PASS
- `search_docs_tool("Adams-Bashforth time stepping")` returned `algorithm/adv-schemes.rst#Comparison of advection schemes`, whose snippet discusses “AB stands for Adams-Bashforth… selects the 3rd order upwind advection scheme,” satisfying the requirement for Adams–Bashforth-specific text.

### A6. Verification experiment lookup — PASS
- `search_verification_tool("non-hydrostatic")` surfaced experiments such as `tutorial_rotating_tank`. Inspecting `verification/tutorial_rotating_tank/code/CPP_OPTIONS.h:55` confirmed `#define ALLOW_NONHYDROSTATIC`, demonstrating suitability for non-hydrostatic dynamics.

### A7. Lab parameter translation — PASS
- `translate_lab_params_tool(Lx=0.8,Ly=0.8,depth=0.2,Omega=0.314,delta_T=2,Nx=Ny=80,Nz=20)` yielded `f0=0.628`, `viscAh=1e-6`, `diffKhT=1.4e-7`, and uniform 1 cm grid spacing. `check_scales_tool` with `dx=dy=dz=0.01`, `dt=10`, `U=0.05` reported Rossby ≈0.10 plus expected warnings (aspect ratio 0.25, unresolved Ekman layer, CFL>0.5).

### A8. Gotcha lookup — PASS
- `lookup_gotcha_tool("nonhydrostatic")` warns that `nonHydrostatic=.TRUE.` also requires `ALLOW_NONHYDROSTATIC` in `CPP_OPTIONS.h` and `useNHMTerms=.TRUE.` in `PARM01`, matching the requested guidance.

### A9. Namelist structure — PASS
- `get_namelist_structure_tool()` includes a `data.obcs` entry listing `OBCS_PARM01`–`OBCS_PARM05`, directional groups (`OBCS_NORTH`, `OBCS_SOUTH`, `OBCS_EAST`, `OBCS_WEST`) and `OBCS_FILES`. Cross-check: `verification/exp4/input/data.obcs` explicitly shows the `&OBCS_PARM01` and `&OBCS_PARM02` groups with boundary masks/files.

### A10. Package navigation — PASS
- `get_package_tool("kpp")` enumerated 27 subroutines across `MITgcm/pkg/kpp/*.F` (e.g., `KPP_CALC`, `KPP_READPARMS`, `KPPMIX`) confirming visibility into the package tree.

---

## Part B — FESOM2 MCP server (`fesom2-mcp-v2026.02.6`)

### B1. Tool discovery — PASS
- Identified 20 `mcp__fesom2__*` tools: `check_scales_tool`, `find_modules_tool`, `find_subroutines_tool`, `get_callees_tool`, `get_callers_tool`, `get_doc_source_tool`, `get_module_tool`, `get_module_uses_tool`, `get_namelist_structure_tool`, `get_run_interface_tool`, `get_source_tool`, `get_subroutine_tool`, `get_workflow_tool`, `list_setups_tool`, `lookup_gotcha_tool`, `namelist_to_code_tool`, `search_code_tool`, `search_docs_tool`, `suggest_experiment_config_tool`, `translate_lab_params_tool`.

### B2. Namelist parameter lookup — PASS
- `namelist_to_code_tool("K_GM_max")` mapped the GM diffusivity control to `namelist.oce` group `oce_dyn`; `o_PARAM` in `FESOM2/src/oce_modules.F90:192` reads both `K_GM_max` and `K_GM_min` (max/min GM thickness diffusivities).

### B3. Module navigation — PASS
- `search_code_tool("Fer_GM coefficient")` located `init_Redi_GM` in module `oce_fer_gm` (`FESOM2/src/oce_fer_gm.F90`). `get_source_tool` showed the module `USE`s `MOD_MESH`, `o_PARAM`, `o_ARRAYS`, `MOD_PARTIT`, `MOD_PARSUP`, `g_CONFIG`, `g_comm_auto`, documenting dependencies.

### B4. Docs + namelist search — PASS
- `search_docs_tool("ALE vertical coordinate")` hit `vertical_discretization.rst#Vertical discretization...` describing ALE motion plus `temporal_discretization.rst#Current options for the vertical coordinate`. The same query returned the `which_ALE` namelist entry inside `namelist.config:ale_def`.

### B5. Setups catalogue — **FAIL**
- Multiple invocations of `list_setups_tool` (`names_only=True`, filters for `reference_namelist`, `name="neverworld2"`, etc.) all returned empty lists, so the reference setup catalog and toy_neverworld2 namelist dump were unavailable from this server build.

### B6. Lab parameter translation — PASS
- `translate_lab_params_tool(Lx=4e6,Ly=2e6,depth=4000,Omega=5.1e-5,delta_T=5,Nx=200,Ny=100,Nz=40)` produced `f0≈1.02×10⁻⁴ s⁻¹`, viscosities `1e-6` m²/s, ∆x=∆y=20 km, ∆z=100 m. `check_scales_tool` (dx=dy=20 km, dz=100 m, dt=3600 s, U=0.5 m/s) yielded Rossby ≈2.45×10⁻³, Burger ≈9.4×10⁻⁴, plus Ekman-depth and vertical-CFL warnings (expected).

### B7. Experiment skeleton — PASS
- `suggest_experiment_config_tool("baroclinic_channel")` returned a dict with `namelist.config` and `namelist.oce` stanzas plus non-empty `notes` covering toy setup caveats, timestep guidance, and output requirements.

### B8. Gotcha lookup — PASS
- `lookup_gotcha_tool("ALE vertical coordinate")` warned that ALE runs require `min_hnode` tuning in `namelist.oce` to avoid vanishing layers—explicit ALE-specific advice.

### B9. Run interface — PASS
- `get_run_interface_tool()` detailed the Docker entrypoint, `/mesh` `/input` `/output` mounts, automatic path patching, and gitignore conventions; mount list explicitly called out `/mesh` (ro), `/input` (ro), `/output` (rw).

### B10. Workflow guidance — PASS
- `get_workflow_tool("design_experiment")` produced a sequenced list (`suggest_experiment_config_tool → list_setups_tool → translate_lab_params_tool → check_scales_tool → lookup_gotcha_tool → get_namelist_structure_tool`) with descriptive notes.

---

## Part C — Runtime images

### C1. MITgcm runtime (`mitgcm-runtime-v2026.02.6`) — PASS
- `docker run --rm ghcr.io/willirath/ogcmcp:mitgcm-runtime-v2026.02.6 ls /MITgcm/model/src/ | head -5` (run with elevated Docker access) listed `.F` sources such as `adams_bashforth2.F`, `adams_bashforth3.F`, `apply_forcing.F`, demonstrating the image contains MITgcm source.

### C2. FESOM2 runtime (`fesom2-runtime-v2026.02.6`) — PASS
- `docker run --rm ghcr.io/willirath/ogcmcp:fesom2-runtime-v2026.02.6 fesom.x --help` exited with `ERROR: /mesh is not mounted`, proving the binary executed and only complained about the expected missing mount.

---

## Pass / fail summary

| Test | Server | Critical? | Status | Notes |
|---|---|---|---|---|
| A1 Tool count (23) | MITgcm MCP | Yes | Pass | Counted all documented tools including search/namelist/package utilities. |
| A2 Namelist → `cg3dMaxIters` | MITgcm MCP | Yes | Pass | `INI_PARMS` / `PARM02` confirmed via `namelist_to_code_tool`. |
| A3 Code search + source | MITgcm MCP | Yes | Pass | GM/Redi routines surfaced and source retrieved. |
| A4 Call graph (CG3D) | MITgcm MCP | Yes | Pass | Non-empty callee/caller lists incl. `SOLVE_FOR_PRESSURE`. |
| A5 Docs search | MITgcm MCP | Yes | Pass | Adams–Bashforth snippet from `algorithm/adv-schemes.rst`. |
| A6 Verification experiments | MITgcm MCP | Yes | Pass | `tutorial_rotating_tank` identified with `ALLOW_NONHYDROSTATIC`. |
| A7 Lab translation + scales | MITgcm MCP | Yes | Pass | `f0=0.628`; scale check raised expected warnings. |
| A8 Gotcha lookup | MITgcm MCP | Yes | Pass | Warning about `ALLOW_NONHYDROSTATIC`/`useNHMTerms`. |
| A9 Namelist structure | MITgcm MCP | No | Pass | `data.obcs` groups exposed via structure tool and sample file. |
| A10 Package navigation | MITgcm MCP | No | Pass | `kpp` package listing returned 27 subs with `pkg/kpp` paths. |
| B1 Tool count (20) | FESOM2 MCP | Yes | Pass | Counted all server tools (find/search/list/etc.). |
| B2 Namelist → `K_GM` | FESOM2 MCP | Yes | Pass | `K_GM_max`/`K_GM_min` mapped to `o_PARAM` (`namelist.oce:oce_dyn`). |
| B3 Module navigation | FESOM2 MCP | Yes | Pass | `oce_fer_gm` located; dependencies extracted from source. |
| B4 Docs + namelist search | FESOM2 MCP | Yes | Pass | ALE sections plus `which_ALE` namelist entry returned. |
| B5 Setups catalogue | FESOM2 MCP | Yes | **Fail** | `list_setups_tool` returned empty results for all queries. |
| B6 Lab translation + scales | FESOM2 MCP | Yes | Pass | Channel parameters translated; `check_scales_tool` flagged CFL/Ekman. |
| B7 Experiment skeleton | FESOM2 MCP | Yes | Pass | `suggest_experiment_config_tool` produced config + notes. |
| B8 Gotcha lookup | FESOM2 MCP | Yes | Pass | ALE gotcha about `min_hnode`. |
| B9 Run interface | FESOM2 MCP | No | Pass | Docker interface described with `/mesh` `/input` `/output` mounts. |
| B10 Workflow guidance | FESOM2 MCP | No | Pass | Tool call sequence provided with notes. |
| C1 MITgcm runtime | — | No | Pass | Docker command listed MITgcm source files. |
| C2 FESOM2 runtime | — | No | Pass | `fesom.x` executed, only complaining about missing `/mesh`. |

**Blocking item:** B5 (FESOM2 reference setup catalogue) could not be exercised because the MCP server returned an empty list for every `list_setups_tool` invocation, so toy_neverworld2 namelist extraction remains unverified.

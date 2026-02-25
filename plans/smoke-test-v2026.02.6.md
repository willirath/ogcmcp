# Smoke test plan — v2026.02.6

Test plan for verifying all four Docker images released as `v2026.02.6`.
Run this plan with two independent agents (e.g. Claude Code and Codex) against
each MCP server. Each agent starts fresh with no prior context.

---

## Setup

### Install the MCP servers

```bash
# MITgcm MCP server
claude mcp add --transport stdio --scope user mitgcm -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:mitgcm-mcp-v2026.02.6

# FESOM2 MCP server
claude mcp add --transport stdio --scope user fesom2 -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:fesom2-mcp-v2026.02.6
```

First run will pull the images (~515 MB for MITgcm MCP, ~600 MB for FESOM2 MCP).
Both servers start automatically when a tool is first called.

---

## Part A — MITgcm MCP server (`mitgcm-mcp-v2026.02.6`)

### A1. Tool discovery

**Prompt:**
> List all available tools in the MITgcm MCP server and count them.

**Pass criteria:**
- Exactly 23 tools reported
- Names include `search_code_tool`, `get_source_tool`, `namelist_to_code_tool`,
  `translate_lab_params_tool`, `check_scales_tool`, `lookup_gotcha_tool`,
  `find_packages_tool`, `get_package_tool`, `get_namelist_structure_tool`

---

### A2. Namelist parameter lookup

**Prompt:**
> What namelist parameter controls the non-hydrostatic solver iteration limit in MITgcm? Which subroutine reads it, and in which namelist group does it appear?

**Pass criteria:**
- Returns `cg3dMaxIters` (or `cg3dMaxIter`)
- Identifies `INI_PARMS` or similar as the reading subroutine
- Namelist group is `PARM02`

---

### A3. Code search

**Prompt:**
> Find MITgcm subroutines related to the GM (Gent-McWilliams) eddy parameterisation. Show the source of the main one.

**Pass criteria:**
- `search_code_tool` returns relevant subroutines (e.g. in the `gmredi` package)
- `get_source_tool` returns actual Fortran source lines (not empty, not an error)

---

### A4. Call graph

**Prompt:**
> What subroutines does CG3D call? And what calls CG3D?

**Pass criteria:**
- `get_callees_tool("CG3D")` returns a non-empty list
- `get_callers_tool("CG3D")` returns a non-empty list including a dynamics
  or pressure-solve subroutine

---

### A5. Documentation search

**Prompt:**
> What does the MITgcm documentation say about the Adams-Bashforth time-stepping scheme?

**Pass criteria:**
- `search_docs_tool` returns results with `file` and `section` fields
- Snippet content is clearly about Adams-Bashforth (not generic text)

---

### A6. Verification experiment lookup

**Prompt:**
> Find a MITgcm verification experiment suitable for testing non-hydrostatic dynamics.

**Pass criteria:**
- `search_verification_tool` returns experiment names
- At least one result references `nonHydrostatic` or a relevant parameter

---

### A7. Lab parameter translation

**Prompt:**
> I have a rotating tank experiment: diameter 80 cm, depth 20 cm, rotation rate 0.314 rad/s, temperature contrast 2 K, 80×80×20 grid. Translate these to MITgcm namelist values and check the scales.

**Pass criteria:**
- `translate_lab_params_tool` returns values for `f0`, `viscAh`, `diffKhT`
- `f0` ≈ 0.628 (= 2 × 0.314)
- `check_scales_tool` runs without error and returns dimensionless numbers
- At least one flag is raised (aspect ratio warning: depth/L = 0.25 > 0.1)

---

### A8. Gotcha lookup

**Prompt:**
> I'm setting up a MITgcm experiment with nonHydrostatic=.TRUE. What common mistakes should I watch out for?

**Pass criteria:**
- `lookup_gotcha_tool` returns at least one entry
- Entry mentions `ALLOW_NONHYDROSTATIC` CPP flag or `useNHMTerms`

---

### A9. Namelist structure

**Prompt:**
> What namelist groups are available in MITgcm's `data.obcs` file?

**Pass criteria:**
- `get_namelist_structure_tool` returns a dict including `data.obcs`
- At least `OBCS_PARM01` is listed with a description

---

### A10. Package navigation

**Prompt:**
> What subroutines are in the MITgcm `kpp` package?

**Pass criteria:**
- `get_package_tool("kpp")` returns a non-empty subroutine list
- File paths reference `pkg/kpp/`

---

## Part B — FESOM2 MCP server (`fesom2-mcp-v2026.02.6`)

### B1. Tool discovery

**Prompt:**
> List all available tools in the FESOM2 MCP server and count them.

**Pass criteria:**
- Exactly 20 tools reported
- Names include `search_code_tool`, `get_source_tool`, `namelist_to_code_tool`,
  `find_modules_tool`, `get_module_tool`, `get_module_uses_tool`,
  `list_setups_tool`, `translate_lab_params_tool`, `lookup_gotcha_tool`,
  `get_run_interface_tool`

---

### B2. Namelist parameter lookup

**Prompt:**
> What controls GM diffusivity in FESOM2? Which module reads the parameter and in which namelist group?

**Pass criteria:**
- Returns `K_GM` (case-insensitive)
- Identifies the reading module (e.g. a config or oce module)
- Namelist group references `namelist_oce` or similar

---

### B3. Module navigation

**Prompt:**
> Find the FESOM2 module that handles GM/Redi eddy parameterisation and show what other modules it depends on.

**Pass criteria:**
- `find_modules_tool` or `search_code_tool` finds a relevant module
  (e.g. `oce_fer_gm` or similar)
- `get_module_uses_tool` returns a list of dependent modules

---

### B4. Semantic search over docs + namelists

**Prompt:**
> What does the FESOM2 documentation say about the ALE vertical coordinate? Also search for namelist parameters related to ALE.

**Pass criteria:**
- `search_docs_tool` returns results with file/section from FESOM2 RST docs
- At least one result mentions ALE or vertical coordinates

---

### B5. Setups catalogue

**Prompt:**
> List all available FESOM2 reference setups. Then show the full namelist configuration for the toy_neverworld2 setup.

**Pass criteria:**
- `list_setups_tool(names_only=True)` returns multiple setups
- `list_setups_tool(name="neverworld2")` returns namelist stanzas with
  `{value, comment}` leaves
- At least `namelist.oce` and `namelist.config` groups present

---

### B6. Lab parameter translation

**Prompt:**
> Set up a FESOM2 baroclinic channel experiment: 4000 km × 2000 km, depth 4 km, f-plane at 45°N (Omega=5.1e-5 rad/s), temperature contrast 5 K, 200×100×40 grid. Translate to namelist values and check scales.

**Pass criteria:**
- `translate_lab_params_tool` returns `f0`, viscosity, diffusivity values
- `check_scales_tool` returns Rossby number, Burger number, and flags
- No Python exception or error message in the response

---

### B7. Experiment skeleton

**Prompt:**
> Suggest a FESOM2 namelist skeleton for a baroclinic channel experiment.

**Pass criteria:**
- `suggest_experiment_config_tool("baroclinic_channel")` returns a dict
- Keys include `namelists` with at least `namelist.config` and `namelist.oce`
- `notes` list is non-empty

---

### B8. Gotcha lookup

**Prompt:**
> What FESOM2 gotchas should I know about when setting up an ALE experiment?

**Pass criteria:**
- `lookup_gotcha_tool` returns at least one entry mentioning ALE or vertical
  coordinate

---

### B9. Run interface

**Prompt:**
> Describe the FESOM2 Docker runtime interface — what directories do I need to mount and where does output go?

**Pass criteria:**
- `get_run_interface_tool` returns a dict with `docker_interface` key
- Mounts for `/mesh`, `/input`, `/output` are described

---

### B10. Workflow guidance

**Prompt:**
> I want to design a new FESOM2 ocean experiment from scratch. What is the recommended sequence of MCP tool calls to use?

**Pass criteria:**
- `get_workflow_tool("design_experiment")` returns a step list
- Steps reference actual FESOM2 tool names

---

## Part C — Runtime images

### C1. MITgcm runtime (`mitgcm-runtime-v2026.02.6`)

This image is used as a build base for experiment Dockerfiles, not run directly.
Verify it is pullable and contains MITgcm source:

```bash
docker run --rm ghcr.io/willirath/ogcmcp:mitgcm-runtime-v2026.02.6 \
  ls /MITgcm/model/src/ | head -5
```

**Pass criteria:** Prints `.F` filenames (e.g. `MAIN.F`, `THE_MAIN_LOOP.F`).

---

### C2. FESOM2 runtime (`fesom2-runtime-v2026.02.6`)

Verify the image is pullable and the FESOM2 binary exists:

```bash
docker run --rm ghcr.io/willirath/ogcmcp:fesom2-runtime-v2026.02.6 \
  fesom.x --help 2>&1 | head -3
```

**Pass criteria:** Binary exists (even if `--help` exits non-zero, the
absence of "not found" is sufficient).

---

## Pass / fail summary

| Test | Server | Critical? |
|---|---|---|
| A1 Tool count (23) | MITgcm MCP | Yes |
| A2 Namelist → `cg3dMaxIters` | MITgcm MCP | Yes |
| A3 Code search + source | MITgcm MCP | Yes |
| A4 Call graph (CG3D) | MITgcm MCP | Yes |
| A5 Docs search | MITgcm MCP | Yes |
| A6 Verification experiments | MITgcm MCP | Yes |
| A7 Lab translation + scales | MITgcm MCP | Yes |
| A8 Gotcha lookup | MITgcm MCP | Yes |
| A9 Namelist structure | MITgcm MCP | No |
| A10 Package navigation | MITgcm MCP | No |
| B1 Tool count (20) | FESOM2 MCP | Yes |
| B2 Namelist → `K_GM` | FESOM2 MCP | Yes |
| B3 Module navigation | FESOM2 MCP | Yes |
| B4 Docs + namelist search | FESOM2 MCP | Yes |
| B5 Setups catalogue | FESOM2 MCP | Yes |
| B6 Lab translation + scales | FESOM2 MCP | Yes |
| B7 Experiment skeleton | FESOM2 MCP | Yes |
| B8 Gotcha lookup | FESOM2 MCP | Yes |
| B9 Run interface | FESOM2 MCP | No |
| B10 Workflow guidance | FESOM2 MCP | No |
| C1 MITgcm runtime | — | No |
| C2 FESOM2 runtime | — | No |

A release is ready to tag when all "Yes" rows pass.

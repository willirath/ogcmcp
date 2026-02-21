# MITgcm Rotating-Tank Knowledge System

A design sketch for an AI-assisted system that can reason about MITgcm's
source code, configuration, numerics, and output strategy — specialised for
idealised experiments at tabletop scales with or without rotation.

## Motivation

MITgcm has ~130k lines of Fortran across ~600 files — more than any single
LLM context window. But the harder challenge is not scale; it is
*configuration complexity*. A rotating-tank
experiment sits far from MITgcm's ocean defaults — non-hydrostatic dynamics,
centimetre-scale grids, walls on all sides, f-plane rotation calibrated to a
lab turntable. Getting from a physical experiment description to a running
model configuration requires:

- Translating lab parameters (tank radius, rotation rate, density contrast)
  into model inputs (grid spacing, `f0`, linear EOS coefficients)
- Knowing which CPP flags unlock non-hydrostatic and other needed physics
- Choosing diagnostics that map to what the lab actually measures
- Avoiding well-known gotchas (spin-up time, sidewall BCs, pressure
  reference level, non-Boussinesq pitfalls)

Beyond configuration, the system also lowers the cost of extending the model
itself. Adding a new mixing scheme, a custom forcing, or a diagnostic field
means navigating unfamiliar call trees and CPP guard logic. With the code
graph and semantic index in place, those tasks become tractable: find the
right insertion point, understand what interfaces to implement, and avoid
breaking existing paths.

No existing tool does this reasoning. The goal is a system that does.

## Scope: tabletop rotating-tank experiments

Target experiment classes (all at O(0.1–1 m) horizontal scale):

| Experiment | Key physics |
|---|---|
| Rotating convection | Non-hydrostatic, f-plane, surface buoyancy flux |
| Baroclinic instability (Eady) | Two-layer density, sidewalls, spindown |
| Ekman layer | Viscous boundary layer, no-slip bottom |
| Gravity current on an f-plane | Density front, geostrophic adjustment |
| Taylor column | Topography, quasi-geostrophic constraint |
| Differential heating / Hadley cell analogue | Sidewall heating, axisymmetric setup |

These share: closed lateral boundaries, linear EOS, f-plane or non-rotating,
resolution O(1–10 mm), explicit viscosity, no sub-grid parameterisations.

## Architecture

```
                  ┌─────────────────────────────┐
                  │   User query                │
                  │   (lab params or config Q)  │
                  └──────────────┬──────────────┘
                                 │
                  ┌──────────────▼──────────────┐
                  │   Claude (tool use +        │
                  │   extended thinking)        │
                  └──┬──────┬──────┬────────────┘
                     │      │      │
         ┌───────────▼─┐ ┌──▼───┐ ┌▼──────────────────┐
         │ Code graph  │ │ RAG  │ │ Domain knowledge  │
         │ (DuckDB)    │ │index │ │ (scaling + gotcha)│
         └─────────────┘ └──────┘ └───────────────────┘
```

The code graph and RAG index answer *how MITgcm works*.
The domain knowledge layer answers *how to configure it for a tank*.

### Code graph (DuckDB)

Parsed from source via `tree-sitter-fortran` (handles both `.F` and `.F90`):

```sql
metadata(key, value)  -- e.g. mitgcm_commit_sha, indexed_at
subroutines(id, name, file, package, source_text, line_start, line_end)
calls(caller_id, callee_name)
namelist_refs(param_name, subroutine_id, namelist_file)
diagnostics_fills(field_name, subroutine_id, array_name, comment)
cpp_guards(subroutine_id, cpp_flag)  -- #ifdef / #ifndef blocks
package_options(package_name, cpp_flag, description)
```

Three tables go beyond what a generic Fortran code graph would need:

- `cpp_guards` — MITgcm's physics is heavily CPP-gated (`ALLOW_NONHYDROSTATIC`,
  `ALLOW_OBCS`, `ALLOW_DIAGNOSTICS`, …). Knowing which guard wraps a given
  code path is essential for building `CPP_OPTIONS.h`.
- `package_options` — each MITgcm package declares its CPP flags; this table
  makes them queryable.
- `namelist_file` on `namelist_refs` — MITgcm splits namelists across
  `data`, `data.diagnostics`, `data.pkg`, `data.eos`, etc. The file
  attribution matters when building a configuration.

### Semantic index (ChromaDB + nomic-embed-text)

Each subroutine embedded as a unit. Enables natural-language search over
physics implementation: "sidewall no-slip condition", "linear equation of
state", "non-hydrostatic pressure solve", "Coriolis on f-plane".

Embeddings run locally via Ollama — no external API, runs on Apple Silicon:

```sh
ollama pull nomic-embed-text
```

```python
def embed(text: str) -> list[float]:
    return ollama.embed(model="nomic-embed-text", input=text)["embeddings"][0]
```

### Domain knowledge layer (the differentiator)

A curated, structured store of tank-experiment knowledge that an LLM alone
does not reliably have. Three sub-components:

### Parameter translator

Maps physical lab quantities to MITgcm namelist values:

```
f0      = 2 * Omega              (Omega: turntable angular velocity in rad/s)
deltaX  = tank_radius / Nx       (horizontal resolution)
viscAz  = molecular_nu           (kinematic viscosity, ~1e-6 m²/s for water)
tAlpha  = thermal_expansion      (for linear EOS, ~2e-4 K⁻¹ for fresh water)
```

### Dimensionless number checker

Given a proposed configuration, computes and interprets:

| Number | Formula | Regime guidance |
|---|---|---|
| Rossby | Ro = U / (f L) | Ro ≪ 1 → rotationally dominated |
| Ekman | Ek = ν / (f H²) | Ek ≪ 1 → thin Ekman layers; check vertical resolution |
| Burger | Bu = (N H / f L)² | Bu ~ 1 → baroclinic instability active |
| Hydrostatic | δ = H / L | δ ≪ 1 → hydrostatic OK; δ ~ 1 → non-hydrostatic needed |
| CFL | Co = u Δt / Δx | < 0.5 for explicit advection |

Flags when non-hydrostatic is required (δ ≳ 0.1), when Ekman layers are
under-resolved, or when spin-up time exceeds the integration length.

### Gotcha catalogue

Known configuration traps for this experiment class:

- `nonHydrostatic = .TRUE.` requires `ALLOW_NONHYDROSTATIC` in `CPP_OPTIONS.h`
  *and* `useNHMTerms = .TRUE.` in `data` namelist `&PARM01`
- Linear EOS (`eosType = 'LINEAR'`) requires `tAlpha`, `sBeta` in `data.eos`
  `&EOS_PARM01`; forgetting `sBeta = 0` in a freshwater experiment causes a
  spurious salinity-driven density gradient
- Spin-up to solid-body rotation takes O(Ek^{-1/2}) rotation periods; a
  common mistake is too-short spin-up before imposing the forcing
- Sidewall no-slip: MITgcm defaults to free-slip; set `no_slip_sides = .TRUE.`
  explicitly when matching tank walls
- Pressure reference level: `EosRefP0` matters for compressibility; for
  shallow tanks set to surface pressure and use Boussinesq
- `rigidLid = .TRUE.` is appropriate for a tank with a solid lid; free surface
  runs need `implicitFreeSurface = .TRUE.` and a barotropic time-step check
- Diagnostics output: `data.diagnostics` frequency is in seconds; a common
  error is confusing it with model time steps

## Tool interface

```python
# Code navigation
search_code(query, top_k=8)
    # semantic search over subroutine embeddings

get_subroutine(name)
    # full source of a named subroutine

get_callers(name)
    # subroutines that call this one

get_callees(name)
    # subroutines called by this one

namelist_to_code(param, namelist_file=None)
    # all subroutines that read a given namelist parameter

diagnostics_fill_to_source(field_name)
    # trace a diagnostics field name back to the array and subroutine

get_cpp_requirements(subroutine_name)
    # what CPP flags gate this subroutine or the code paths within it

get_package_flags(package_name)
    # all CPP options a package exposes, with descriptions

# Domain knowledge
translate_lab_params(tank_radius, depth, Omega, delta_T=None, delta_rho=None)
    # returns a draft namelist stanza

check_scales(config_dict)
    # computes Ro, Ek, Bu, hydrostatic parameter, CFL; flags issues

lookup_gotcha(topic)
    # search the gotcha catalogue by keyword

suggest_experiment_config(experiment_type)
    # returns a minimal working configuration skeleton for a known experiment class
```

The model traverses the graph lazily, fetching only what the query needs.

## Example sessions

### Session 1 — translating a lab setup

> "My tank is 40 cm radius, 20 cm deep, rotating at 3 RPM. I want to
> simulate Eady baroclinic instability with a 2 K top-to-bottom temperature
> difference. What namelist parameters do I need?"

Claude calls `translate_lab_params(radius=0.4, depth=0.2, Omega=0.314, delta_T=2.0)`,
then `check_scales(...)` — sees Ek ~ 4×10⁻⁴, Bu ~ 0.8, δ = 0.5 — flags
that δ is large enough to consider non-hydrostatic, then calls
`get_cpp_requirements("solve_for_pressure")` to confirm the flag needed, and
finally `lookup_gotcha("linear EOS freshwater")`. Returns a complete
`data` + `data.eos` + `CPP_OPTIONS.h` fragment with annotations.

### Session 2 — diagnostics

> "I want time series of the azimuthal velocity at mid-depth. Is this
> available directly or do I need to derive it?"

Claude calls `search_code("azimuthal velocity diagnostics")`, then
`diagnostics_fill_to_source("UVEL")` and `diagnostics_fill_to_source("VVEL")`,
discovers that MITgcm outputs Cartesian U/V but not azimuthal velocity
directly — recommends a post-processing approach and provides the
`data.diagnostics` stanza for UVEL/VVEL snapshots at the required level.

### Session 3 — debugging a stalled run

> "My non-hydrostatic run crashes after 2 time steps with a pressure solve
> divergence. What should I check?"

Claude calls `lookup_gotcha("nonhydrostatic pressure")`, `get_subroutine("cg3d")`,
`namelist_to_code("cg3dMaxIter")` — and identifies that the default
conjugate-gradient iteration limit is often too low for tank-scale grids
with high aspect ratio, recommending `cg3dMaxIter = 200` and `cg3dTargetResidual`.

## Infrastructure

Single machine (MacBook or small VM) is sufficient.

| Component | Software |
|---|---|
| Parsing | `tree-sitter` + `tree-sitter-fortran` |
| Code graph | DuckDB (single file, no server) |
| Embeddings model | `nomic-embed-text` via Ollama |
| Vector store | ChromaDB (embedded, no server) |
| Tool server | FastAPI + `mcp` library |
| LLM | Claude API (tool use + extended thinking) |

```
pixi add fastapi chromadb duckdb tree-sitter ollama
```

Exposed as an MCP server so Claude Desktop / Claude Code can connect directly.

### Parsing notes for MITgcm

MITgcm mixes `.F` (fixed-form with CPP) and `.F90` (free-form) files.
`tree-sitter-fortran` handles both, but CPP directives need a pre-pass:

```
cpp -P -traditional-cpp <file>.F | tree-sitter parse --lang fortran
```

Run the parse twice: once with each relevant `ALLOW_*` flag set, to capture
both sides of `#ifdef` blocks. Store the guard condition in `cpp_guards`.

## Indexing pipeline

Run once against the MITgcm source tree (~20 minutes):

```
git clone https://github.com/MITgcm/MITgcm
  → for each .F / .F90 in model/, pkg/, eesupp/:
      → cpp pre-pass (with standard flags)
      → tree-sitter parse
      → extract subroutines, calls, namelist refs,
        DIAGNOSTICS_FILL calls, #ifdef guards
      → write to DuckDB
      → embed subroutine text with nomic-embed-text
      → write to ChromaDB
  → parse all data.* namelist templates in verification/
    → seed gotcha catalogue from known-good examples
```

MITgcm does not use numbered release tags. Pin the source as a git submodule; the submodule ref is the version record. Re-run the indexing pipeline when the submodule is updated.

## Domain knowledge seeding

The gotcha catalogue and parameter translator are seeded by:

1. Reading the MITgcm documentation (`mitgcm.readthedocs.io`) and the
   "Getting Started" and "Packaged Configurations" chapters alongside the
   source.
2. Ingesting a curated set of published rotating-tank MITgcm configurations
   (e.g., the MITgcm rotating tank tutorial; TBD: real literature examples)
   and extracting their namelist choices as examples.
3. Growing iteratively: each time a user query reveals a gap, the answer
   (once verified) is added to the catalogue.

## Build order

1. Indexing pipeline — DuckDB + ChromaDB populated from MITgcm source
2. Core code-navigation tools (`search_code`, `get_subroutine`, `get_callers`,
   `namelist_to_code`, `diagnostics_fill_to_source`, `get_cpp_requirements`)
3. MCP server exposing tools to Claude
4. Domain knowledge layer — parameter translator, scaling checker, gotcha
   catalogue — added as a structured JSON/YAML store queried by `translate_lab_params`,
   `check_scales`, `lookup_gotcha`, `suggest_experiment_config`
5. Iterative enrichment from real experiment sessions

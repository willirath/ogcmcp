# MCP server

`src/server.py` wraps all tools in an MCP server using FastMCP (bundled in
the official `mcp` SDK). It communicates over stdio so Claude Code picks it
up with zero network configuration.

## Install (users)

```bash
claude mcp add --transport stdio --scope user mitgcm -- \
  docker run --rm -i ghcr.io/willirath/mitgcm-mcp:v2026.02.1
```

## Development use

```sh
pixi run serve
```

The `.mcp.json` in the repo root points Claude Code at `pixi run serve`
when working inside the repository.

## Tools

All name and parameter lookups are case-insensitive.

### Code navigation

#### `search_code_tool`
```
search_code_tool(query: str, top_k: int = 5) -> list[dict]
```
Semantic search over subroutine embeddings. Returns up to `top_k` subroutines
ranked by cosine similarity to the query.

#### `find_subroutines_tool`
```
find_subroutines_tool(name: str) -> list[dict]
```
All subroutines matching `name` across all packages. Use when a name may
appear in multiple packages (e.g. `DIC_COEFFS_SURF` in `bling` and `dic`).
Follow up with `get_source_tool(name, package=...)`.

#### `get_subroutine_tool`
```
get_subroutine_tool(name: str, package: str | None = None) -> dict | None
```
Metadata for one subroutine — no source text. Returns `None` if not found.
Pass `package=` when the name is shared across packages.

#### `get_source_tool`
```
get_source_tool(name: str, package: str | None = None,
                offset: int = 0, limit: int = 100) -> dict | None
```
Paginated source lines. `offset` is 0-based within the subroutine source.
Check `total_lines` from `get_subroutine_tool` before fetching large routines.

#### `get_callers_tool`
```
get_callers_tool(name: str, package: str | None = None) -> list[dict]
```
All subroutines that call `name`. Empty list if none.

#### `get_callees_tool`
```
get_callees_tool(name: str, package: str | None = None) -> list[dict]
```
All subroutine names called by `name`. Includes unresolved external references.

#### `namelist_to_code_tool`
```
namelist_to_code_tool(param: str) -> list[dict]
```
Subroutines that reference a namelist parameter, with their namelist group.
Returns declaration sites (where the parameter is read from the namelist),
not use sites. Follow up with `get_callers_tool` to find where the value
is actually used.

#### `diagnostics_fill_to_source_tool`
```
diagnostics_fill_to_source_tool(field_name: str) -> list[dict]
```
Subroutines that fill a MITgcm diagnostics field. Trailing spaces trimmed.

#### `get_cpp_requirements_tool`
```
get_cpp_requirements_tool(subroutine_name: str) -> list[str]
```
CPP flags that guard a subroutine. Empty list if none.

#### `get_package_flags_tool`
```
get_package_flags_tool(package_name: str) -> list[dict]
```
CPP flags defined by a package, with descriptions.

### Documentation search

#### `search_docs_tool`
```
search_docs_tool(query: str, top_k: int = 5) -> list[dict]
```
Semantic search over MITgcm RST documentation sections (parameter
descriptions, package tutorials, algorithm explanations). Each result has
`file`, `section`, and `snippet` (first 400 chars of the matched section).

### Domain knowledge

#### `translate_lab_params_tool`
```
translate_lab_params_tool(Lx, Ly, depth, Omega, delta_T=None,
                           Nx=None, Ny=None, Nz=None,
                           nu=1e-6, kappa=1.4e-7, alpha=2e-4) -> dict
```
Translate physical lab geometry to MITgcm namelist values. All lengths in
metres, Omega in rad/s. Returns `PARM01`, `EOS_PARM01`, `PARM04`, `derived`,
and `notes`.

#### `check_scales_tool`
```
check_scales_tool(Lx, Ly, depth, Omega, delta_T=None,
                  dx=None, dy=None, dz=None, dt=None, U=None,
                  nu=1e-6, alpha=2e-4) -> dict
```
Compute dimensionless numbers (Ro, Ek, Bu, N, Ld) and flag CFL, Ekman-layer
resolution, and aspect-ratio issues.

#### `lookup_gotcha_tool`
```
lookup_gotcha_tool(topic: str) -> list[dict]
```
Keyword search over a curated catalogue of MITgcm rotating-tank configuration
traps. Returns matching entries with `title`, `keywords`, `summary`, `detail`.

#### `suggest_experiment_config_tool`
```
suggest_experiment_config_tool(experiment_type: str) -> dict | None
```
Skeleton MITgcm configuration for `rotating_convection` or
`baroclinic_instability`. Returns CPP flags, namelist stanzas, and notes.

## Example session

```
→ namelist_to_code_tool("cg3dMaxIters")
  [{"name": "CG3D", "namelist_group": "PARM03", ...}]

→ get_cpp_requirements_tool("CG3D")
  ["ALLOW_NONHYDROST"]

→ search_docs_tool("non-hydrostatic pressure solver")
  [{"file": "phys_pkgs/nonhydrostatic.rst",
    "section": "Non-hydrostatic pressure", "snippet": "..."}]
```

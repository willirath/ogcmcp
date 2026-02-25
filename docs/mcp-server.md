# MCP server (MITgcm)

`src/mitgcm/server.py` exposes MITgcm code-navigation tools via FastMCP
over stdio. For the FESOM2 equivalent see `src/fesom2/server.py`; the
install instructions and tool reference for FESOM2 are in `README.md`.

## Install (users)

**Claude Code:**
```bash
claude mcp add --transport stdio --scope user mitgcm -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:mitgcm-mcp-v2026.02.5
```

**Codex CLI:**
```bash
codex mcp add mitgcm -- docker run --rm -i ghcr.io/willirath/ogcmcp:mitgcm-mcp-v2026.02.5
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
descriptions, package tutorials, algorithm explanations) and verification
experiment `.h` header files. Each result has `file`, `section`, and
`snippet` (first 400 chars of the matched section).

#### `get_doc_source_tool`
```
get_doc_source_tool(file: str, section: str,
                    offset: int = 0, limit: int = 200) -> dict | None
```
Full paginated text of a documentation section or header file. Use
`search_docs_tool` first to discover `file` and `section` values, then call
this to read the complete content. Mirrors `get_source_tool` for subroutines.
Returns `{file, section, total_lines, offset, lines}` or `None` if not found.

### Verification experiments

#### `list_verification_experiments_tool`
```
list_verification_experiments_tool() -> list[dict]
```
Structured catalogue of all MITgcm verification and tutorial experiments.
Per-experiment fields: `name`, `tutorial`, `packages`, `domain_class`,
`Nx`, `Ny`, `Nr`, `grid_type`, `nonhydrostatic`, `free_surface`, `eos_type`.
All fields derived automatically from experiment files. No Ollama required.

#### `search_verification_tool`
```
search_verification_tool(query: str, top_k: int = 5) -> list[dict]
```
Semantic search over verification/tutorial experiment configuration files
(`input/data*`, `input/eedata`, `code/*.h`, `code/packages.conf`). Each
result has `experiment`, `file`, `filename`, `snippet`. Follow up with
`get_verification_source_tool` to read the full file content.

#### `get_verification_source_tool`
```
get_verification_source_tool(file: str,
                              offset: int = 0, limit: int = 200) -> dict | None
```
Full paginated text of a verification experiment file. Use
`search_verification_tool` first to discover `file` paths
(e.g. `"verification/rotating_convection/input/data"`). Returns
`{file, total_lines, offset, lines}` or `None` if not found. Call
repeatedly with increasing `offset` to page through large files.

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
Keyword search over a curated catalogue of MITgcm configuration traps. Returns matching entries with `title`, `keywords`, `summary`, `detail`.

#### `suggest_experiment_config_tool`
```
suggest_experiment_config_tool(experiment_type: str) -> dict | None
```
Skeleton MITgcm configuration for `rotating_convection` or
`baroclinic_instability`. Returns CPP flags, namelist stanzas, and notes.

#### `get_namelist_structure_tool`
```
get_namelist_structure_tool() -> dict[str, dict[str, str]]
```
Full map of MITgcm namelist files to groups to descriptions:
`{ file: { group: description } }`. Use this to orient yourself when you
know the domain (e.g. 'open boundary conditions', 'EOS parameters') but
not yet which file/group to edit. Returns all ~100 groups known to the
DuckDB index plus explicit entries for core and non-obvious groups
(e.g. `GM_PARM01 → data.gmredi`, `EXF_NML_01 → data.exf`).

To find which group reads a *specific parameter by name*, use
`namelist_to_code_tool` instead. To understand a group's parameters in
depth, follow up with `search_docs_tool(group_name)`.

### Workflow guidance

#### `get_workflow_tool`
```
get_workflow_tool(task: str | None = None) -> dict
```
Recommended tool sequence for a named task. Call this at the start of a
session to orient the agent. `task` is one of `design_experiment`,
`debug_configuration`, `understand_package`, `explore_code`. Pass `None` (or
omit) to get all workflows. Each workflow has `description`, `steps` (ordered
list of `{tool, purpose}`), and `notes`.

## Working with namelists

MITgcm namelists are standard Fortran namelists. The Python package
[`f90nml`](https://f90nml.readthedocs.io/) is available in the project
environment (`pixi run python -c "import f90nml"`) and is useful for two
tasks that are not covered by MCP tools:

**Validation** — parse a namelist stanza to catch syntax errors before
writing to disk:
```python
import f90nml
nml = f90nml.reads("&PARM01\n nonHydrostatic=.TRUE.,\n f0=1e-4,\n /")
# raises ValueError on malformed input (missing terminator, etc.)
```

**Formatting** — produce clean, consistently indented output from a dict:
```python
import f90nml, io
nml = f90nml.reads("&PARM01\n nonHydrostatic=.TRUE.,\n f0=1e-4,\n /")
buf = io.StringIO()
f90nml.write(nml, buf)
print(buf.getvalue())
```

Caveats: `f90nml` lowercases all group and parameter names on output
(MITgcm reads case-insensitively, so this is fine). `N*value` array
shorthand (`tRef=29*20.0`) is expanded to an explicit list on parse and
not re-compressed on write.

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

# MCP server

`src/server.py` wraps the seven M3 code-navigation tools in an MCP server
using FastMCP (bundled in the official `mcp` SDK). It communicates over stdio
so Claude Code picks it up with zero network configuration.

## Starting the server

```sh
pixi run serve
```

The server blocks on stdin waiting for a client. You do not normally run it
manually — Claude Code launches it automatically via `.mcp.json`.

## Claude Code integration

The project root contains `.mcp.json`:

```json
{
  "mcpServers": {
    "mitgcm": {
      "command": "pixi",
      "args": ["run", "serve"]
    }
  }
}
```

Open a Claude Code session in this directory and the `mitgcm` server appears
in `/mcp`. Claude can then call any of the eight tools directly.

## Tools

All name and parameter lookups are case-insensitive.

### `search_code_tool`

```
search_code_tool(query: str, top_k: int = 5) -> list[dict]
```

Semantic search over subroutine embeddings. Requires a running Ollama server
(`docker compose up -d`) and a populated ChromaDB index (`pixi run embed`).

Returns up to `top_k` subroutines ranked by cosine similarity:

```python
[{"id": 42, "name": "CG3D", "file": "...", "package": "model",
  "line_start": 1, "line_end": 350}]
```

### `get_subroutine_tool`

```
get_subroutine_tool(name: str) -> dict | None
```

Metadata for one subroutine — no source text. Returns `None` if not found.

```python
{"id": 42, "name": "CG3D", "file": "...", "package": "model",
 "line_start": 1, "line_end": 350}
```

Use `line_end - line_start` to gauge size before fetching source.

### `get_source_tool`

```
get_source_tool(name: str, offset: int = 0, limit: int = 100) -> dict | None
```

Paginated source lines for a subroutine. `offset` is 0-based within the
subroutine source; `limit` caps lines returned. Returns `None` if not found.

```python
{"name": "CG3D", "total_lines": 532, "offset": 0,
 "lines": ["      SUBROUTINE CG3D(", ...]}
```

Large subroutines (e.g. `INI_PARMS` at ~1500 lines) must be read in pages
to stay within Claude Code's tool-result token limit.

### `get_callers_tool`

```
get_callers_tool(name: str) -> list[dict]
```

All subroutines that call `name`. Empty list if none.

### `get_callees_tool`

```
get_callees_tool(name: str) -> list[dict]
```

All subroutine names called by `name`. Includes callees not present in the
index (external or unresolved references). Empty list if none.

### `namelist_to_code_tool`

```
namelist_to_code_tool(param: str) -> list[dict]
```

Subroutines that reference a namelist parameter, with their namelist group.

```python
[{"id": 42, "name": "CG3D", "file": "...", "package": "model",
  "namelist_group": "PARM03"}]
```

### `diagnostics_fill_to_source_tool`

```
diagnostics_fill_to_source_tool(field_name: str) -> list[dict]
```

Subroutines that fill a MITgcm diagnostics field. Trailing spaces in stored
field names are trimmed before comparison.

```python
[{"id": 42, "name": "CG3D", "file": "...", "package": "model",
  "array_name": "rho3d"}]
```

### `get_cpp_requirements_tool`

```
get_cpp_requirements_tool(subroutine_name: str) -> list[str]
```

CPP flags that guard a subroutine. Empty list if none.

```python
["ALLOW_NONHYDROST"]
```

### `get_package_flags_tool`

```
get_package_flags_tool(package_name: str) -> list[dict]
```

CPP flags defined by a package, with descriptions. Empty list if not found.

```python
[{"cpp_flag": "ALLOW_NONHYDROST", "description": "Enable non-hydrostatic solver"}]
```

## Example session

```
> get_subroutine_tool("cg3d")
{"id": 42, "name": "CG3D", "file": "model/src/cg3d.F",
 "line_start": 13, "line_end": 545}

> get_source_tool("cg3d", offset=0, limit=20)
{"name": "CG3D", "total_lines": 532, "offset": 0, "lines": [...]}

> namelist_to_code_tool("cg3dMaxIters")
[{"name": "CG3D", "namelist_group": "PARM03", ...}]

> get_cpp_requirements_tool("CG3D")
["ALLOW_NONHYDROST"]
```

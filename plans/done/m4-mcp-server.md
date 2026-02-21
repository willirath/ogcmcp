# M4: MCP Server

## Context

M3 exposed the seven code-navigation tools as plain Python callables.
M4 wraps them in an MCP server so Claude Code (and Claude Desktop) can call
them directly during a session.

Done-when criterion: a live Claude Code session calls `get_subroutine("cg3d")`
and gets source text back.

---

## Package choice

Use **`mcp`** (the official Anthropic Python SDK, currently 1.26.0).
It bundles FastMCP's high-level decorator API at `mcp.server.fastmcp.FastMCP`.

There is also a standalone `fastmcp` package (currently 3.0.0) which has
diverged significantly from the version absorbed into `mcp`. We don't need
its extra features and sticking with the official SDK keeps the dependency
graph simple.

The roadmap listed "FastAPI + mcp" but FastAPI is not needed for a stdio
server — FastMCP handles everything. FastAPI remains in the environment for
possible future HTTP/SSE transport.

---

## Files to create / modify

### `pixi.toml`

Add to `[pypi-dependencies]`:

```toml
mcp = ">=1.0,<2"
```

Add to `[tasks]`:

```toml
serve = "python -m src.server"
```

### `src/server.py`

```python
from mcp.server.fastmcp import FastMCP
from src.tools import (
    search_code, get_subroutine, get_callers, get_callees,
    namelist_to_code, diagnostics_fill_to_source,
    get_cpp_requirements, get_package_flags,
)

mcp = FastMCP("mitgcm")

@mcp.tool()
def ...  # one wrapper per M3 function

if __name__ == "__main__":
    mcp.run()  # stdio by default
```

FastMCP infers JSON schemas and descriptions from Python type annotations
and docstrings — no manual schema work needed. Return types are already
plain dicts/lists.

`search_code` is included; its docstring notes it requires a running Ollama
server and a populated ChromaDB.

### `.mcp.json` (project root)

Claude Code picks this up automatically when the working directory is the
project root:

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

### `tests/test_server.py`

Registration-only: assert all 7 expected tool names are present in the
FastMCP app's tool registry. Fast, no fixtures, no live services needed.

```python
EXPECTED_TOOLS = {
    "search_code", "get_subroutine", "get_callers", "get_callees",
    "namelist_to_code", "diagnostics_fill_to_source",
    "get_cpp_requirements", "get_package_flags",
}

def test_all_tools_registered():
    from src.server import mcp
    registered = {t.name for t in mcp.list_tools()}  # or equivalent API
    assert EXPECTED_TOOLS <= registered
```

Exact introspection API (`mcp.list_tools()` or `mcp._tools` etc.) to be
confirmed at implementation time by inspecting the FastMCP object.

### `docs/mcp-server.md`

Covers: purpose, `pixi run serve`, `.mcp.json` setup, all seven tools with
inputs and return shapes, note about Ollama requirement for `search_code`.

---

## Roadmap update

Tick off all M4 checklist items in `plans/roadmap.md`.

---

## Verification

1. `pixi run python -c "from mcp.server.fastmcp import FastMCP"` succeeds
2. `pixi run test` still passes
3. Open a Claude Code session in this directory — `mitgcm` appears in `/mcp`
4. Call `get_subroutine("cg3d")` — returns source text ✓

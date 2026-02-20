"""MCP server exposing the M3 code-navigation tools via stdio."""

from mcp.server.fastmcp import FastMCP

from src.tools import (
    diagnostics_fill_to_source,
    get_callees,
    get_callers,
    get_cpp_requirements,
    get_package_flags,
    get_subroutine,
    namelist_to_code,
    search_code,
)

mcp = FastMCP("mitgcm")


@mcp.tool()
def search_code_tool(query: str, top_k: int = 5) -> list[dict]:
    """Semantic search over MITgcm subroutines.

    Returns up to top_k subroutines whose source most closely matches the
    natural-language query. Requires a running Ollama server and a populated
    ChromaDB index (pixi run embed).
    """
    return search_code(query, top_k=top_k)


@mcp.tool()
def get_subroutine_tool(name: str) -> dict | None:
    """Return metadata for a subroutine by name (no source text).

    Returns id, name, file, package, line_start, line_end.
    Name lookup is case-insensitive. Returns None if not found.
    Use get_source_tool to retrieve the actual source lines.
    """
    result = get_subroutine(name)
    if result is None:
        return None
    result.pop("source_text", None)
    return result


@mcp.tool()
def get_source_tool(name: str, offset: int = 0, limit: int = 100) -> dict | None:
    """Return paginated source lines for a subroutine.

    offset: first line to return (0-based within the subroutine source).
    limit: maximum number of lines to return (default 100).

    Returns {name, total_lines, offset, lines: [...]}. Lines are returned as
    a list of strings without trailing newlines. Returns None if not found.
    Use get_subroutine_tool first to see total line count (line_end - line_start).
    """
    result = get_subroutine(name)
    if result is None:
        return None
    all_lines = result["source_text"].splitlines()
    total = len(all_lines)
    page = all_lines[offset : offset + limit]
    return {"name": result["name"], "total_lines": total, "offset": offset, "lines": page}


@mcp.tool()
def get_callers_tool(name: str) -> list[dict]:
    """Return all subroutines that call the named subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    """
    return get_callers(name)


@mcp.tool()
def get_callees_tool(name: str) -> list[dict]:
    """Return all subroutine names called by the named subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    Callees not present in the subroutines table are still returned by name.
    """
    return get_callees(name)


@mcp.tool()
def namelist_to_code_tool(param: str) -> list[dict]:
    """Return subroutines that reference a namelist parameter.

    Name lookup is case-insensitive. Returns an empty list if not found.
    Each result includes the namelist group (e.g. PARM03).
    """
    return namelist_to_code(param)


@mcp.tool()
def diagnostics_fill_to_source_tool(field_name: str) -> list[dict]:
    """Return subroutines that fill a MITgcm diagnostics field.

    Comparison trims trailing spaces and folds case — extracted field names
    sometimes carry trailing whitespace. Returns an empty list if not found.
    """
    return diagnostics_fill_to_source(field_name)


@mcp.tool()
def get_cpp_requirements_tool(subroutine_name: str) -> list[str]:
    """Return CPP flags that guard a subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    """
    return get_cpp_requirements(subroutine_name)


@mcp.tool()
def get_package_flags_tool(package_name: str) -> list[dict]:
    """Return CPP flags defined by a MITgcm package.

    Name lookup is case-insensitive. Returns an empty list if not found.
    Each result has cpp_flag and description fields.
    """
    return get_package_flags(package_name)


if __name__ == "__main__":
    mcp.run()

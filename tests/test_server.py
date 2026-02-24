"""Registration tests for src/server.py.

Verifies that all expected tools are registered with the FastMCP app.
Business logic is covered by tests/tools/; these tests catch missing
decorators or name typos.
"""

import asyncio

from src.server import mcp

EXPECTED_TOOLS = {
    "search_code_tool",
    "find_subroutines_tool",
    "get_subroutine_tool",
    "get_source_tool",
    "get_callers_tool",
    "get_callees_tool",
    "namelist_to_code_tool",
    "diagnostics_fill_to_source_tool",
    "get_cpp_requirements_tool",
    "get_package_flags_tool",
    "translate_lab_params_tool",
    "check_scales_tool",
    "lookup_gotcha_tool",
    "suggest_experiment_config_tool",
    "search_docs_tool",
    "get_doc_source_tool",
    "get_workflow_tool",
    "list_verification_experiments_tool",
    "search_verification_tool",
    "get_verification_source_tool",
    "get_namelist_structure_tool",
}


def test_all_tools_registered():
    registered = {t.name for t in asyncio.run(mcp.list_tools())}
    assert EXPECTED_TOOLS == registered


def test_tool_count():
    registered = asyncio.run(mcp.list_tools())
    assert len(registered) == len(EXPECTED_TOOLS)


def test_all_tools_have_descriptions():
    for tool in asyncio.run(mcp.list_tools()):
        assert tool.description, f"{tool.name} has no description"


def test_namelist_to_code_unknown_param_returns_warning():
    """namelist_to_code_tool should return a warning dict for unknown params."""
    from src.server import namelist_to_code_tool

    result = namelist_to_code_tool("completelyUnknownXyz123")
    assert len(result) == 1
    assert "warning" in result[0]
    assert "search_code_tool" in result[0]["warning"]


def test_namelist_to_code_known_param_has_no_warning():
    """namelist_to_code_tool should return normal results for known params."""
    from src.server import namelist_to_code_tool

    result = namelist_to_code_tool("deltaT")
    assert len(result) >= 1
    assert "warning" not in result[0]
    assert "name" in result[0]

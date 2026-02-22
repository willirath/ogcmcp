"""Unit tests for src/domain/workflow.py."""

from src.domain.workflow import get_workflow

KNOWN_TASKS = [
    "design_experiment",
    "debug_configuration",
    "understand_package",
    "explore_code",
]


def test_all_tasks_no_arg():
    """get_workflow() with no argument returns all tasks."""
    result = get_workflow()
    for task in KNOWN_TASKS:
        assert task in result


def test_single_task():
    """get_workflow(task) returns only that task."""
    result = get_workflow("design_experiment")
    assert "design_experiment" in result
    assert len(result) == 1


def test_unknown_task_returns_empty():
    """Unknown task returns empty dict."""
    assert get_workflow("nonexistent_task") == {}


def test_each_workflow_has_required_keys():
    """Every workflow must have description, steps, notes."""
    for task, wf in get_workflow().items():
        assert "description" in wf, f"{task} missing description"
        assert "steps" in wf, f"{task} missing steps"
        assert "notes" in wf, f"{task} missing notes"


def test_steps_are_nonempty_lists():
    """steps must be a non-empty list for every workflow."""
    for task, wf in get_workflow().items():
        assert isinstance(wf["steps"], list), f"{task} steps not a list"
        assert len(wf["steps"]) > 0, f"{task} steps is empty"


def test_each_step_has_tool_and_purpose():
    """Every step must have tool and purpose keys."""
    for task, wf in get_workflow().items():
        for i, step in enumerate(wf["steps"]):
            assert "tool" in step, f"{task} step {i} missing tool"
            assert "purpose" in step, f"{task} step {i} missing purpose"


def test_design_experiment_mentions_translate():
    """design_experiment workflow should reference translate_lab_params_tool."""
    wf = get_workflow("design_experiment")["design_experiment"]
    tools = [s["tool"] for s in wf["steps"]]
    assert "translate_lab_params_tool" in tools


def test_case_and_whitespace_normalisation():
    """Task lookup normalises case and spaces."""
    result = get_workflow("Design Experiment")
    assert "design_experiment" in result


def test_workflow_tool_names_are_known():
    """Every tool referenced in a workflow step must be in the MCP tool registry."""
    from tests.test_server import EXPECTED_TOOLS
    for task, wf in get_workflow().items():
        for step in wf["steps"]:
            assert step["tool"] in EXPECTED_TOOLS, (
                f"{task}: step tool '{step['tool']}' not in EXPECTED_TOOLS"
            )

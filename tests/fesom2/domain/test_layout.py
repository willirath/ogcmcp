"""Tests for src/fesom2/domain/layout.py."""

from src.fesom2.domain.layout import get_run_interface


def test_returns_dict():
    result = get_run_interface()
    assert isinstance(result, dict)


def test_required_keys():
    result = get_run_interface()
    for key in ("description", "directory_structure", "docker_interface",
                "gitignore_convention", "notes"):
        assert key in result, f"Missing key: {key!r}"


def test_docker_interface_has_mounts_and_contract():
    di = get_run_interface()["docker_interface"]
    assert "mounts" in di
    assert "entrypoint_contract" in di
    assert "command_template" in di


def test_entrypoint_patches_three_paths():
    contract = get_run_interface()["docker_interface"]["entrypoint_contract"]
    patches = contract["patches"]
    patched_names = " ".join(patches)
    assert "MeshPath" in patched_names
    assert "ResultPath" in patched_names
    assert "ClimateDataPath" in patched_names


def test_directory_structure_has_eight_namelists():
    input_dir = get_run_interface()["directory_structure"]["input/"]
    assert len(input_dir) == 8


def test_notes_is_nonempty_list():
    notes = get_run_interface()["notes"]
    assert isinstance(notes, list)
    assert len(notes) >= 1


def test_get_run_interface_tool_registered():
    from src.fesom2.server import mcp
    names = {t.name for t in mcp._tool_manager.list_tools()}
    assert "get_run_interface_tool" in names

"""Unit tests for src/domain/suggest.py."""

import pytest

from src.domain.suggest import suggest_experiment_config


def test_rotating_convection():
    """suggest_experiment_config('rotating_convection') should return a dict."""
    result = suggest_experiment_config("rotating_convection")
    assert result is not None


def test_convection_alias():
    """'convection' alias should return the same config as 'rotating_convection'."""
    canonical = suggest_experiment_config("rotating_convection")
    alias = suggest_experiment_config("convection")
    assert alias == canonical


def test_rotating_convection_alias():
    """'rotating convection' (with space) should also resolve correctly."""
    canonical = suggest_experiment_config("rotating_convection")
    alias = suggest_experiment_config("rotating convection")
    assert alias == canonical


def test_eady_alias():
    """'eady' alias should return the baroclinic_instability config."""
    result = suggest_experiment_config("eady")
    assert result is not None
    assert result["experiment_type"] == "baroclinic_instability"


def test_baroclinic_alias():
    """'baroclinic' alias should return the baroclinic_instability config."""
    result = suggest_experiment_config("baroclinic")
    assert result is not None
    assert result["experiment_type"] == "baroclinic_instability"


def test_unknown_returns_none():
    """An unrecognised experiment type should return None."""
    assert suggest_experiment_config("unknown_xyz") is None


def test_has_required_keys():
    """Result should have experiment_type, cpp_options, namelists, notes."""
    result = suggest_experiment_config("rotating_convection")
    for key in ("experiment_type", "cpp_options", "namelists", "notes"):
        assert key in result


def test_cpp_options_is_list():
    """cpp_options should be a list."""
    result = suggest_experiment_config("rotating_convection")
    assert isinstance(result["cpp_options"], list)


def test_notes_is_list():
    """notes should be a list."""
    result = suggest_experiment_config("rotating_convection")
    assert isinstance(result["notes"], list)


def test_rotating_convection_has_nonhydrostatic():
    """rotating_convection cpp_options should include ALLOW_NONHYDROSTATIC."""
    result = suggest_experiment_config("rotating_convection")
    assert "ALLOW_NONHYDROSTATIC" in result["cpp_options"]


def test_baroclinic_instability_namelists():
    """baroclinic_instability should have data and data.eos namelists."""
    result = suggest_experiment_config("baroclinic_instability")
    assert "data" in result["namelists"]
    assert "data.eos" in result["namelists"]


def test_case_insensitive():
    """Lookup should be case-insensitive."""
    result = suggest_experiment_config("ROTATING_CONVECTION")
    assert result is not None
    assert result["experiment_type"] == "rotating_convection"


def test_has_quickstart():
    """Result should include a quickstart key."""
    result = suggest_experiment_config("rotating_convection")
    assert "quickstart" in result


def test_quickstart_has_build_and_run():
    """quickstart should have build and run keys."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "build" in qs
    assert "run" in qs


def test_quickstart_has_directory_structure():
    """quickstart should have a directory_structure dict."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "directory_structure" in qs
    assert isinstance(qs["directory_structure"], dict)


def test_quickstart_build_references_docker_image():
    """quickstart build command should reference the mitgcm docker image."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "ghcr.io/willirath/mitgcm" in qs["build"]


def test_quickstart_both_experiment_types():
    """Both experiment types should have a quickstart key."""
    for exp in ("rotating_convection", "baroclinic_instability"):
        result = suggest_experiment_config(exp)
        assert "quickstart" in result, f"{exp} missing quickstart"


def test_quickstart_notes_is_nonempty_list():
    """quickstart.notes should be a non-empty list."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert isinstance(qs["notes"], list)
    assert len(qs["notes"]) > 0


def test_quickstart_directory_structure_has_required_keys():
    """quickstart.directory_structure must include SIZE.h and data.diagnostics."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    ds = qs["directory_structure"]
    assert "code/SIZE.h" in ds
    assert "input/data" in ds
    assert "input/data.diagnostics" in ds


def test_quickstart_run_references_docker_image():
    """quickstart run command should reference the mitgcm docker image."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "ghcr.io/willirath/mitgcm" in qs["run"]


def test_quickstart_build_uses_optfile():
    """quickstart build command must include -optfile (required by genmake2)."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "-optfile" in qs["build"]


def test_quickstart_build_uses_rootdir():
    """quickstart build command must include -rootdir."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "-rootdir" in qs["build"]


def test_quickstart_run_allow_run_as_root():
    """quickstart run command must include --allow-run-as-root for Docker."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "--allow-run-as-root" in qs["run"]


def test_quickstart_run_symlinks_input():
    """quickstart run command should symlink input files into the run directory."""
    qs = suggest_experiment_config("rotating_convection")["quickstart"]
    assert "input/*" in qs["run"]

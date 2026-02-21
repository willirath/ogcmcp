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

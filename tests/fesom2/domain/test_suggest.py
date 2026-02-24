"""Tests for src/fesom2/domain/suggest.py."""

import pytest
from src.fesom2.domain.suggest import suggest_experiment_config


# ── unknown ───────────────────────────────────────────────────────────────────

def test_unknown_returns_none():
    assert suggest_experiment_config("xyzzy_not_an_experiment") is None


# ── baroclinic_channel ────────────────────────────────────────────────────────

def test_baroclinic_channel_found():
    result = suggest_experiment_config("baroclinic_channel")
    assert result is not None
    assert result["experiment_type"] == "baroclinic_channel"


def test_channel_alias():
    assert suggest_experiment_config("channel") is not None
    assert suggest_experiment_config("neverworld2") is not None


def test_baroclinic_channel_has_namelists():
    result = suggest_experiment_config("baroclinic_channel")
    assert "namelists" in result
    assert "namelist.config" in result["namelists"]


def test_baroclinic_channel_no_forcing_namelist():
    # toy_ocean=.true. bypasses the forcing pipeline — skeleton must not include it
    result = suggest_experiment_config("baroclinic_channel")
    assert "namelist.forcing" not in result["namelists"]


def test_baroclinic_channel_notes_mention_windstress_file():
    result = suggest_experiment_config("baroclinic_channel")
    notes_text = " ".join(result["notes"])
    assert "windstress" in notes_text.lower()


def test_baroclinic_channel_has_notes():
    result = suggest_experiment_config("baroclinic_channel")
    assert result["notes"]


# ── pi_control ────────────────────────────────────────────────────────────────

def test_pi_control_found():
    result = suggest_experiment_config("pi_control")
    assert result is not None
    assert result["experiment_type"] == "pi_control"


def test_control_alias():
    assert suggest_experiment_config("control") is not None
    assert suggest_experiment_config("preindustrial") is not None


def test_pi_control_has_ice_namelist():
    result = suggest_experiment_config("pi_control")
    assert "namelist.ice" in result["namelists"]


def test_pi_control_step_per_day_set():
    result = suggest_experiment_config("pi_control")
    config_nl = result["namelists"]["namelist.config"]
    assert "step_per_day" in config_nl


# ── rotating_convection ───────────────────────────────────────────────────────

def test_rotating_convection_found():
    result = suggest_experiment_config("rotating_convection")
    assert result is not None
    assert result["experiment_type"] == "rotating_convection"


def test_convection_alias():
    assert suggest_experiment_config("convection") is not None


def test_rotating_convection_no_ice():
    result = suggest_experiment_config("rotating_convection")
    config_nl = result["namelists"]["namelist.config"]
    assert config_nl.get("use_ice") == ".false."


# ── common structure ──────────────────────────────────────────────────────────

@pytest.mark.parametrize("exp_type", ["baroclinic_channel", "pi_control", "rotating_convection"])
def test_required_keys_present(exp_type):
    result = suggest_experiment_config(exp_type)
    assert "experiment_type" in result
    assert "description" in result
    assert "namelists" in result
    assert "notes" in result


@pytest.mark.parametrize("exp_type", ["baroclinic_channel", "pi_control", "rotating_convection"])
def test_description_nonempty(exp_type):
    result = suggest_experiment_config(exp_type)
    assert result["description"].strip()


@pytest.mark.parametrize("exp_type", ["baroclinic_channel", "pi_control", "rotating_convection"])
def test_case_insensitive(exp_type):
    assert suggest_experiment_config(exp_type.upper()) is not None


@pytest.mark.parametrize("exp_type", ["baroclinic_channel", "pi_control", "rotating_convection"])
def test_no_cpp_options_key(exp_type):
    # FESOM2 is CMake-based — no CPP options in namelist configs
    result = suggest_experiment_config(exp_type)
    assert "cpp_options" not in result

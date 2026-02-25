"""Tests for src/fesom2/domain/forcing.py."""

from pathlib import Path

import pytest
import yaml

from src.fesom2.domain.forcing import get_forcing_spec, list_forcing_datasets


# ── helpers ──────────────────────────────────────────────────────────────────


def _write_forcings(tmp_path: Path, data: dict) -> Path:
    p = tmp_path / "forcings.yml"
    p.write_text(yaml.dump(data), encoding="utf-8")
    return p


# ── list_forcing_datasets ─────────────────────────────────────────────────────


def test_list_missing_file_returns_empty(tmp_path):
    result = list_forcing_datasets(tmp_path / "nonexistent.yml")
    assert result == []


def test_list_returns_sorted_names(tmp_path):
    p = _write_forcings(tmp_path, {"JRA55": {}, "CORE2": {}, "ERA5": {}})
    assert list_forcing_datasets(p) == ["CORE2", "ERA5", "JRA55"]


def test_list_empty_file_returns_empty(tmp_path):
    p = tmp_path / "forcings.yml"
    p.write_text("", encoding="utf-8")
    assert list_forcing_datasets(p) == []


# ── get_forcing_spec ──────────────────────────────────────────────────────────


def test_get_missing_file_returns_none(tmp_path):
    assert get_forcing_spec("ERA5", tmp_path / "nonexistent.yml") is None


def test_get_unknown_dataset_returns_none(tmp_path):
    p = _write_forcings(tmp_path, {"ERA5": {"key": "val"}})
    assert get_forcing_spec("NOTHERE", p) is None


def test_get_exact_match(tmp_path):
    spec = {"forcing_bulk": "kara", "land_ice": False}
    p = _write_forcings(tmp_path, {"ERA5": spec})
    result = get_forcing_spec("ERA5", p)
    # age_tracer defaults are always injected
    assert result["forcing_bulk"] == "kara"
    assert result["land_ice"] is False
    assert "age_tracer" in result


def test_get_case_insensitive(tmp_path):
    spec = {"forcing_bulk": "kara"}
    p = _write_forcings(tmp_path, {"ERA5": spec})
    assert get_forcing_spec("era5", p)["forcing_bulk"] == "kara"
    assert get_forcing_spec("Era5", p)["forcing_bulk"] == "kara"


def test_get_always_includes_age_tracer_defaults(tmp_path):
    """age_tracer is injected even when absent from forcings.yml."""
    spec = {"forcing_bulk": "kara"}
    p = _write_forcings(tmp_path, {"ERA5": spec})
    result = get_forcing_spec("ERA5", p)
    assert "age_tracer" in result
    assert result["age_tracer"]["use_age_tracer"] is False
    assert result["age_tracer"]["age_start_year"] == 2000


def test_get_yml_age_tracer_overrides_defaults(tmp_path):
    """If forcings.yml provides age_tracer, it wins over defaults."""
    spec = {"forcing_bulk": "kara", "age_tracer": {"use_age_tracer": True, "age_start_year": 1990}}
    p = _write_forcings(tmp_path, {"ERA5": spec})
    result = get_forcing_spec("ERA5", p)
    assert result["age_tracer"]["use_age_tracer"] is True
    assert result["age_tracer"]["age_start_year"] == 1990

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
    assert result == spec


def test_get_case_insensitive(tmp_path):
    spec = {"forcing_bulk": "kara"}
    p = _write_forcings(tmp_path, {"ERA5": spec})
    assert get_forcing_spec("era5", p) == spec
    assert get_forcing_spec("Era5", p) == spec

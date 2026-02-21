"""Unit tests for src/domain/translate.py."""

import math
import pytest

from src.domain.translate import translate_lab_params


def _base_call(**kwargs):
    """Call translate_lab_params with sensible defaults, overriding with kwargs."""
    defaults = dict(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0)
    defaults.update(kwargs)
    return translate_lab_params(**defaults)


# --- f0 formula ---

def test_f0_formula():
    """f0 should equal 2 * Omega."""
    result = _base_call(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0)
    assert math.isclose(result["PARM01"]["f0"], 2.0, rel_tol=1e-9)


def test_omega_zero():
    """Omega=0 should produce f0=0.0."""
    result = _base_call(Omega=0.0)
    assert result["PARM01"]["f0"] == 0.0
    assert result["derived"]["f0"] == 0.0


# --- Grid spacings ---

def test_grid_spacing_present():
    """When Nx, Ny, Nz are given, PARM04 should contain delX, delY, delZ."""
    result = translate_lab_params(
        Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0, Nx=80, Ny=80, Nz=20
    )
    assert "PARM04" in result
    assert math.isclose(result["PARM04"]["delX"], 0.8 / 80)
    assert math.isclose(result["PARM04"]["delY"], 0.8 / 80)
    assert math.isclose(result["PARM04"]["delZ"], 0.2 / 20)


def test_grid_spacing_absent():
    """When no Nx/Ny/Nz are given, PARM04 should be absent."""
    result = _base_call()
    assert "PARM04" not in result or result.get("PARM04") == {}


# --- Aspect ratio note ---

def test_aspect_ratio_note():
    """High aspect ratio should trigger a note."""
    result = translate_lab_params(Lx=0.8, Ly=0.8, depth=0.4, Omega=1.0)
    assert any("aspect ratio" in n.lower() or "nonhydrostatic" in n.lower() for n in result["notes"])


def test_no_aspect_ratio_note():
    """Low aspect ratio should not trigger the aspect-ratio note."""
    result = translate_lab_params(Lx=0.8, Ly=0.8, depth=0.01, Omega=1.0)
    # No note about aspect ratio / nonhydrostatic.
    for n in result["notes"]:
        assert "aspect ratio" not in n.lower() and "nonhydrostatic" not in n.lower()


# --- EOS always present ---

def test_eos_always_present():
    """EOS_PARM01 should always be present with tAlpha and sBeta=0.0."""
    result = _base_call()
    assert "EOS_PARM01" in result
    assert "tAlpha" in result["EOS_PARM01"]
    assert result["EOS_PARM01"]["sBeta"] == 0.0


# --- Rectangular tank note ---

def test_rectangular_note():
    """Rectangular tank (Lx != Ly) should add a note about the horizontal scale."""
    result = translate_lab_params(Lx=0.8, Ly=0.4, depth=0.2, Omega=1.0)
    assert any("rectangular" in n.lower() for n in result["notes"])


def test_square_no_rectangular_note():
    """Square tank (Lx == Ly) should not add a rectangular-tank note."""
    result = _base_call(Lx=0.8, Ly=0.8)
    for n in result["notes"]:
        assert "rectangular" not in n.lower()


# --- derived dict ---

def test_derived_keys():
    """derived dict should have f0, L, aspect_ratio, and dx/dy/dz (None when N not given)."""
    result = _base_call()
    d = result["derived"]
    assert "f0" in d
    assert "L" in d
    assert "aspect_ratio" in d
    assert d["dx"] is None
    assert d["dy"] is None
    assert d["dz"] is None


def test_derived_dx_dy_dz_populated():
    """derived dx, dy, dz should be populated when Nx, Ny, Nz are given."""
    result = translate_lab_params(
        Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0, Nx=80, Ny=80, Nz=20
    )
    d = result["derived"]
    assert math.isclose(d["dx"], 0.8 / 80)
    assert math.isclose(d["dy"], 0.8 / 80)
    assert math.isclose(d["dz"], 0.2 / 20)


def test_derived_L_min_lx_ly():
    """derived L should equal min(Lx, Ly)."""
    result = translate_lab_params(Lx=0.8, Ly=0.4, depth=0.2, Omega=1.0)
    assert math.isclose(result["derived"]["L"], 0.4)


def test_derived_aspect_ratio():
    """derived aspect_ratio should equal depth / min(Lx, Ly)."""
    result = translate_lab_params(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0)
    assert math.isclose(result["derived"]["aspect_ratio"], 0.2 / 0.8)

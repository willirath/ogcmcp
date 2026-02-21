"""Unit tests for src/domain/scales.py."""

import math
import pytest

from src.domain.scales import check_scales


def _base_call(**kwargs):
    defaults = dict(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0)
    defaults.update(kwargs)
    return check_scales(**defaults)


# --- Ekman number formula ---

def test_ek_formula():
    """Ek_v = nu / (f0 * depth^2) for Omega=1, depth=0.2, nu=1e-6."""
    result = _base_call(Omega=1.0, depth=0.2, nu=1e-6)
    f0 = 2.0 * 1.0
    expected_Ek = 1e-6 / (f0 * 0.2 ** 2)
    assert math.isclose(result["numbers"]["Ek_v"], expected_Ek, rel_tol=1e-6)


def test_omega_zero_ek_none():
    """Omega=0 should set Ek_v to None."""
    result = _base_call(Omega=0.0)
    assert result["numbers"]["Ek_v"] is None


# --- Optional numbers absent when inputs missing ---

def test_no_delta_T_no_Bu():
    """Without delta_T, Bu should not appear in numbers."""
    result = _base_call(delta_T=None)
    assert "Bu" not in result["numbers"]


def test_no_U_no_Ro():
    """Without U, Ro should not appear in numbers."""
    result = _base_call(U=None)
    assert "Ro" not in result["numbers"]


def test_no_dt_no_CFL():
    """Without dt, CFL_h should not appear in numbers."""
    result = _base_call(dt=None, U=0.01)
    assert "CFL_h" not in result["numbers"]


# --- Flags ---

def test_aspect_ratio_warning():
    """depth/L > 0.1 should trigger an aspect-ratio warning."""
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.4, Omega=1.0)
    messages = [f["message"] for f in result["flags"]]
    assert any("aspect ratio" in m.lower() or "non-hydrostatic" in m.lower() for m in messages)


def test_no_aspect_ratio_warning():
    """depth/L < 0.1 should not trigger the aspect-ratio warning."""
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.05, Omega=1.0)
    for f in result["flags"]:
        assert "aspect" not in f["message"].lower()


def test_cfl_warning():
    """CFL_h > 0.5 should trigger a warning."""
    # U=0.1, dt=1.0, dx=0.01 → CFL_h = 0.1 * 1.0 / 0.01 = 10.0 > 0.5
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0, U=0.1, dt=1.0, dx=0.01)
    messages = [f["message"] for f in result["flags"]]
    assert any("cfl" in m.lower() for m in messages)


def test_cfl_no_warning_when_ok():
    """CFL_h <= 0.5 should not trigger a CFL warning."""
    # U=0.001, dt=0.1, dx=0.01 → CFL_h = 0.01 << 0.5
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0, U=0.001, dt=0.1, dx=0.01)
    for f in result["flags"]:
        assert "cfl" not in f["message"].lower()


def test_ekman_not_resolved():
    """dz > ekman_depth should trigger a vertical Ekman-layer resolution warning."""
    # Omega=1 → f0=2, nu=1e-6 → ekman_depth = sqrt(1e-6/2) ≈ 0.000707 m (0.7 mm)
    # Use dz=0.01 m (10 mm) >> ekman_depth → layer not resolved.
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0, dz=0.01)
    messages = [f["message"] for f in result["flags"]]
    assert any("ekman layer" in m.lower() and "not resolved" in m.lower() for m in messages)


def test_ekman_resolved_no_warning():
    """dz < ekman_depth should not trigger the resolution warning."""
    # ekman_depth ≈ 0.000707 m (0.7 mm); use dz=0.0001 m (0.1 mm) → layer resolved.
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.2, Omega=1.0, dz=0.0001)
    for f in result["flags"]:
        assert "not resolved" not in f["message"].lower()


def test_spin_up_info_flag():
    """spin_up_periods > 50 should produce an info flag."""
    # Ek_v ~ 1e-6/(2*0.04) ~ 1.25e-5, spin_up ~ 283 >> 50
    result = _base_call(Omega=1.0, depth=0.2, nu=1e-6)
    levels = [f["level"] for f in result["flags"]]
    assert "info" in levels


def test_f0_always_present():
    """f0 should always be in numbers."""
    result = _base_call()
    assert "f0" in result["numbers"]


def test_aspect_ratio_always_present():
    """aspect_ratio should always be in numbers."""
    result = _base_call()
    assert "aspect_ratio" in result["numbers"]


def test_Bu_computed_with_delta_T():
    """Bu should appear in numbers when delta_T and Omega are given."""
    result = _base_call(delta_T=2.0)
    assert "Bu" in result["numbers"]
    assert result["numbers"]["Bu"] > 0


def test_N_computed_without_Omega():
    """N should appear even when Omega=0, as it does not depend on rotation."""
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.2, Omega=0.0, delta_T=2.0)
    assert "N" in result["numbers"]
    assert result["numbers"]["N"] > 0


def test_CFL_computed_without_Omega():
    """CFL_h should be computed even when Omega=0."""
    result = check_scales(Lx=0.8, Ly=0.8, depth=0.2, Omega=0.0, U=0.1, dt=1.0, dx=0.01)
    assert "CFL_h" in result["numbers"]

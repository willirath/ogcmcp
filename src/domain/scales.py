"""Compute dimensionless numbers and flag issues for rotating-tank configurations.

pint is used internally for all physics calculations; units are stripped
before populating the returned dict (MCP/JSON boundary accepts plain floats only).

Dimensionless numbers computed:
- f0 = 2 * Omega                     (s^-1)
- aspect_ratio = depth / L            (depth / min(Lx, Ly))
- Ek_v = nu / (f0 * depth^2)         (vertical Ekman number)
- ekman_depth = sqrt(nu / f0)         (Ekman layer thickness, m)
- spin_up_periods = Ek_v^{-0.5}      (rotation periods to solid-body spin-up)
- N = sqrt(alpha * g * delta_T / depth)  (buoyancy frequency, s^-1, if delta_T given)
- Bu = (N * depth / (f0 * L))^2      (Burger number, if delta_T and Omega given)
- Ro = U / (f0 * L)                  (Rossby number, if U and Omega given)
- CFL_h = U * dt / min(dx, dy)       (horizontal CFL, if U, dt, and dx or dy given)
- CFL_v = U * dt / dz                (vertical CFL, if U, dt, and dz given)
"""

import math

import pint

ureg = pint.UnitRegistry()

_GRAVITY = 9.81  # m/s^2


def check_scales(
    Lx: float,
    Ly: float,
    depth: float,
    Omega: float,
    delta_T: float | None = None,
    dx: float | None = None,
    dy: float | None = None,
    dz: float | None = None,
    dt: float | None = None,
    U: float | None = None,
    nu: float = 1e-6,
    alpha: float = 2e-4,
) -> dict:
    """Compute dimensionless numbers and flag issues for a rotating-tank configuration.

    Parameters
    ----------
    Lx : float
        Tank length in x in metres.
    Ly : float
        Tank length in y in metres.
    depth : float
        Water depth in metres.
    Omega : float
        Rotation rate in rad/s (0 for non-rotating).
    delta_T : float or None
        Temperature contrast in Kelvin (needed for N and Bu).
    dx : float or None
        Horizontal grid spacing in x in metres (for CFL and Ekman resolution check).
    dy : float or None
        Horizontal grid spacing in y in metres (for CFL).
    dz : float or None
        Vertical grid spacing in metres (for Ekman-layer resolution and CFL_v).
    dt : float or None
        Time step in seconds (needed for CFL).
    U : float or None
        Velocity scale in m/s (needed for Ro and CFL).
    nu : float
        Kinematic viscosity in m^2/s (default: water at 20 degC, 1e-6).
    alpha : float
        Thermal expansion coefficient in K^-1 (default: water at 20 degC, 2e-4).

    Returns
    -------
    dict
        Keys:
        - "numbers" : dict of computed numbers and scales.  Keys absent when
                      the required inputs to compute them are missing.
                      Always present: f0, aspect_ratio, Ek_v (None when Omega=0).
        - "flags"   : list of {"level": "warning"|"info", "message": str}.
    """
    # Attach pint units to inputs.
    _Lx = Lx * ureg.meter
    _Ly = Ly * ureg.meter
    _depth = depth * ureg.meter
    _Omega = Omega * ureg.radian / ureg.second
    _nu = nu * ureg.meter ** 2 / ureg.second
    _alpha = alpha / ureg.kelvin
    _g = _GRAVITY * ureg.meter / ureg.second ** 2

    _L = min(_Lx, _Ly)
    _f0 = 2.0 * _Omega
    _aspect_ratio = (_depth / _L).to(ureg.dimensionless)

    numbers: dict = {}
    flags: list[dict] = []

    # Always-present numbers.
    numbers["f0"] = _f0.to(1 / ureg.second).magnitude
    numbers["aspect_ratio"] = _aspect_ratio.magnitude

    if Omega == 0.0:
        numbers["Ek_v"] = None
    else:
        _Ek_v = (_nu / (_f0 * _depth ** 2)).to(ureg.dimensionless)
        numbers["Ek_v"] = _Ek_v.magnitude

        _ekman_depth = (_nu / _f0) ** 0.5
        numbers["ekman_depth"] = _ekman_depth.to(ureg.meter).magnitude

        _spin_up_periods = _Ek_v ** (-0.5)
        numbers["spin_up_periods"] = _spin_up_periods.magnitude

        if U is not None:
            _U = U * ureg.meter / ureg.second
            _Ro = (_U / (_f0 * _L)).to(ureg.dimensionless)
            numbers["Ro"] = _Ro.magnitude

        if delta_T is not None:
            _delta_T = delta_T * ureg.kelvin
            _N = (_alpha * _g * _delta_T / _depth) ** 0.5
            numbers["N"] = _N.to(1 / ureg.second).magnitude
            _Bu = ((_N * _depth) / (_f0 * _L)) ** 2
            numbers["Bu"] = _Bu.to(ureg.dimensionless).magnitude

        # CFL: horizontal — need dt and U plus at least one of dx, dy.
        if dt is not None and U is not None:
            _U = U * ureg.meter / ureg.second
            _dt = dt * ureg.second

            # Determine the smallest available horizontal spacing.
            _dx_vals = []
            if dx is not None:
                _dx_vals.append(dx * ureg.meter)
            if dy is not None:
                _dx_vals.append(dy * ureg.meter)

            if _dx_vals:
                _min_dh = min(_dx_vals)
                _CFL_h = (_U * _dt / _min_dh).to(ureg.dimensionless)
                numbers["CFL_h"] = _CFL_h.magnitude

            if dz is not None:
                _dz = dz * ureg.meter
                _CFL_v = (_U * _dt / _dz).to(ureg.dimensionless)
                numbers["CFL_v"] = _CFL_v.magnitude

    # --- Flags ---

    if _aspect_ratio.magnitude > 0.1:
        flags.append({
            "level": "warning",
            "message": (
                f"Aspect ratio {_aspect_ratio.magnitude:.2f} — "
                "non-hydrostatic effects likely significant "
                "(consider nonHydrostatic = .TRUE.)"
            ),
        })

    if Omega != 0.0:
        _Ek_v_val = numbers["Ek_v"]
        if _Ek_v_val > 0.1:
            flags.append({
                "level": "warning",
                "message": (
                    f"Vertical Ekman number {_Ek_v_val:.2e} — "
                    "viscous effects dominate rotation; tank may not be in "
                    "rotationally-dominated regime"
                ),
            })

        _ekman_depth_val = numbers["ekman_depth"]
        _ekman_depth_mm = _ekman_depth_val * 1000.0

        if dz is not None and _ekman_depth_val > dz:
            _dz_mm = dz * 1000.0
            flags.append({
                "level": "warning",
                "message": (
                    f"Ekman layer depth {_ekman_depth_mm:.1f} mm > dz {_dz_mm:.1f} mm — "
                    "Ekman layer not resolved vertically"
                ),
            })

        if _ekman_depth_val > depth:
            _depth_mm = depth * 1000.0
            flags.append({
                "level": "warning",
                "message": (
                    f"Ekman layer depth {_ekman_depth_mm:.1f} mm exceeds water depth — "
                    "entirely viscous column"
                ),
            })

        _spin_up_val = numbers["spin_up_periods"]
        if _spin_up_val > 50:
            flags.append({
                "level": "info",
                "message": (
                    f"Spin-up requires ~{_spin_up_val:.0f} rotation periods (Ek^{{-1/2}})"
                ),
            })

    if "CFL_h" in numbers and numbers["CFL_h"] > 0.5:
        flags.append({
            "level": "warning",
            "message": (
                f"Horizontal CFL {numbers['CFL_h']:.2f} > 0.5 — "
                "reduce time step or coarsen grid"
            ),
        })

    if "CFL_v" in numbers and numbers["CFL_v"] > 0.5:
        flags.append({
            "level": "warning",
            "message": (
                f"Vertical CFL {numbers['CFL_v']:.2f} > 0.5"
            ),
        })

    return {"numbers": numbers, "flags": flags}

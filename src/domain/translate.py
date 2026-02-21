"""Translate physical lab parameters to MITgcm namelist values.

Given tank geometry, rotation rate, and fluid properties, returns a dict
structured around MITgcm namelist groups plus derived intermediate quantities
and advisory notes.

pint is used internally for all physics calculations; units are stripped
before populating the returned dict (MCP/JSON boundary accepts plain floats only).
"""

import math

import pint

ureg = pint.UnitRegistry()


def translate_lab_params(
    Lx: float,
    Ly: float,
    depth: float,
    Omega: float,
    delta_T: float | None = None,
    Nx: int | None = None,
    Ny: int | None = None,
    Nz: int | None = None,
    nu: float = 1e-6,
    kappa: float = 1.4e-7,
    alpha: float = 2e-4,
) -> dict:
    """Translate physical lab parameters to MITgcm namelist values.

    Parameters
    ----------
    Lx : float
        Tank length in x in metres (use diameter for a cylindrical tank).
    Ly : float
        Tank length in y in metres (use diameter for a cylindrical tank).
    depth : float
        Water depth in metres.
    Omega : float
        Rotation rate in rad/s (0 for non-rotating).
    delta_T : float or None
        Temperature contrast in Kelvin (not used in namelist output; reserved
        for future stratification calculations).
    Nx : int or None
        Grid cells in x.  If given, delX = Lx / Nx is added to PARM04.
    Ny : int or None
        Grid cells in y.  If given, delY = Ly / Ny is added to PARM04.
    Nz : int or None
        Vertical grid cells.  If given, delZ = depth / Nz is added to PARM04.
    nu : float
        Kinematic viscosity in m^2/s (default: water at 20 degC, 1e-6).
    kappa : float
        Thermal diffusivity in m^2/s (default: water at 20 degC, 1.4e-7).
    alpha : float
        Thermal expansion coefficient in K^-1 (default: water at 20 degC, 2e-4).

    Returns
    -------
    dict
        Keys:
        - "PARM01"     : dict of core namelist parameters (f0, beta, viscAh,
                         viscAz, diffKhT, diffKzT).
        - "EOS_PARM01" : dict of EOS namelist parameters (eosType, tAlpha, sBeta).
        - "PARM04"     : dict of grid parameters (delX, delY, delZ as available);
                         key is absent if none of Nx, Ny, Nz are given.
        - "derived"    : dict of computed intermediate values (f0, L,
                         aspect_ratio, dx, dy, dz).
        - "notes"      : list of advisory strings.
    """
    # Attach pint units to inputs.
    _Lx = Lx * ureg.meter
    _Ly = Ly * ureg.meter
    _depth = depth * ureg.meter
    _Omega = Omega * ureg.radian / ureg.second
    _nu = nu * ureg.meter ** 2 / ureg.second
    _kappa = kappa * ureg.meter ** 2 / ureg.second
    _alpha = alpha / ureg.kelvin

    # Characteristic horizontal scale.
    _L = min(_Lx, _Ly)

    # Coriolis parameter.
    _f0 = 2.0 * _Omega

    # Aspect ratio (dimensionless).
    _aspect_ratio = _depth / _L

    notes: list[str] = []

    # Advisory notes.
    if _aspect_ratio.magnitude > 0.1:
        notes.append(
            f"Aspect ratio {_aspect_ratio.magnitude:.2f} — consider nonHydrostatic = .TRUE. in PARM01"
        )

    if Lx != Ly:
        _L_val = _L.to(ureg.meter).magnitude
        notes.append(
            f"Rectangular tank ({Lx} m \u00d7 {Ly} m); L = min(Lx, Ly) = {_L_val} m used as horizontal scale"
        )

    # Grid spacings (optional).
    parm04: dict = {}
    _dx = None
    _dy = None
    _dz = None

    if Nx is not None:
        _dx = _Lx / Nx
        parm04["delX"] = _dx.to(ureg.meter).magnitude

    if Ny is not None:
        _dy = _Ly / Ny
        parm04["delY"] = _dy.to(ureg.meter).magnitude

    if Nz is not None:
        _dz = _depth / Nz
        parm04["delZ"] = _dz.to(ureg.meter).magnitude

    # Strip units for output.
    f0_val = _f0.to(1 / ureg.second).magnitude

    result: dict = {
        "PARM01": {
            "f0": f0_val,
            "beta": 0.0,
            "viscAh": _nu.to(ureg.meter ** 2 / ureg.second).magnitude,
            "viscAz": _nu.to(ureg.meter ** 2 / ureg.second).magnitude,
            "diffKhT": _kappa.to(ureg.meter ** 2 / ureg.second).magnitude,
            "diffKzT": _kappa.to(ureg.meter ** 2 / ureg.second).magnitude,
        },
        "EOS_PARM01": {
            "eosType": "LINEAR",
            "tAlpha": _alpha.to(1 / ureg.kelvin).magnitude,
            "sBeta": 0.0,
        },
        "derived": {
            "f0": f0_val,
            "L": _L.to(ureg.meter).magnitude,
            "aspect_ratio": _aspect_ratio.to(ureg.dimensionless).magnitude,
            "dx": _dx.to(ureg.meter).magnitude if _dx is not None else None,
            "dy": _dy.to(ureg.meter).magnitude if _dy is not None else None,
            "dz": _dz.to(ureg.meter).magnitude if _dz is not None else None,
        },
        "notes": notes,
    }

    if parm04:
        result["PARM04"] = parm04

    return result

# M5 — Domain Knowledge Skeleton

## Context

M5 adds a `src/domain/` layer that answers "how do I configure MITgcm for a rotating tank?"
The code-graph and semantic index (M1–M4) answer *how MITgcm works*; domain knowledge
answers *how to configure it for a specific experiment class*.

Design sketch: `plans/mitgcm-tank-knowledge-system.md`.

---

## Files to create

```
src/domain/
├── __init__.py          re-exports public functions
├── translate.py         translate_lab_params
├── scales.py            check_scales
├── gotcha.py            lookup_gotcha + static catalogue
└── suggest.py           suggest_experiment_config

tests/domain/
├── __init__.py
├── test_translate.py
├── test_scales.py
├── test_gotcha.py
└── test_suggest.py

docs/domain-knowledge.md
```

`src/server.py` gets four new MCP tools.

---

## Function specifications

### `translate_lab_params`

```python
def translate_lab_params(
    Lx: float,                  # m — tank length in x (diameter for cylindrical)
    Ly: float,                  # m — tank length in y (diameter for cylindrical)
    depth: float,               # m — water depth
    Omega: float,               # rad/s — rotation rate (0 for non-rotating)
    delta_T: float | None = None,  # K — temperature contrast
    Nx: int | None = None,      # grid cells in x
    Ny: int | None = None,      # grid cells in y
    Nz: int | None = None,      # vertical grid cells
    nu: float = 1e-6,           # m²/s — kinematic viscosity (water at 20 °C)
    kappa: float = 1.4e-7,      # m²/s — thermal diffusivity (water at 20 °C)
    alpha: float = 2e-4,        # K⁻¹ — thermal expansion coefficient (water at 20 °C)
) -> dict:
```

Use pint internally for all intermediate calculations. Strip units before
populating the return dict (MCP/JSON boundary; plain floats only in output).

`L = min(Lx, Ly)` is the characteristic horizontal scale for aspect ratio and
dimensionless numbers.

Returns a dict with keys `"PARM01"`, `"EOS_PARM01"`, `"PARM04"` (each a dict of
namelist parameter → value), plus `"notes"` (list of strings) and `"derived"` (a
dict of plain-float quantities computed along the way: f0, dx, dy, dz, L, aspect_ratio).

Computed values:
- `f0 = 2 * Omega`
- `beta = 0.0`
- `viscAh = nu`, `viscAz = nu`
- `diffKhT = kappa`, `diffKzT = kappa`
- `eosType = 'LINEAR'`, `tAlpha = alpha`, `sBeta = 0.0`  (→ EOS_PARM01)
- If `Nx` given: `delX = Lx / Nx`
- If `Ny` given: `delY = Ly / Ny`
- If `Nz` given: `delZ = depth / Nz`
- Add note if `depth / min(Lx, Ly) > 0.1` recommending `nonHydrostatic = .TRUE.`

Do not emit grid parameters when the corresponding N is None. Always emit
f0/viscosity/EOS parameters.

### `check_scales`

```python
def check_scales(
    Lx: float,                  # m — tank length in x
    Ly: float,                  # m — tank length in y
    depth: float,               # m
    Omega: float,               # rad/s
    delta_T: float | None = None,   # K
    dx: float | None = None,    # m — horizontal grid spacing x
    dy: float | None = None,    # m — horizontal grid spacing y
    dz: float | None = None,    # m — vertical grid spacing
    dt: float | None = None,    # s — time step
    U: float | None = None,     # m/s — velocity scale (for Ro and CFL)
    nu: float = 1e-6,
    alpha: float = 2e-4,
) -> dict:
```

`L = min(Lx, Ly)` is used as the characteristic horizontal scale.
Use pint internally; strip units in the returned dict.

Returns `{"numbers": {...}, "flags": [...]}`.

Always computed (no optional inputs needed):
- `f0 = 2 * Omega`
- `Ek_v = nu / (f0 * depth**2)`  — vertical Ekman number (set to None if Omega==0)
- `aspect_ratio = depth / min(Lx, Ly)`
- `ekman_depth = sqrt(nu / f0)` if Omega > 0 else None
- `spin_up_periods = Ek_v**(-0.5)` — rotation periods to solid-body spin-up

Computed if `delta_T` given:
- `N = sqrt(alpha * 9.81 * delta_T / depth)` — buoyancy frequency
- `Bu = (N * depth / (f0 * min(Lx, Ly)))**2`   — Burger number (L = min(Lx, Ly))

Computed if `U` given:
- `Ro = U / (f0 * min(Lx, Ly))`

Computed if `dx` or `dy`, plus `dt` and `U`, all given:
- `CFL_h = U * dt / min(dx, dy)`  (use whichever of dx/dy is provided)
- `CFL_v = U * dt / dz`  (if dz and dt and U given)

Flags (list of `{"level": "warning"|"info", "message": str}`):
- aspect_ratio > 0.1 → warn non-hydrostatic effects significant
- Ek_v > 0.1 → warn rotationally dominated flow unlikely
- ekman_depth > dz → warn Ekman layer not resolved (if dz given)
- ekman_depth > depth → warn Ekman fills entire column
- CFL_h > 0.5 or CFL_v > 0.5 → warn CFL exceeded
- spin_up_periods > 50 → info (many rotation periods needed for spin-up)

### `lookup_gotcha`

```python
def lookup_gotcha(topic: str) -> list[dict]:
```

Case-insensitive keyword search over a static catalogue in `gotcha.py`.
Returns list of `{"title": str, "keywords": list[str], "summary": str, "detail": str}`.
Empty list if no match.

Initial catalogue (7 entries from the design doc, plus any others known):
1. `nonhydrostatic` — CPP flag + useNHMTerms
2. `linear eos freshwater` — sBeta = 0, eosType, data.eos file
3. `spin-up time` — O(Ek^{-1/2}) rotation periods
4. `sidewall no-slip` — no_slip_sides default
5. `pressure reference` — EosRefP0 for shallow tanks
6. `rigid lid vs free surface` — rigidLid vs implicitFreeSurface
7. `diagnostics frequency` — seconds vs time steps in data.diagnostics

Match on any keyword appearing in the query string.

### `suggest_experiment_config`

```python
def suggest_experiment_config(experiment_type: str) -> dict | None:
```

Returns a skeleton config dict for known experiment types, or None if unknown.
Keys: `"experiment_type"`, `"description"`, `"cpp_options"` (list of flags to
`#define`), `"namelists"` (dict of namelist-file → dict of group → dict of
param → value), `"notes"` (list of strings).

Two configurations for M5:
- `"rotating_convection"` — non-hydrostatic, f-plane, surface buoyancy flux,
  no-slip sidewalls and bottom, linear EOS
- `"baroclinic_instability"` — two-layer stratification, Eady-like setup,
  f-plane, free-slip or no-slip sidewalls

Match `experiment_type` case-insensitively; also match common aliases
(`"eady"` → baroclinic_instability, `"convection"` → rotating_convection).

---

## MCP tools (additions to `src/server.py`)

```python
translate_lab_params_tool(tank_radius, depth, Omega, delta_T=None, Nx=None, Nz=None, nu=1e-6, kappa=1.4e-7, alpha=2e-4)
check_scales_tool(tank_radius, depth, Omega, delta_T=None, dx=None, dz=None, dt=None, U=None, nu=1e-6, alpha=2e-4)
lookup_gotcha_tool(topic)
suggest_experiment_config_tool(experiment_type)
```

Update the test in `tests/test_server.py` that counts expected tools (currently 10 → 14).

---

## Tests

Unit tests only — no external dependencies. All pure computation and static data.

| File | What to test |
|---|---|
| `test_translate.py` | f0 formula; Omega=0 gives f0=0; Nx/Nz absent → no delX/delZ; note emitted for high aspect ratio; EOS params present |
| `test_scales.py` | Ek formula; flags triggered at known thresholds; Omega=0 gives None for rotation numbers; delta_T=None skips Bu/N; CFL skipped without dt+U |
| `test_gotcha.py` | "nonhydrostatic" matches entry 1; unknown topic returns []; case-insensitive; partial word match |
| `test_suggest.py` | "rotating_convection" returns dict with cpp_options; "eady" alias works; unknown type returns None |

---

## Roadmap tick-off

After implementation, mark M5 checklist complete in `plans/roadmap.md`.

---

## Done-when

```python
from src.domain import translate_lab_params, check_scales

# Cylindrical tank: Lx = Ly = diameter = 2 * 0.4 m
params = translate_lab_params(Lx=0.8, Ly=0.8, depth=0.2, Omega=0.314, delta_T=2.0, Nx=80, Ny=80, Nz=20)
scales = check_scales(Lx=0.8, Ly=0.8, depth=0.2, Omega=0.314, delta_T=2.0,
                      dx=params["derived"]["dx"], dy=params["derived"]["dy"],
                      dz=params["derived"]["dz"])
# scales["numbers"]["Ek_v"] ~ 4e-4
# scales["flags"] contains aspect-ratio warning
```

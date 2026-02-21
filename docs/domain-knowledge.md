# Domain Knowledge Layer

The domain knowledge layer (`src/domain/`) provides tank-experiment-specific
reasoning on top of the MITgcm code graph and semantic index.  Where the
code-graph tools answer *how MITgcm works*, the domain knowledge tools
answer *how to configure it for a rotating-tank experiment*.

---

## Module layout

```
src/domain/
├── __init__.py      re-exports public functions
├── translate.py     translate_lab_params — lab geometry → namelist values
├── scales.py        check_scales — dimensionless numbers and regime flags
├── gotcha.py        lookup_gotcha — static gotcha catalogue + keyword search
└── suggest.py       suggest_experiment_config — skeleton configs
```

---

## Functions

### `translate_lab_params`

Translates physical lab quantities to MITgcm namelist parameters.

```python
from src.domain import translate_lab_params

result = translate_lab_params(
    Lx: float,          # m  — tank length in x (diameter for cylindrical)
    Ly: float,          # m  — tank length in y (diameter for cylindrical)
    depth: float,       # m  — water depth
    Omega: float,       # rad/s — rotation rate (0 for non-rotating)
    delta_T: float | None = None,  # K — temperature contrast (optional)
    Nx: int | None = None,         # grid cells in x
    Ny: int | None = None,         # grid cells in y
    Nz: int | None = None,         # vertical grid cells
    nu: float = 1e-6,              # m²/s — kinematic viscosity
    kappa: float = 1.4e-7,         # m²/s — thermal diffusivity
    alpha: float = 2e-4,           # K⁻¹ — thermal expansion coefficient
) -> dict
```

The characteristic horizontal scale is `L = min(Lx, Ly)`.

Return structure:

```python
{
    "PARM01": {
        "f0": float,        # 2 * Omega,  s⁻¹
        "beta": 0.0,
        "viscAh": float,    # nu,  m²/s
        "viscAz": float,    # nu,  m²/s
        "diffKhT": float,   # kappa,  m²/s
        "diffKzT": float,   # kappa,  m²/s
    },
    "EOS_PARM01": {
        "eosType": "LINEAR",
        "tAlpha": float,    # alpha,  K⁻¹
        "sBeta": 0.0,
    },
    "PARM04": {             # present only if any of Nx/Ny/Nz given
        "delX": float,      # Lx / Nx,  m  (only if Nx given)
        "delY": float,      # Ly / Ny,  m  (only if Ny given)
        "delZ": float,      # depth / Nz,  m  (only if Nz given)
    },
    "derived": {
        "f0": float,
        "L": float,           # min(Lx, Ly),  m
        "aspect_ratio": float,  # depth / L
        "dx": float | None,
        "dy": float | None,
        "dz": float | None,
    },
    "notes": list[str],     # advisory strings
}
```

Advisory notes are added when:
- `depth / min(Lx, Ly) > 0.1` — recommends `nonHydrostatic = .TRUE.`
- `Lx != Ly` — notes that `L = min(Lx, Ly)` is used as the horizontal scale

pint is used internally for all physics calculations.  The returned dict
contains plain floats only — no pint Quantities — making it safe to
serialise to JSON or pass over MCP.

---

### `check_scales`

Computes dimensionless numbers and raises flags for a proposed configuration.

```python
from src.domain import check_scales

result = check_scales(
    Lx: float,          # m  — tank length in x
    Ly: float,          # m  — tank length in y
    depth: float,       # m  — water depth
    Omega: float,       # rad/s — rotation rate
    delta_T: float | None = None,  # K — temperature contrast (for N, Bu)
    dx: float | None = None,       # m — horizontal grid spacing x (for CFL, Ek resolution)
    dy: float | None = None,       # m — horizontal grid spacing y (for CFL)
    dz: float | None = None,       # m — vertical grid spacing (for CFL_v, Ek resolution)
    dt: float | None = None,       # s — time step (for CFL)
    U: float | None = None,        # m/s — velocity scale (for Ro, CFL)
    nu: float = 1e-6,              # m²/s — kinematic viscosity
    alpha: float = 2e-4,           # K⁻¹ — thermal expansion coefficient
) -> dict
```

`L = min(Lx, Ly)` is the characteristic horizontal scale.

Return structure:

```python
{
    "numbers": {
        "f0": float,                     # 2 * Omega,  s⁻¹  (always)
        "aspect_ratio": float,           # depth / L  (always)
        "Ek_v": float | None,            # nu / (f0 * depth²)  (None when Omega=0)
        "ekman_depth": float,            # sqrt(nu / f0),  m  (when Omega > 0)
        "spin_up_periods": float,        # Ek_v^{-0.5}  (when Omega > 0)
        "Ro": float,                     # U / (f0 * L)  (when U and Omega given)
        "N": float,                      # sqrt(alpha * g * delta_T / depth),  s⁻¹  (when delta_T and Omega given)
        "Bu": float,                     # (N * depth / (f0 * L))²  (when delta_T and Omega given)
        "CFL_h": float,                  # U * dt / min(dx, dy)  (when U, dt, and dx or dy given)
        "CFL_v": float,                  # U * dt / dz  (when U, dt, dz given)
    },
    "flags": [
        {"level": "warning" | "info", "message": str},
        ...
    ],
}
```

Flags are raised when:
- `aspect_ratio > 0.1` — non-hydrostatic effects likely
- `Ek_v > 0.1` — viscous effects dominate rotation
- `ekman_depth > dz` — Ekman layer not vertically resolved
- `ekman_depth > depth` — entirely viscous column
- `CFL_h > 0.5` or `CFL_v > 0.5` — time step too large
- `spin_up_periods > 50` — long spin-up time (info level)

---

### `lookup_gotcha`

Case-insensitive keyword search over a static catalogue of known
configuration traps for rotating-tank MITgcm experiments.

```python
from src.domain import lookup_gotcha

entries = lookup_gotcha(topic: str) -> list[dict]
```

Each returned entry has keys: `title`, `keywords`, `summary`, `detail`.

The catalogue contains seven entries covering:
1. Non-hydrostatic pressure solve — CPP flag + `useNHMTerms`
2. Linear EOS in freshwater — `sBeta = 0` in `data.eos`
3. Spin-up time — O(Ek^{-1/2}) rotation periods
4. Sidewall boundary conditions — `no_slip_sides` default
5. Pressure reference level — `EosRefP0` for shallow tanks
6. Rigid lid vs free surface — `rigidLid` vs `implicitFreeSurface`
7. Diagnostics output frequency — seconds, not time steps

Any keyword phrase from a catalogue entry that appears in the lowercased
query string triggers a match.  Returns an empty list if no entry matches.

---

### `suggest_experiment_config`

Returns a skeleton MITgcm configuration dict for a known experiment class.

```python
from src.domain import suggest_experiment_config

config = suggest_experiment_config(experiment_type: str) -> dict | None
```

Recognised experiment types:

| Key | Aliases |
|---|---|
| `rotating_convection` | `convection`, `rotating convection` |
| `baroclinic_instability` | `eady`, `baroclinic` |

Lookup is case-insensitive.  Returns `None` for unrecognised types.

Return structure:

```python
{
    "experiment_type": str,
    "description": str,
    "cpp_options": list[str],   # flags to #define in CPP_OPTIONS.h
    "namelists": {
        "<namelist-file>": {
            "<group>": {
                "<param>": <value>,
                ...
            },
        },
        ...
    },
    "notes": list[str],
}
```

Placeholder strings (e.g. `"<from translate_lab_params>"`) mark parameters
that must be filled from `translate_lab_params` output or chosen to satisfy
CFL before use.

---

## MCP tools

Four MCP tools wrap the domain functions in `src/server.py`:

| Tool | Wraps |
|---|---|
| `translate_lab_params_tool` | `translate_lab_params` |
| `check_scales_tool` | `check_scales` |
| `lookup_gotcha_tool` | `lookup_gotcha` |
| `suggest_experiment_config_tool` | `suggest_experiment_config` |

All parameters use plain Python types (float, int, str, None).  The tools
accept the same parameters as the underlying functions.  Units for every
parameter are documented in the tool docstrings so the LLM has them in
the tool description.

---

## Done-when verification

```python
from src.domain import translate_lab_params, check_scales

# Cylindrical tank: Lx = Ly = diameter = 2 * 0.4 m
params = translate_lab_params(Lx=0.8, Ly=0.8, depth=0.2, Omega=0.314, delta_T=2.0, Nx=80, Ny=80, Nz=20)
scales = check_scales(
    Lx=0.8, Ly=0.8, depth=0.2, Omega=0.314, delta_T=2.0,
    dx=params["derived"]["dx"],
    dy=params["derived"]["dy"],
    dz=params["derived"]["dz"],
)
# scales["numbers"]["Ek_v"] ~ 4e-5
# scales["flags"] contains aspect-ratio warning (depth/L = 0.25 > 0.1)
```

Expected outputs:
- `params["PARM01"]["f0"]` ≈ 0.628 (= 2 × 0.314)
- `scales["numbers"]["Ek_v"]` ≈ 3.98×10⁻⁵ (= 1×10⁻⁶ / (0.628 × 0.04))
- `scales["flags"]` contains at least one warning about aspect ratio

---

## Implementation notes

- pint (`pint.UnitRegistry`) is used internally in `translate.py` and
  `scales.py` for all physics calculations.  Units are stripped (`.magnitude`)
  before populating any returned dict — MCP and JSON cannot carry pint
  Quantities.
- The MCP interface accepts plain floats for all physical parameters.
- The gotcha catalogue and experiment configs are static Python data
  structures; no external files or databases are required.

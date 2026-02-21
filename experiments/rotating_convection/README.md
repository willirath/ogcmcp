# Rotating convection experiment

A rotating convection experiment in a 1 m tabletop tank, configured end-to-end
using the MITgcm MCP tools. The goal was to simulate bottom-forced convection
under rotation and observe how the resulting plume structures are controlled by
the Rossby deformation radius.

![Temperature cross-sections at t = 360 s](T_section.png)

---

## Motivation

The original setup request:

> Cylindrical tank. Rotating clockwise at 10 rpm. Water depth is 1 cm in the
> centre of the cylinder and 12 cm at the edge. Radius of tank is 50 cm.
> Initial condition is stratified with linear temperature from 20 °C at
> surface to 0 °C at bottom. Cooled from below, assume infinite heat bath
> outside of the tank.

The system used `translate_lab_params`, `check_scales`, and
`suggest_experiment_config` to derive namelist parameters, then iterated on
the configuration until the model ran stably.

---

## Physical setup

| Parameter | Value |
|---|---|
| Tank diameter | 1 m |
| Rotation rate | 10 rpm clockwise (f₀ = −2.094 rad/s) |
| Bottom depth at centre | 1 cm |
| Bottom depth at edge | 12 cm |
| Bottom shape | Parabolic |
| Initial T | Linear, 20 °C (surface) → 0 °C (at edge depth) |
| Bottom forcing | RBCS restoring to 0 °C, τ = 1 s at bottom cell |
| Simulation duration | 360 s (6 min, 60 rotations) |

---

## Scale analysis

Computed with `check_scales` from the domain knowledge layer:

| Number | Value | Interpretation |
|---|---|---|
| Ω | 1.047 rad/s | 10 rpm |
| N (buoyancy freq.) | 0.572 rad/s | From ΔT = 20 K over H = 12 cm |
| Rossby deformation radius Ld | 3.3 cm | Controls plume width |
| Rossby number Ro | 0.066 | Rotation-dominated flow |
| Ekman number Ek_h | 1.9 × 10⁻⁵ | Weak horizontal friction |
| Ekman number Ek_v | 1.7 × 10⁻³ | Moderate vertical friction |
| Burger number Bu | 0.004 | Strongly rotationally controlled |

The small Burger number (Bu ≪ 1) and Ld ≈ 3 cm indicate that convective
plumes should be narrow and strongly influenced by rotation — the classical
geostrophic turbulence regime.

---

## Parameter choices

**Grid:** 60 × 60 × 40 cells over 1 m × 1 m × 12 cm (Δx = Δy = 1.67 cm,
Δz = 3 mm). Horizontal resolution ~Ld/2, sufficient to resolve plumes.

**Timestep:** Δt = 0.05 s. Initially set to 0.1 s but reduced after a
vertical CFL instability caused NaN blow-up (convective velocities ~2 cm/s
exceed the w·Δt/Δz < 0.5 limit with Δt = 0.1 s).

**Advection scheme:** `tempAdvScheme = 33` (PPM monotone). Prevents
spurious oscillations near sharp fronts in the convective plumes.

**Viscosity:** νh = 10⁻⁵ m²/s, νz = 5 × 10⁻⁵ m²/s. Chosen to damp
grid-scale noise while keeping Ek_h small.

**Equation of state:** Linear, α = 2 × 10⁻⁴ K⁻¹.

---

## Gotchas encountered during setup

**1. RBCS mask is dimensionless (0–1); `tauRelaxT` must be non-zero.**
The mask file holds weights, not rates. The actual restoring rate is
`mask / tauRelaxT`. Setting `tauRelaxT = 0` causes division by zero;
omitting it leaves RBCS inactive. Set `tauRelaxT = 1.` (1 s restoring at
bottom) and mask = 1.0 at the bottom wet cell of each column.

**2. `DIAGNOSTICS_SIZE.h` `numDiags` must cover all requested fields.**
With the default `numDiags`, adding diagnostics output fields causes a
runtime abort: `STOP in DIAGNOSTICS_SETPARMS: numDiags too small`. Increase
`numDiags` in `code/DIAGNOSTICS_SIZE.h` to at least the number of scalar
diagnostic fields requested.

**3. Vertical CFL instability with explicit advection.**
Convective velocities (~2 cm/s) combined with the thin bottom layers
(Δz = 3 mm) yield w·Δt/Δz > 0.5 at Δt = 0.1 s. Symptoms: NaN in the
temperature field within the first few time steps. Fix: halve Δt and switch
to a monotone advection scheme (`tempAdvScheme = 33`).

---

## Files

```
experiments/rotating_convection/
├── gen.py              Generate binary input files (bathy, T, RBCS)
├── plot.py             Cross-section visualisation (requires a completed run)
├── T_section.png       Output from a completed run (360 s)
├── code/               MITgcm source modifications
│   ├── CPP_OPTIONS.h
│   ├── DIAGNOSTICS_SIZE.h
│   ├── SIZE.h          (nPx=2 for 2-process MPI run)
│   └── packages.conf
└── input/              Namelists and binary input files
    ├── data            Main namelist
    ├── data.diagnostics
    ├── data.mnc
    ├── data.pkg
    ├── data.rbcs
    ├── eedata
    ├── bathy.bin       Parabolic bathymetry
    ├── init_T.bin      Initial temperature
    ├── rbcs_T.bin      RBCS target temperature
    └── rbcs_mask.bin   RBCS relaxation mask
```

---

## How to run

### Regenerate input files

Only needed if you modify the geometry or forcing parameters in `gen.py`:

```bash
pixi run gen-rotating-convection
```

### Build and run with Docker

Requires the `mitgcm:latest` image (`pixi run build-image`):

```bash
pixi run build-rotating-convection   # compile MITgcm with this experiment's code
pixi run run-rotating-convection     # run for 360 s (7200 × 0.05 s steps)
```

Wall time on a MacBook Pro (M-series, 2 MPI processes via Rosetta): ~4 min.
Output is written to `experiments/rotating_convection/run/mnc_out_*/`.

### Visualise

```bash
pixi run python experiments/rotating_convection/plot.py
```

Produces `T_section.png` showing the final-time temperature field as a
meridional cross-section and an azimuthal mean.

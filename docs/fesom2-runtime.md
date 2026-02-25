# FESOM2 runtime Docker image

Self-contained Docker image for running FESOM2 toy experiments on a laptop.
Mesh files, namelists, and output each live in their own directory — all
three are mounted at runtime; nothing is baked into the image.

## Usage

```bash
EXP=/path/to/experiment
mkdir -p "$EXP/output"

docker run --rm \
  -v "$EXP/mesh:/mesh:ro" \
  -v "$EXP/input:/input:ro" \
  -v "$EXP/output:/output" \
  fesom2:latest
```

All experiment parameters (`run_length`, `step_per_day`, `n_part`,
`which_toy`, …) are read from the committed `input/` namelists. The
entrypoint patches only the three container-internal paths and writes
`fesom.clock`.

## What the entrypoint does

1. Validates that `/mesh`, `/input`, `/output` are mounted and
   `/input/namelist.config` exists
2. Copies `/input/namelist.*` to a temporary work directory
3. Patches three paths in the copied `namelist.config`:
   - `MeshPath → '/mesh/'`
   - `ResultPath → '/output/'`
   - `ClimateDataPath → '/dev/null/'`
4. Reads `timenew`, `daynew`, `yearnew` from `&clockinit` and writes
   `/output/fesom.clock` (two identical lines = cold start)
5. Reads `n_part` from the patched `namelist.config`
6. Runs `mpirun --allow-run-as-root -np <n_part> /fesom2/bin/fesom.x`

## Experiment directory layout

```
experiments/fesom2/<name>/
├── mesh/
│   ├── nod2d.out          node coordinates
│   ├── elem2d.out         triangular connectivity
│   ├── aux3d.out          level depths + bathymetry
│   ├── windstress.out     toy wind stress (when wind_opt=1)
│   ├── edges.out          }
│   ├── edge_tri.out       } derived geometry (pre-computed by fesom_ini.x)
│   ├── edgenum.out        }
│   ├── elvls.out          }
│   ├── nlvls.out          }
│   ├── dist_2/            METIS partition for 2 ranks
│   └── create_mesh.py     mesh provenance script
├── input/
│   ├── namelist.config    MeshPath/ResultPath/ClimateDataPath left as '' (patched at runtime)
│   ├── namelist.oce
│   ├── namelist.tra
│   ├── namelist.dyn
│   ├── namelist.cvmix
│   ├── namelist.ice
│   ├── namelist.io
│   └── namelist.forcing
├── output/                gitignored — created at runtime
├── run.sh                 docker run command
└── plot.py                visualisation script
```

## Build

```bash
# Local (for testing on the native platform)
docker build --platform linux/arm64 -t fesom2:test -f docker/fesom2/Dockerfile .

# Multi-arch via pixi task (amd64 + arm64)
pixi run build-fesom2-image
```

Build time: ~50 s (FESOM2 compilation dominates; apt packages are cached
across rebuilds). Image size: ~1.4 GB (gfortran + OpenMPI + NetCDF +
FESOM2 binary).

## Notes

- The entrypoint never overrides `run_length`, `step_per_day`, `n_part`,
  `which_toy`, or any physics parameter. Everything lives in the committed
  `input/` namelists.
- `toy_ocean=.true.` bypasses the bulk forcing pipeline. `namelist.forcing`
  must still exist (FESOM2 opens it unconditionally) but its contents are
  ignored.
- For real (NetCDF-forced) experiments add a fourth mount:
  `-v /path/to/forcing:/forcing:ro` and set forcing paths in
  `namelist.forcing` to `/forcing/<prefix>`.
- The image runs as root (required for `--allow-run-as-root` with OpenMPI
  inside Docker). Safe for isolated laptop use.

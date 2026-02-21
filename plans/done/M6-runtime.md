# M6 — MITgcm Runtime Environment

## Purpose

Provide a reproducible, compile-on-demand build and run environment for
MITgcm experiments with real MPI parallelism from the start. The container
is the toolchain; the experiment directory is the workspace. Works locally
via Docker; translates directly to Singularity on HPC.

---

## CI image review findings

MITgcm's own CI uses `mitgcm/testreport-images:ubuntu_18_04_villon` —
Ubuntu 18.04, EOL, no MPI, serial runs only. Their `testreport` harness
wraps genmake2/make/run but doesn't exercise MPI communication. We go
beyond this from the start.

Key quirks from the `linux_amd64_gfortran` optfile (the one CI uses, which
we reuse):
- `MPI=true` env var switches compilers from `gfortran`→`mpif77`/`mpif90`
  and `gcc`→`mpicc`. Must be set before calling genmake2 and make.
- `-fconvert=big-endian` — in the optfile, required for MITgcm's raw binary
  I/O (`.bin` files). Non-negotiable.
- `-fallow-argument-mismatch` — added for gfortran ≥ 10 by the optfile's
  version detection. A modern Debian/Ubuntu base (gfortran 12+) needs this
  or the build fails on MITgcm's F77-style mixed-type calls.
- `-mcmodel=medium` — in the optfile; needed for large grids.

We reuse `linux_amd64_gfortran` as-is; no custom optfile needed.

---

## Design

### Container role

The image provides the toolchain only:
- gfortran, OpenMPI, NetCDF-Fortran
- No MITgcm source baked in (mounted from the submodule)
- No experiment files

### Experiment directory layout

```
experiments/<name>/
├── code/           committed — genmake2 -mods source
│   ├── SIZE.h
│   ├── CPP_OPTIONS.h
│   ├── packages.conf
│   └── <custom .F overrides>
├── input/          text files committed; .bin files NOT committed
│   ├── data
│   ├── data.pkg
│   ├── eedata
│   └── *.bin       gitignored — copied from submodule or generated
├── build/          gitignored — genmake2/make output
└── run/            gitignored — mitgcmuv output
```

### Volume mounts (`docker run`)

| Host path | Container path | Purpose |
|---|---|---|
| `./MITgcm` | `/MITgcm:ro` | Source tree (read-only) |
| `./experiments/<name>` | `/experiment` | Experiment workspace (read-write) |

Container runs as the host user (`--user $(id -u):$(id -g)`) so output
files are owned correctly.

### MPI: two processes from the start

`SIZE.h` encodes MPI layout at compile time via `nPx`/`nPy`. The done-when
experiment uses `nPx=2, nSx=2` so two MPI processes each own two tiles.
Run with `mpirun --allow-run-as-root -np ${MITGCM_NP:-2}` (the
`--allow-run-as-root` flag is needed when Docker runs as root, which is the
default; our `--user` flag avoids this on Linux but may be needed on some
setups — include it unconditionally for robustness).

### Build step (inside container)

```sh
mkdir -p /experiment/build
cd /experiment/build
MPI=true /MITgcm/tools/genmake2 \
    -mods /experiment/code \
    -optfile /MITgcm/tools/build_options/linux_amd64_gfortran \
    -mpi
make depend
make -j$(nproc)
```

Output: `/experiment/build/mitgcmuv`

### Run step (inside container)

```sh
mkdir -p /experiment/run
cd /experiment/run
ln -sf /experiment/build/mitgcmuv .
ln -sf /experiment/input/* .
mpirun --allow-run-as-root -np ${MITGCM_NP:-2} ./mitgcmuv
```

---

## Files to create

### `Dockerfile`

```dockerfile
FROM ubuntu:24.04
RUN apt-get update && apt-get install -y --no-install-recommends \
    gfortran \
    libopenmpi-dev \
    openmpi-bin \
    libnetcdf-dev \
    libnetcdff-dev \
    make \
    perl \
    && rm -rf /var/lib/apt/lists/*
```

Single stage — no build/runtime split needed for a science container.

On Apple Silicon the host runs Linux/amd64 via Rosetta. Add
`--platform linux/amd64` to `docker build` and `docker run` calls, or set
`DOCKER_DEFAULT_PLATFORM=linux/amd64` in the environment. Document this.

### `scripts/build-experiment.sh`

```sh
#!/usr/bin/env bash
set -euo pipefail
EXP=${1:?Usage: build-experiment.sh <experiment-dir>}
EXP_ABS=$(realpath "$EXP")
REPO=$(realpath "$(dirname "$0")/..")
docker run --rm \
  --platform linux/amd64 \
  --user "$(id -u):$(id -g)" \
  -v "$REPO/MITgcm:/MITgcm:ro" \
  -v "$EXP_ABS:/experiment" \
  mitgcm:latest bash -c "
    mkdir -p /experiment/build && cd /experiment/build &&
    MPI=true /MITgcm/tools/genmake2 \
      -mods /experiment/code \
      -optfile /MITgcm/tools/build_options/linux_amd64_gfortran \
      -mpi &&
    make depend && make -j\$(nproc)"
```

### `scripts/run-experiment.sh`

```sh
#!/usr/bin/env bash
set -euo pipefail
EXP=${1:?Usage: run-experiment.sh <experiment-dir>}
EXP_ABS=$(realpath "$EXP")
REPO=$(realpath "$(dirname "$0")/..")
NP=${MITGCM_NP:-2}
docker run --rm \
  --platform linux/amd64 \
  --user "$(id -u):$(id -g)" \
  -v "$REPO/MITgcm:/MITgcm:ro" \
  -v "$EXP_ABS:/experiment" \
  mitgcm:latest bash -c "
    mkdir -p /experiment/run && cd /experiment/run &&
    ln -sf /experiment/build/mitgcmuv . &&
    ln -sf /experiment/input/* . &&
    mpirun --allow-run-as-root -np $NP ./mitgcmuv"
```

### `scripts/setup-tutorial.sh`

Copies binary input files from the MITgcm submodule into the committed
experiment directory (`.bin` files are gitignored in `experiments/`):

```sh
#!/usr/bin/env bash
set -euo pipefail
REPO=$(realpath "$(dirname "$0")/..")
SRC="$REPO/MITgcm/verification/tutorial_rotating_tank/input"
DST="$REPO/experiments/tutorial_rotating_tank/input"
cp "$SRC/bathyPolR.bin" "$DST/"
cp "$SRC/thetaPolR.bin" "$DST/"
echo "Binary input files copied to $DST"
```

### `experiments/tutorial_rotating_tank/`

Committed files:

**`code/SIZE.h`** — modified from the tutorial for `nPx=2`:
```fortran
PARAMETER (
&           sNx =  30,
&           sNy =  23,
&           OLx =   3,
&           OLy =   3,
&           nSx =   2,   ! was 4
&           nSy =   1,
&           nPx =   2,   ! was 1
&           nPy =   1,
&           Nx  = sNx*nSx*nPx,
&           Ny  = sNy*nSy*nPy,
&           Nr  =  29)
```

`Nx = 30*2*2 = 120`, `Ny = 23*1*1 = 23` — unchanged from original.

**`code/CPP_OPTIONS.h`**, **`code/packages.conf`**, **`code/apply_forcing.F`**,
**`code/DIAGNOSTICS_SIZE.h`** — copied verbatim from
`MITgcm/verification/tutorial_rotating_tank/code/`.

**`input/data`**, **`input/data.pkg`**, **`input/data.mnc`**,
**`input/eedata`** — copied verbatim from
`MITgcm/verification/tutorial_rotating_tank/input/`.

**`input/bathyPolR.bin`**, **`input/thetaPolR.bin`** — gitignored; provided
by `scripts/setup-tutorial.sh`.

### `.gitignore` additions

```
experiments/*/input/*.bin
experiments/*/build/
experiments/*/run/
```

### `pixi.toml` additions

```toml
build-image       = "docker build --platform linux/amd64 -t mitgcm:latest ."
setup-tutorial    = "bash scripts/setup-tutorial.sh"
build-tutorial    = "bash scripts/build-experiment.sh experiments/tutorial_rotating_tank"
run-tutorial      = "bash scripts/run-experiment.sh experiments/tutorial_rotating_tank"
```

### `docs/runtime.md`

Cover:
- Prerequisites (Docker ≥ 20, `pixi run build-image`)
- Apple Silicon note (`--platform linux/amd64`, Rosetta)
- Experiment directory layout and the committed/gitignored split
- `pixi run setup-tutorial` → `build-tutorial` → `run-tutorial` workflow
- Singularity translation (drop-in: `singularity exec --bind` same mounts)
- Known limitation: SIZE.h is compiled in — different process counts or grid
  dimensions require a rebuild
- `MITGCM_NP` env variable to override process count

---

## Done-when

```sh
pixi run build-image
pixi run setup-tutorial
pixi run build-tutorial    # produces experiments/tutorial_rotating_tank/build/mitgcmuv
pixi run run-tutorial      # 20 time steps, mpirun -np 2, monitor output to stdout
```

Run completes without error and `experiments/tutorial_rotating_tank/run/`
contains `mnc_test_*/` NetCDF output files and pickup files.

---

## Singularity translation (HPC note)

```sh
# Build locally, export
docker save mitgcm:latest | gzip > mitgcm.tar.gz
singularity build mitgcm.sif docker-archive://mitgcm.tar.gz

# Run on HPC (no daemon)
singularity exec \
  --bind ./MITgcm:/MITgcm:ro \
  --bind ./experiments/my_exp:/experiment \
  mitgcm.sif bash /experiment/build.sh
```

---

## Open questions (not blocking M6)

- **Binary input generation**: for new experiments (M7), initial conditions
  and bathymetry are generated with numpy (`array.astype('>f4').tofile(path)`).
  The rotating tank tutorial shows the format: raw big-endian float32, shape
  `(Ny, Nx)` for bathy and `(Nr, Ny, Nx)` for 3-D fields.
- **MPI process count vs grid**: `nPx * nSx * sNx = Nx` must hold. For
  arbitrary experiments, the user chooses `nPx`/`nPy` and the build script
  uses `MITGCM_NP = nPx * nPy` automatically (M7 concern).
- **Diagnostics output**: the tutorial uses MNC. Plain binary pickup files
  and stdout monitor output are always produced regardless. Reading output
  is an M7 concern.

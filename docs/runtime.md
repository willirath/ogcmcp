# MITgcm Runtime Environment

## Purpose

Provides a reproducible, compile-on-demand build and run environment for MITgcm
experiments. The Docker image supplies the toolchain (gfortran, OpenMPI,
NetCDF-Fortran); the MITgcm source tree and experiment directories are mounted
at run time. The same image translates directly to Singularity on HPC.

## Prerequisites

- Docker >= 20 installed and running
- MITgcm submodule initialised (`git submodule update --init`)
- Image built: `pixi run build-image`

The `build-image` task runs:

```
docker build --platform linux/amd64 -t mitgcm:latest .
```

## Apple Silicon note

On Apple Silicon (M1/M2/M3) the Docker daemon runs Linux/amd64 images via
Rosetta 2. The `--platform linux/amd64` flag is set in every `docker build` and
`docker run` call in this project, so no extra configuration is required. The
MITgcm optfile `linux_amd64_gfortran` is used as-is.

Alternatively, set `DOCKER_DEFAULT_PLATFORM=linux/amd64` in your shell
environment to make the flag implicit across all Docker commands.

## Experiment directory layout

Each experiment lives under `experiments/<name>/`:

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
├── build/          gitignored — genmake2/make output, mitgcmuv binary
└── run/            gitignored — mitgcmuv runtime output
```

The `.bin` binary input files are gitignored. They are either copied from the
MITgcm verification directory (tutorial experiments) or generated from Python
scripts (custom experiments).

Volume mounts used by the scripts:

| Host path | Container path | Mode |
|---|---|---|
| `./MITgcm` | `/MITgcm` | read-only |
| `./experiments/<name>` | `/experiment` | read-write |

Containers run as the host user (`--user $(id -u):$(id -g)`) so output files
are owned by the calling user.

## Step-by-step workflow: tutorial_rotating_tank

### 1. Build the Docker image

```
pixi run build-image
```

This builds `mitgcm:latest` with gfortran, OpenMPI, and NetCDF-Fortran from
Ubuntu 24.04 packages.

### 2. Copy binary input files

```
pixi run setup-tutorial
```

Copies `bathyPolR.bin` and `thetaPolR.bin` from
`MITgcm/verification/tutorial_rotating_tank/input/` to
`experiments/tutorial_rotating_tank/input/`. These files are gitignored.

### 3. Build the experiment

```
pixi run build-tutorial
```

Runs inside the container:

```sh
mkdir -p /experiment/build && cd /experiment/build
MPI=true /MITgcm/tools/genmake2 \
    -mods /experiment/code \
    -optfile /MITgcm/tools/build_options/linux_amd64_gfortran \
    -mpi
make depend
make -j$(nproc)
```

Produces `experiments/tutorial_rotating_tank/build/mitgcmuv`.

The `linux_amd64_gfortran` optfile sets `-fconvert=big-endian` (required for
MITgcm's raw binary I/O), `-fallow-argument-mismatch` (needed for gfortran 10+
with MITgcm's F77-style mixed-type calls), and `-mcmodel=medium`. Setting
`MPI=true` switches the compilers from `gfortran` to `mpif77`/`mpif90`/`mpicc`.

### 4. Run the experiment

```
pixi run run-tutorial
```

Runs inside the container:

```sh
mkdir -p /experiment/run && cd /experiment/run
ln -sf /experiment/build/mitgcmuv .
ln -sf /experiment/input/* .
mpirun --allow-run-as-root -np 2 ./mitgcmuv
```

The `--allow-run-as-root` flag is included unconditionally for robustness; it
is required when Docker runs as root (the default on many systems) and harmless
when `--user` is effective.

## Run output

The run produces:

- **stdout** — MITgcm monitor output (per-timestep diagnostics, solver
  residuals) streamed to the terminal
- **`mnc_test_*/`** — NetCDF output files written by the MNC package; the
  directory name is set by `mnc_outdir_str` in `data.mnc`
- **pickup files** — `pickup.*.data` / `pickup.*.meta` in `run/`, written at
  intervals set by `pChkptFreq` in `data`

The tutorial runs 20 time steps (`nTimeSteps=20` in `data`).

## Overriding the MPI process count

The default is 2 processes, matching the `nPx=2` in `SIZE.h`. To override:

```
MITGCM_NP=4 pixi run run-tutorial
```

Note that `SIZE.h` encodes the MPI decomposition at compile time via `nPx` and
`nPy`. Changing the process count without a matching `SIZE.h` rebuild will cause
MITgcm to abort. See the known limitations section below.

## Singularity translation (HPC)

The Docker image can be exported and run under Singularity on HPC clusters that
do not provide a Docker daemon:

```sh
# On a workstation with Docker: export the image
docker save mitgcm:latest | gzip > mitgcm.tar.gz

# On HPC: convert to Singularity image format
singularity build mitgcm.sif docker-archive://mitgcm.tar.gz

# Run on HPC (no daemon required)
singularity exec \
  --bind ./MITgcm:/MITgcm:ro \
  --bind ./experiments/tutorial_rotating_tank:/experiment \
  mitgcm.sif bash -c "
    cd /experiment/run &&
    mpirun -np 2 ./mitgcmuv"
```

Singularity automatically runs as the calling user, so `--user` and
`--allow-run-as-root` are not needed.

## Known limitations

- **SIZE.h is compiled in.** The MPI decomposition (`nPx`, `nPy`) and tile
  dimensions (`sNx`, `nSx`) are baked into the binary. Changing the process
  count requires editing `SIZE.h` and rebuilding with `pixi run build-tutorial`.
- **Binary input files are not committed.** The `.bin` files in
  `experiments/*/input/` are gitignored and must be provided via
  `pixi run setup-tutorial` (for the tutorial) or generated by a script (for
  custom experiments). This keeps the repository free of large binary blobs.

# FESOM2 runtime Docker image

Self-contained Docker image for running FESOM2 toy experiments on a laptop.
No external data or separate FESOM2 installation required.

## What it includes

- FESOM2 binary compiled from the submodule source (`1b58e7f`)
- Toy-experiment meshes with pre-computed METIS partitions:
  - `neverworld2` — 8578 nodes, idealized channel (Neverworld2 configuration)
  - `soufflet` — 2875 nodes, idealized channel (Soufflet configuration)
- Toy-experiment namelist configs (from `FESOM2/config/`)
- All runtime dependencies (OpenMPI, NetCDF-Fortran, gfortran libs)

## Usage

```bash
docker run --rm -v $(pwd)/output:/output \
  ghcr.io/willirath/2026-mitgcm-mcp:fesom2-v2026.02.1 \
  [experiment] [nranks] [ndays]
```

| Argument | Default | Valid values |
|---|---|---|
| `experiment` | `neverworld2` | `neverworld2`, `soufflet` |
| `nranks` | `2` | `2`, `8` |
| `ndays` | `5` | any positive integer |

`nranks` is constrained to 2 or 8 — only those METIS partition files are
bundled. Changing `nranks` requires no re-partitioning; pick whatever fits
your laptop.

### Examples

```bash
# 5-day neverworld2 run, 2 MPI ranks (default)
mkdir output
docker run --rm -v $(pwd)/output:/output fesom2:latest

# 1-day neverworld2 run, 8 MPI ranks
docker run --rm -v $(pwd)/output:/output fesom2:latest neverworld2 8 1

# 3-day soufflet run, 2 MPI ranks
docker run --rm -v $(pwd)/output:/output fesom2:latest soufflet 2 3
```

## Output

Output NetCDF files and the updated `fesom.clock` are written to the
`/output` directory (your volume mount). The model writes whatever fields
are listed in `namelist.io` — the default config outputs SST, SSS, SSH,
MLD, surface stresses, 3-D T/S/u/v/w, and a restart file.

## Build

```bash
# Local single-arch (for testing)
docker build --platform linux/arm64 -t fesom2:test -f docker/fesom2/Dockerfile .

# Multi-arch via pixi task
pixi run build-fesom2-image
```

Build time: ~50 s (FESOM2 compilation dominates; apt packages are cached
across rebuilds). The image is ~1.4 GB (gfortran + OpenMPI + NetCDF +
FESOM2 source + binary + meshes).

## How it works

`entrypoint.sh`:

1. Validates `experiment`, `nranks`, `ndays`
2. Creates a temp working directory
3. Copies toy-specific namelists (`namelist.X.toy_<experiment>`) and shared
   namelists (`namelist.cvmix`, `namelist.ice`, `namelist.io`,
   `namelist.dyn`, `namelist.forcing`)
4. Patches `namelist.config`: sets `MeshPath`, `ResultPath`, `run_length`,
   `n_part`, `which_toy` for the requested experiment/run
5. Writes `fesom.clock` to `/output/` (cold-start clock file, two identical
   lines of `time day year` from `&clockinit`)
6. Runs `mpirun --allow-run-as-root -np <nranks> fesom.x`

## Notes

- The image runs as root (required for `--allow-run-as-root` with OpenMPI
  inside Docker). This is safe for isolated laptop use.
- `toy_ocean=.true.` bypasses the bulk forcing pipeline entirely — no
  atmospheric forcing data is needed or read.
- `fesom.clock` is always overwritten with a fresh cold-start clock on each
  run. To restart from a previous run, mount the previous output directory
  and set `restart_length_unit` in `namelist.config`.

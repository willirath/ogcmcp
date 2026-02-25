# FESOM2 laptop runtime image.
#
# Self-contained: compiles FESOM2 from the bundled submodule source at build
# time, bakes in toy-experiment meshes (neverworld2, soufflet) with their
# pre-computed METIS partitions, and runs via a simple entrypoint:
#
#   docker run --rm -v $(pwd)/output:/output fesom2 neverworld2 2 5
#                                                   ^experiment ^nranks ^ndays
#
# nranks must be 2 or 8 (only those METIS partitions are bundled).
# Multi-arch: supports linux/amd64 and linux/arm64.
FROM ubuntu:24.04@sha256:d1e2e92c075e5ca139d51a140fff46f84315c0fdce203eab2807c7e495eff4f9

RUN apt-get update && apt-get install -y --no-install-recommends \
    gfortran gcc g++ make cmake git ca-certificates \
    libopenmpi-dev openmpi-bin \
    libnetcdf-dev libnetcdff-dev \
    libblas-dev liblapack-dev \
    python3 \
    && rm -rf /var/lib/apt/lists/*

# Bake FESOM2 source from the submodule pinned at the indexed commit.
# The .git history is not needed at runtime.
COPY FESOM2/ /fesom2/

# Build: configure.sh sources env/ubuntu/shell (sets FC/CC/CXX to mpi wrappers)
# then calls cmake + make install. Binaries land in /fesom2/bin/:
#   fesom.x          — ocean model (run with mpirun)
#   fesom_meshpart   — METIS mesh partitioner (serial, no mpirun)
WORKDIR /fesom2
RUN bash -l configure.sh ubuntu -DBUILD_MESHPARTITIONER=ON

# Entrypoint
COPY docker/fesom2/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

WORKDIR /
ENTRYPOINT ["/entrypoint.sh"]
# Default: neverworld2, 2 MPI ranks, 5 days
CMD ["neverworld2", "2", "5"]

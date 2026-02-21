# MITgcm runtime image: gfortran + MPICH + NetCDF-Fortran.
#
# Uses Debian bookworm-slim + MPICH to avoid the ~180 MB of AMD ROCm/UCX
# libraries that Ubuntu 24.04's OpenMPI pulls in.
#
# Multi-arch: supports linux/amd64 and linux/arm64.
# The build script selects the correct MITgcm optfile per architecture.
FROM debian:bookworm-slim@sha256:98f4b71de414932439ac6ac690d7060df1f27161073c5036a7553723881bffbe
RUN apt-get update && apt-get install -y --no-install-recommends \
    gfortran \
    mpich \
    libmpich-dev \
    libnetcdf-dev \
    libnetcdff-dev \
    make \
    perl \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

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
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Bake MITgcm source at the exact commit used to build the MCP indices
# (checkpoint69k + 11 commits = decd05a). depth=12 reaches the commit
# via a shallow clone from the checkpoint69k tag; .git is stripped so
# it does not bloat the image layer.
RUN git clone --depth=12 --branch checkpoint69k \
      https://github.com/MITgcm/MITgcm.git /MITgcm \
    && git -C /MITgcm checkout decd05a68a16c9a10b05da054f140886b217a441 \
    && rm -rf /MITgcm/.git

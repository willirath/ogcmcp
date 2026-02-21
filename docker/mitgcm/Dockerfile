FROM ubuntu:24.04
RUN apt-get update && apt-get install -y --no-install-recommends \
    gfortran \
    libopenmpi-dev \
    openmpi-bin \
    libnetcdf-dev \
    libnetcdff-dev \
    make \
    perl \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

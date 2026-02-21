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

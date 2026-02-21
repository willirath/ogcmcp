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

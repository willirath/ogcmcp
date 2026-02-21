#!/usr/bin/env bash
set -euo pipefail
REPO=$(realpath "$(dirname "$0")/..")
SRC="$REPO/MITgcm/verification/tutorial_rotating_tank/input"
DST="$REPO/experiments/tutorial_rotating_tank/input"
cp "$SRC/bathyPolR.bin" "$DST/"
cp "$SRC/thetaPolR.bin" "$DST/"
echo "Binary input files copied to $DST"

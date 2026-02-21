#!/usr/bin/env bash
set -euo pipefail
EXP=${1:?Usage: build-experiment.sh <experiment-dir>}
EXP_ABS=$(realpath "$EXP")
REPO=$(realpath "$(dirname "$0")/..")

# Detect the architecture inside the container and select the matching
# MITgcm optfile and MPICH library path accordingly.
docker run --rm \
  --user "$(id -u):$(id -g)" \
  -v "$REPO/MITgcm:/MITgcm" \
  -v "$EXP_ABS:/experiment" \
  mitgcm:latest bash -c "
    MULTIARCH=\$(dpkg-architecture -qDEB_HOST_MULTIARCH)
    case \$(dpkg-architecture -qDEB_HOST_ARCH) in
      amd64) OPTFILE=linux_amd64_gfortran ;;
      arm64) OPTFILE=linux_arm64_gfortran ;;
      *)     echo 'Unsupported arch'; exit 1 ;;
    esac
    mkdir -p /experiment/build && cd /experiment/build &&
    MPI=true MPI_HOME=/usr/lib/\$MULTIARCH/mpich /MITgcm/tools/genmake2 \
      -rootdir /MITgcm \
      -mods /experiment/code \
      -optfile /MITgcm/tools/build_options/\$OPTFILE \
      -mpi &&
    make depend && make -j\$(nproc)"

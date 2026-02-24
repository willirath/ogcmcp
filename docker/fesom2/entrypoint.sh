#!/usr/bin/env bash
# Run a FESOM2 toy experiment.
#
# Usage: entrypoint.sh [experiment] [nranks] [ndays]
#   experiment : neverworld2 (default) | soufflet
#   nranks     : 2 (default) | 8
#   ndays      : 5 (default)
#
# Output files are written to /output (mount a host directory there).
set -euo pipefail

EXPERIMENT=${1:-neverworld2}
NRANKS=${2:-2}
NDAYS=${3:-5}

# --- Validate inputs ----------------------------------------------------------
case "$EXPERIMENT" in
    neverworld2) ;;
    soufflet)    ;;
    *) echo "ERROR: unknown experiment '$EXPERIMENT'. Valid: neverworld2, soufflet" >&2; exit 1 ;;
esac

case "$NRANKS" in
    2|8) ;;
    *) echo "ERROR: nranks must be 2 or 8 (pre-partitioned meshes only)" >&2; exit 1 ;;
esac

if ! [[ "$NDAYS" =~ ^[0-9]+$ ]] || [ "$NDAYS" -lt 1 ]; then
    echo "ERROR: ndays must be a positive integer" >&2; exit 1
fi

# --- Setup --------------------------------------------------------------------
MESH_DIR="/fesom2/test/meshes/${EXPERIMENT}"
WORK_DIR=$(mktemp -d)

echo "=== FESOM2 toy experiment ==="
echo "    experiment : ${EXPERIMENT}"
echo "    nranks     : ${NRANKS}"
echo "    ndays      : ${NDAYS}"
echo "    mesh       : ${MESH_DIR}"
echo "    workdir    : ${WORK_DIR}"
echo "    output     : /output"
echo ""

mkdir -p /output
cd "${WORK_DIR}"

# --- Copy config files --------------------------------------------------------
# Toy-specific variants (namelist.X.toy_<experiment> → namelist.X)
for f in /fesom2/config/namelist.*.toy_${EXPERIMENT}; do
    base=$(basename "${f}" | sed "s/\.toy_${EXPERIMENT}//")
    cp "${f}" "${base}"
done

# soufflet has no namelist.config.toy_soufflet — fall back to neverworld2's
# config and patch which_toy below
if [ ! -f namelist.config ]; then
    cp /fesom2/config/namelist.config.toy_neverworld2 namelist.config
fi

# Shared configs that have no toy-specific variant
# namelist.forcing is always required even when toy_ocean=.true. (FESOM2 opens
# it unconditionally at startup; its contents are not used for toy runs)
for nml in namelist.cvmix namelist.ice namelist.io namelist.dyn namelist.forcing; do
    [ -f "${nml}" ] || cp "/fesom2/config/${nml}" .
done

# --- Patch namelist.config at runtime and write fesom.clock ------------------
python3 - <<PYEOF
import re

with open('namelist.config') as fh:
    text = fh.read()

text = re.sub(r"(MeshPath\s*=\s*)('[^']*'|\"[^\"]*\")", r"\1'${MESH_DIR}/'", text)
text = re.sub(r"(ResultPath\s*=\s*)('[^']*'|\"[^\"]*\")", r"\1'/output/'", text)
text = re.sub(r"(ClimateDataPath\s*=\s*)('[^']*'|\"[^\"]*\")", r"\1'/dev/null'", text)
text = re.sub(r"(run_length\s*=\s*)\d+", r"\g<1>${NDAYS}", text)
text = re.sub(r"(run_length_unit\s*=\s*)'\w+'", r"\1'd'", text)
text = re.sub(r"(n_part\s*=\s*)\d+", r"\g<1>${NRANKS}", text)
text = re.sub(r"(which_toy\s*=\s*)'[^']*'", r"\1'${EXPERIMENT}'", text)

with open('namelist.config', 'w') as fh:
    fh.write(text)

# Write fesom.clock for a cold start.
# Format: two identical lines of "time day year".
# Duplicate lines signal a cold start (not a restart) to clock_init().
m_t = re.search(r'timenew\s*=\s*([\d.]+)', text)
m_d = re.search(r'daynew\s*=\s*(\d+)', text)
m_y = re.search(r'yearnew\s*=\s*(\d+)', text)
t = m_t.group(1) if m_t else '0.0'
d = m_d.group(1) if m_d else '1'
y = m_y.group(1) if m_y else '1900'
clock_line = f'{t} {d} {y}\n'
with open('/output/fesom.clock', 'w') as fh:
    fh.write(clock_line)
    fh.write(clock_line)
PYEOF

echo "=== Patched namelist.config (key values) ==="
grep -E "MeshPath|ResultPath|run_length|n_part|which_toy" namelist.config | grep -v "^!"
echo ""

# --- Run FESOM2 ---------------------------------------------------------------
echo "=== Running fesom.x ==="
mpirun --allow-run-as-root -np "${NRANKS}" /fesom2/bin/fesom.x

echo ""
echo "=== Done. Output files in /output ==="
ls /output/

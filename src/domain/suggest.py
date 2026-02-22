"""Skeleton experiment configurations for known MITgcm setups.

Returns a structured dict with CPP options, namelist stanzas, and notes for
two canonical experiment types:
- rotating_convection  : non-hydrostatic f-plane convection driven by surface buoyancy flux
- baroclinic_instability : Eady-like setup with stratified initial conditions
"""

# Shared quickstart recipe — identical for all experiment types.
# Build and run commands match scripts/build-experiment.sh and
# scripts/run-experiment.sh exactly.
_QUICKSTART: dict = {
    "directory_structure": {
        "code/CPP_OPTIONS.h": "#define each flag from cpp_options in this response",
        "code/SIZE.h": (
            "Set sNx, sNy, Nr, nPx=1, nPy=1, nSx=1, nSy=1, OLx=2, OLy=2. "
            "Constraint: sNx*nSx*nPx == Nx, sNy*nSy*nPy == Ny. "
            "Use search_docs_tool('SIZE.h') to find a working example."
        ),
        "code/packages.conf": "One package name per line (e.g. diagnostics)",
        "code/DIAGNOSTICS_SIZE.h": (
            "Required when ALLOW_DIAGNOSTICS is in cpp_options. "
            "Use search_docs_tool('DIAGNOSTICS_SIZE.h') for an example."
        ),
        "input/data": "PARM01, PARM03, PARM04 from translate_lab_params_tool",
        "input/data.eos": "EOS_PARM01 from namelists['data.eos'] in this response",
        "input/data.pkg": "useXXX=.TRUE. for each active package",
        "input/data.diagnostics": (
            "Required when diagnostics package is active. "
            "Use search_docs_tool('data.diagnostics') for an example."
        ),
        "input/<fields>": (
            "Input fields (binary .bin or NetCDF) — write a Python script "
            "to generate from your physical parameters."
        ),
    },
    "build": (
        "docker run --rm \\\n"
        "  --user \"$(id -u):$(id -g)\" \\\n"
        "  -v $(pwd)/my_experiment:/exp \\\n"
        "  ghcr.io/willirath/mitgcm:latest \\\n"
        "  bash -c \"\n"
        "    MULTIARCH=\\$(dpkg-architecture -qDEB_HOST_MULTIARCH)\n"
        "    case \\$(dpkg-architecture -qDEB_HOST_ARCH) in\n"
        "      amd64) OPTFILE=linux_amd64_gfortran ;;\n"
        "      arm64) OPTFILE=linux_arm64_gfortran ;;\n"
        "      *)     echo 'Unsupported arch'; exit 1 ;;\n"
        "    esac\n"
        "    mkdir -p /exp/build && cd /exp/build &&\n"
        "    MPI=true MPI_HOME=/usr/lib/\\$MULTIARCH/mpich\n"
        "      /MITgcm/tools/genmake2 \\\n"
        "        -rootdir /MITgcm \\\n"
        "        -mods /exp/code \\\n"
        "        -optfile /MITgcm/tools/build_options/\\$OPTFILE \\\n"
        "        -mpi &&\n"
        "    make depend && make -j\\$(nproc)\""
    ),
    "run": (
        "docker run --rm \\\n"
        "  --user \"$(id -u):$(id -g)\" \\\n"
        "  -v $(pwd)/my_experiment:/exp \\\n"
        "  ghcr.io/willirath/mitgcm:latest \\\n"
        "  bash -c \"\n"
        "    mkdir -p /exp/run && cd /exp/run &&\n"
        "    ln -sf /exp/build/mitgcmuv . &&\n"
        "    ln -sf /exp/input/* . &&\n"
        "    mpirun --allow-run-as-root -np N ./mitgcmuv\""
    ),
    "notes": [
        "Replace N in mpirun with nPx*nPy from SIZE.h.",
        "The ghcr.io/willirath/mitgcm image includes MITgcm source — no git clone needed.",
        "Input fields (.bin or NetCDF) must be generated separately — write a Python script; these tools do not produce them.",
        "Output is written to my_experiment/run/; remove or rename between runs.",
    ],
}


_CONFIGS: dict = {
    "rotating_convection": {
        "experiment_type": "rotating_convection",
        "description": (
            "Non-hydrostatic rotating convection on an f-plane. Surface buoyancy flux "
            "drives convection; rotation organises it into columnar structures."
        ),
        "cpp_options": ["ALLOW_NONHYDROSTATIC", "ALLOW_DIAGNOSTICS"],
        "namelists": {
            "data": {
                "PARM01": {
                    "nonHydrostatic": ".TRUE.",
                    "useNHMTerms": ".TRUE.",
                    "f0": "<from translate_lab_params>",
                    "beta": 0.0,
                    "viscAh": "<nu>",
                    "viscAz": "<nu>",
                    "diffKhT": "<kappa>",
                    "diffKzT": "<kappa>",
                    "no_slip_sides": ".TRUE.",
                    "no_slip_bottom": ".TRUE.",
                    "eosType": "'LINEAR'",
                    "rigidLid": ".TRUE.",
                    "implicitFreeSurface": ".FALSE.",
                },
                "PARM03": {
                    "nTimeSteps": 10000,
                    "deltaT": "<choose to satisfy CFL>",
                    "abEps": 0.1,
                },
            },
            "data.eos": {
                "EOS_PARM01": {
                    "eosType": "'LINEAR'",
                    "tAlpha": "<alpha>",
                    "sBeta": 0.0,
                },
            },
        },
        "notes": [
            "Set f0, viscAh/Az, diffKhT/ZT, tAlpha from translate_lab_params output.",
            "Surface heat flux applied via data.pkg/OBCS or external forcing file.",
            "Check Ek < 0.01 and aspect ratio > 0.1 via check_scales before running.",
            "Spin up to solid-body rotation before enabling heat flux.",
        ],
        "quickstart": _QUICKSTART,
    },
    "baroclinic_instability": {
        "experiment_type": "baroclinic_instability",
        "description": (
            "Eady-like baroclinic instability. Two-layer or linearly stratified initial "
            "conditions on an f-plane; sidewalls close the domain."
        ),
        "cpp_options": ["ALLOW_DIAGNOSTICS"],
        "namelists": {
            "data": {
                "PARM01": {
                    "f0": "<from translate_lab_params>",
                    "beta": 0.0,
                    "viscAh": "<nu>",
                    "viscAz": "<nu>",
                    "diffKhT": "<kappa>",
                    "diffKzT": "<kappa>",
                    "no_slip_sides": ".FALSE.",
                    "no_slip_bottom": ".TRUE.",
                    "eosType": "'LINEAR'",
                    "rigidLid": ".TRUE.",
                    "implicitFreeSurface": ".FALSE.",
                },
                "PARM03": {
                    "nTimeSteps": 50000,
                    "deltaT": "<choose to satisfy CFL>",
                    "abEps": 0.1,
                },
            },
            "data.eos": {
                "EOS_PARM01": {
                    "eosType": "'LINEAR'",
                    "tAlpha": "<alpha>",
                    "sBeta": 0.0,
                },
            },
        },
        "notes": [
            "Set initial stratification to match laboratory density contrast.",
            "Burger number Bu ~ 1 for active baroclinic instability (check with check_scales).",
            "Free-slip sides (no_slip_sides=.FALSE.) often used in Eady models; adjust to match experiment.",
            "Hydrostatic approximation typically valid for Eady setups (aspect ratio < 0.1); confirm with check_scales.",
        ],
        "quickstart": _QUICKSTART,
    },
}

_ALIASES: dict = {
    "convection": "rotating_convection",
    "rotating convection": "rotating_convection",
    "eady": "baroclinic_instability",
    "baroclinic": "baroclinic_instability",
}


def suggest_experiment_config(experiment_type: str) -> dict | None:
    """Return a skeleton configuration dict for a known experiment type.

    Parameters
    ----------
    experiment_type : str
        Experiment name.  Recognised values: "rotating_convection",
        "baroclinic_instability".  Common aliases are also accepted:
        "convection", "rotating convection", "eady", "baroclinic".
        Lookup is case-insensitive.

    Returns
    -------
    dict or None
        Configuration dict with keys:
        - "experiment_type" : str
        - "description"     : str
        - "cpp_options"     : list of str (CPP flags to #define in CPP_OPTIONS.h)
        - "namelists"       : dict of namelist-file -> dict of group -> dict of param -> value
        - "notes"           : list of str (setup and verification reminders)
        - "quickstart"      : dict with keys "directory_structure", "build", "run", "notes"
        Returns None if the experiment type is not recognised.
    """
    key = experiment_type.lower().strip()
    key = _ALIASES.get(key, key)
    return _CONFIGS.get(key)

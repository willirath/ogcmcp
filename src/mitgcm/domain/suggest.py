"""Skeleton experiment configurations for known MITgcm setups.

Returns a structured dict with CPP options, namelist stanzas, and notes for
two canonical experiment types:
- rotating_convection  : non-hydrostatic f-plane convection driven by surface buoyancy flux
- baroclinic_instability : Eady-like setup with stratified initial conditions
"""

_RUNTIME_IMAGE = "ghcr.io/willirath/ogcmcp:runtime-latest"

# Two explicit Dockerfiles — one per target platform.
# Do not use a runtime uname/dpkg switch: cross-build contexts require the
# platform to be fixed at image build time.
#
# ARM64 note: newer gfortran requires -fallow-argument-mismatch for MITgcm's
# Fortran 77 MPI bindings (mpif77/mpif.h argument type mismatches). Inject via
# code/genmake_local:
#   EXTRA_FFLAGS="$EXTRA_FFLAGS -fallow-argument-mismatch"
# Include that file in code/ before running docker build.
_DOCKERFILE_AMD64 = (
    f"FROM {_RUNTIME_IMAGE}\n"
    "# Platform: linux/amd64  (x86-64 hosts)\n"
    "# Build with: docker build --platform linux/amd64 -t my_experiment -f Dockerfile.amd64 .\n"
    "COPY code/ /experiment/code/\n"
    "COPY input/ /experiment/input/\n"
    "RUN mkdir -p /experiment/build && cd /experiment/build && \\\n"
    "    MPI=true MPI_HOME=/usr/lib/x86_64-linux-gnu/mpich \\\n"
    "      /MITgcm/tools/genmake2 \\\n"
    "        -rootdir /MITgcm -mods /experiment/code \\\n"
    "        -optfile /MITgcm/tools/build_options/linux_amd64_gfortran -mpi && \\\n"
    "    make depend && make -j$(nproc)\n"
    "ENV NP=1\n"
    "WORKDIR /experiment/run\n"
    "CMD ln -sf /experiment/build/mitgcmuv . && \\\n"
    "    { [ -n \"$(ls -A /experiment/input/)\" ] && ln -sf /experiment/input/* .; } && \\\n"
    "    mpirun -np $NP ./mitgcmuv\n"
)

_DOCKERFILE_ARM64 = (
    f"FROM {_RUNTIME_IMAGE}\n"
    "# Platform: linux/arm64  (Apple Silicon M-series, ARM64 cloud instances)\n"
    "# Build with: docker build --platform linux/arm64 -t my_experiment -f Dockerfile.arm64 .\n"
    "#\n"
    "# If compilation fails with 'Type mismatch in argument' errors, add\n"
    "# code/genmake_local containing:\n"
    "#   EXTRA_FFLAGS=\"$EXTRA_FFLAGS -fallow-argument-mismatch\"\n"
    "COPY code/ /experiment/code/\n"
    "COPY input/ /experiment/input/\n"
    "RUN mkdir -p /experiment/build && cd /experiment/build && \\\n"
    "    MPI=true MPI_HOME=/usr/lib/aarch64-linux-gnu/mpich \\\n"
    "      /MITgcm/tools/genmake2 \\\n"
    "        -rootdir /MITgcm -mods /experiment/code \\\n"
    "        -optfile /MITgcm/tools/build_options/linux_arm64_gfortran -mpi && \\\n"
    "    make depend && make -j$(nproc)\n"
    "ENV NP=1\n"
    "WORKDIR /experiment/run\n"
    "CMD ln -sf /experiment/build/mitgcmuv . && \\\n"
    "    { [ -n \"$(ls -A /experiment/input/)\" ] && ln -sf /experiment/input/* .; } && \\\n"
    "    mpirun -np $NP ./mitgcmuv\n"
)

# Shared quickstart recipe — identical for all experiment types.
# Strategy: one self-contained directory with a Dockerfile that bakes in
# the compiled binary and input files. Build once, run with a single volume
# mount for output. No cascading docker run calls.
_QUICKSTART: dict = {
    "directory_structure": {
        "Dockerfile.amd64": (
            "Use on x86-64 hosts. Copy from quickstart['dockerfile_amd64']. "
            "Set ENV NP to nPx*nPy from SIZE.h."
        ),
        "Dockerfile.arm64": (
            "Use on Apple Silicon / ARM64 hosts. Copy from quickstart['dockerfile_arm64']. "
            "Build with: docker build --platform linux/arm64 -t my_experiment -f Dockerfile.arm64 ."
        ),
        "code/CPP_OPTIONS.h": "#define each flag from cpp_options in this response",
        "code/SIZE.h": (
            "Set sNx, sNy, Nr, nPx, nPy, nSx=1, nSy=1, OLx=2, OLy=2. "
            "Constraint: sNx*nSx*nPx == Nx, sNy*nSy*nPy == Ny. "
            "Use search_docs_tool('SIZE.h') to find a working example."
        ),
        "code/packages.conf": "One package name per line (e.g. diagnostics)",
        "code/genmake_local": (
            "ARM64 only, if compilation fails with 'Type mismatch in argument' errors: "
            "add EXTRA_FFLAGS=\"$EXTRA_FFLAGS -fallow-argument-mismatch\" to inject the "
            "flag that newer gfortran requires for MITgcm's Fortran 77 MPI bindings."
        ),
        "code/DIAGNOSTICS_SIZE.h": (
            "Required when ALLOW_DIAGNOSTICS is in cpp_options. "
            "Use search_docs_tool('DIAGNOSTICS_SIZE.h') for an example."
        ),
        "input/data": "PARM01, PARM03, PARM04 from translate_lab_params_tool",
        "input/data.eos": "EOS_PARM01 from namelists['data.eos'] in this response",
        "input/data.exf": (
            "Required when ALLOW_EXF is in cpp_options. "
            "Set hflxconst for a uniform heat flux or hflxFile for a binary field."
        ),
        "input/data.pkg": "useXXX=.TRUE. for each active package",
        "input/data.diagnostics": (
            "Required when diagnostics package is active. "
            "Use search_docs_tool('data.diagnostics') for an example."
        ),
        "input/<fields>": "Input fields (.bin or NetCDF) — written by gen_input.py.",
        "gen_input.py": (
            "Python script that writes binary or NetCDF input fields to input/. "
            "Requires numpy (pip install numpy) at minimum; scipy/xarray if needed. "
            "Run before docker build so fields are COPY'd into the image."
        ),
    },
    "dockerfile_amd64": _DOCKERFILE_AMD64,
    "dockerfile_arm64": _DOCKERFILE_ARM64,
    "dockerfile_note": (
        "Use Dockerfile.amd64 on x86-64 hosts (most cloud VMs, Linux workstations). "
        "Use Dockerfile.arm64 on Apple Silicon or other ARM64 hosts."
    ),
    "build": "docker build --platform linux/amd64 -t my_experiment -f Dockerfile.amd64 .",
    "build_arm64": "docker build --platform linux/arm64 -t my_experiment -f Dockerfile.arm64 .",
    "run": (
        "mkdir -p output && "
        "docker run --rm -e NP=1 -v \"$(pwd)/output:/experiment/run\" my_experiment"
    ),
    "run_with_input_mount": (
        "mkdir -p output && "
        "docker run --rm -e NP=1 "
        "-v \"$(pwd)/input:/experiment/input:ro\" "
        "-v \"$(pwd)/output:/experiment/run\" "
        "my_experiment"
    ),
    "notes": [
        f"Runtime image {_RUNTIME_IMAGE} includes MITgcm source at /MITgcm.",
        "Prerequisites before docker build: pip install numpy; python gen_input.py",
        (
            "NP=1 is the default. NP>1 requires editing nPx/nPy in code/SIZE.h and "
            "rebuilding the image — passing -e NP=... at runtime alone does NOT work."
        ),
        "Output lands in output/ on the host after docker run.",
        (
            "For namelist-only changes (no code changes), use run_with_input_mount "
            "to avoid a full rebuild. Only rebuild when code/ or gen_input.py changes."
        ),
    ],
}


_CONFIGS: dict = {
    "rotating_convection": {
        "experiment_type": "rotating_convection",
        "description": (
            "Non-hydrostatic rotating convection on an f-plane. Surface buoyancy flux "
            "drives convection; rotation organises it into columnar structures."
        ),
        "cpp_options": ["ALLOW_NONHYDROSTATIC", "ALLOW_DIAGNOSTICS", "ALLOW_EXF"],
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
            "data.exf": {
                "EXF_NML_01": {
                    "hflxconst": "<surface_heat_flux_W_m2>",
                },
            },
        },
        "notes": [
            "Set f0, viscAh/Az, diffKhT/ZT, tAlpha from translate_lab_params output.",
            (
                "Surface heat flux is applied via the EXF package (ALLOW_EXF in "
                "cpp_options, useEXF=.TRUE. in data.pkg). Set hflxconst in "
                "data.exf &EXF_NML_01 for a spatially uniform constant flux, "
                "or set hflxFile for a binary field written by gen_input.py."
            ),
            "Add 'exf' to code/packages.conf alongside 'diagnostics'.",
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
        - "quickstart"      : dict with keys "directory_structure", "dockerfile",
                              "build", "run", "notes"
        Returns None if the experiment type is not recognised.
    """
    key = experiment_type.lower().strip()
    key = _ALIASES.get(key, key)
    return _CONFIGS.get(key)

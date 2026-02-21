"""Skeleton experiment configurations for known rotating-tank MITgcm setups.

Returns a structured dict with CPP options, namelist stanzas, and notes for
two canonical experiment types:
- rotating_convection  : non-hydrostatic f-plane convection driven by surface buoyancy flux
- baroclinic_instability : Eady-like setup with stratified initial conditions
"""


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
        Returns None if the experiment type is not recognised.
    """
    key = experiment_type.lower().strip()
    key = _ALIASES.get(key, key)
    return _CONFIGS.get(key)

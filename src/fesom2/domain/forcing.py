"""FESOM2 forcing dataset catalogue.

Loads FESOM2/setups/forcings.yml and exposes it as a queryable catalogue.
Each entry contains bulk formula coefficients, file prefix patterns,
NetCDF variable names, and calendar parameters for a forcing dataset.
"""

from pathlib import Path

import yaml

_FORCINGS_PATH = Path("FESOM2/setups/forcings.yml")

# age_tracer namelist group defaults (gen_modules_forcing.F90:49-52).
# setup_model reads this group unconditionally from namelist.forcing, but it is
# absent from every entry in forcings.yml.  We inject the Fortran defaults so
# callers always receive a complete, valid set of groups.
_AGE_TRACER_DEFAULTS: dict = {
    "age_tracer": {
        "use_age_tracer": False,
        "use_age_mask": False,
        "age_tracer_path": "./mesh/",
        "age_start_year": 2000,
    }
}


def list_forcing_datasets(forcings_path: Path = _FORCINGS_PATH) -> list[str]:
    """Return names of all available forcing datasets."""
    if not forcings_path.exists():
        return []
    with forcings_path.open(encoding="utf-8") as fh:
        data = yaml.safe_load(fh) or {}
    return sorted(data.keys())


def get_forcing_spec(
    dataset: str, forcings_path: Path = _FORCINGS_PATH
) -> dict | None:
    """Return the full specification for a forcing dataset.

    Parameters
    ----------
    dataset:
        Dataset name, case-insensitive (e.g. "CORE2", "JRA55", "ERA5").

    Returns
    -------
    dict or None
        Keys: forcing_exchange_coeff, forcing_bulk, land_ice, age_tracer,
        nam_sbc, namelist.config (if present).
        age_tracer is always included with Fortran defaults
        (use_age_tracer=False, use_age_mask=False, age_tracer_path='./mesh/',
        age_start_year=2000) even when absent from forcings.yml, because
        setup_model reads that group unconditionally from namelist.forcing.
        Returns None if the dataset is not found.
    """
    if not forcings_path.exists():
        return None
    with forcings_path.open(encoding="utf-8") as fh:
        data = yaml.safe_load(fh) or {}
    # Case-insensitive lookup
    lookup = {k.lower(): v for k, v in data.items()}
    spec = lookup.get(dataset.lower())
    if spec is None:
        return None
    # Inject age_tracer defaults when absent from forcings.yml
    return {**_AGE_TRACER_DEFAULTS, **spec}

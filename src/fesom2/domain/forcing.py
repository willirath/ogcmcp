"""FESOM2 forcing dataset catalogue.

Loads FESOM2/setups/forcings.yml and exposes it as a queryable catalogue.
Each entry contains bulk formula coefficients, file prefix patterns,
NetCDF variable names, and calendar parameters for a forcing dataset.
"""

from pathlib import Path

import yaml

_FORCINGS_PATH = Path("FESOM2/setups/forcings.yml")


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
        Keys: forcing_exchange_coeff, forcing_bulk, land_ice, nam_sbc,
        namelist.config (if present). Returns None if not found.
    """
    if not forcings_path.exists():
        return None
    with forcings_path.open(encoding="utf-8") as fh:
        data = yaml.safe_load(fh) or {}
    # Case-insensitive lookup
    lookup = {k.lower(): v for k, v in data.items()}
    return lookup.get(dataset.lower())

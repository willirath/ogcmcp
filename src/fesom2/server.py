"""MCP server exposing FESOM2 code-navigation tools via stdio."""

from mcp.server.fastmcp import FastMCP

from src.fesom2.tools import (
    find_modules,
    find_subroutines,
    get_callers,
    get_callees,
    get_doc_source,
    get_forcing_spec,
    get_module,
    get_module_uses,
    get_subroutine,
    list_forcing_datasets,
    list_setups,
    namelist_to_code,
    search_code,
    search_docs,
)
from src.fesom2.domain import (
    get_run_interface,
    lookup_gotcha,
    suggest_experiment_config,
    get_workflow,
    get_namelist_structure,
)
from src.shared import translate_lab_params, check_scales

mcp = FastMCP("fesom2")


# ── Code navigation ───────────────────────────────────────────────────────────


@mcp.tool()
def search_code_tool(query: str, top_k: int = 5) -> list[dict]:
    """Semantic search over FESOM2 subroutines.

    Returns up to ``top_k`` subroutines whose source most closely matches the
    natural-language query. Requires Ollama and a populated
    ``fesom2_subroutines`` ChromaDB collection.

    Each result has: id, name, module_name, file, start_line, end_line.
    Follow up with ``get_source_tool`` to read the subroutine source.
    """
    return search_code(query, top_k=top_k)


@mcp.tool()
def find_modules_tool(name: str) -> list[dict]:
    """Find FESOM2 F90 modules by name (case-insensitive).

    Returns an empty list if not found.
    Each result has: id, name, file, start_line, end_line.
    Follow up with ``get_module_tool`` for the full module record including
    its contained subroutines.
    """
    return find_modules(name)


@mcp.tool()
def find_subroutines_tool(name: str) -> list[dict]:
    """Find FESOM2 subroutines by name (case-insensitive).

    A name may appear in more than one module (e.g. init routines). Returns
    all matches. Each result has: id, name, module_name, file, start_line,
    end_line. Pass module= to ``get_source_tool`` to disambiguate.
    """
    return find_subroutines(name)


@mcp.tool()
def get_module_tool(name: str) -> dict | None:
    """Return metadata for a FESOM2 module including its subroutines.

    Returns the module file, line range, and list of contained subroutines
    (name, start_line, end_line). Returns None if not found.
    Use ``get_source_tool`` with one of the subroutine names to read source.
    """
    return get_module(name)


@mcp.tool()
def get_subroutine_tool(name: str, module: str | None = None) -> dict | None:
    """Return metadata for a subroutine by name (no source text).

    Returns id, name, module_name, file, start_line, end_line.
    Name lookup is case-insensitive. Returns None if not found.
    Pass ``module=`` to disambiguate when a name appears in multiple modules.
    Use ``find_subroutines_tool`` to discover which modules contain the name,
    then ``get_source_tool`` to retrieve the actual source lines.

    When a name is ambiguous and ``module=`` is omitted, returns a
    ``{"disambiguation_needed": True, "matches": [...]}`` dict rather than
    erroring — safe to call in parallel with other tools.
    """
    try:
        result = get_subroutine(name, module=module)
    except ValueError as exc:
        return {
            "disambiguation_needed": True,
            "message": str(exc),
            "matches": find_subroutines(name),
        }
    if result is None:
        return None
    result.pop("source_text", None)
    return result


@mcp.tool()
def get_source_tool(
    name: str, module: str | None = None, offset: int = 0, limit: int = 100
) -> dict | None:
    """Return paginated source lines for a FESOM2 subroutine.

    offset: first line to return (0-based within the subroutine source).
    limit: maximum lines to return (default 100).

    Returns {name, module_name, total_lines, offset, lines}. Returns None if
    not found. Pass ``module=`` when a name is shared across modules to select
    the correct copy.

    When a name is ambiguous and ``module=`` is omitted, returns a
    ``{"disambiguation_needed": True, "matches": [...]}`` dict rather than
    erroring — safe to call in parallel with other tools.
    """
    try:
        result = get_subroutine(name, module=module)
    except ValueError as exc:
        return {
            "disambiguation_needed": True,
            "message": str(exc),
            "matches": find_subroutines(name),
        }
    if result is None:
        return None
    all_lines = result["source_text"].splitlines()
    total = len(all_lines)
    return {
        "name": result["name"],
        "module_name": result["module_name"],
        "total_lines": total,
        "offset": offset,
        "lines": all_lines[offset: offset + limit],
    }


@mcp.tool()
def get_callers_tool(name: str) -> list[dict]:
    """Return all subroutines that call the named subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    Each result has: caller_name, caller_module.
    """
    return get_callers(name)


@mcp.tool()
def get_callees_tool(name: str) -> list[dict]:
    """Return all subroutines called by the named subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    """
    return get_callees(name)


@mcp.tool()
def get_module_uses_tool(module_name: str) -> list[str]:
    """Return the modules USEd by a FESOM2 module.

    Reflects the USE statements at the top of the module. Name lookup is
    case-insensitive. Returns a list of module name strings.
    Use this to trace data flow between modules (e.g. which modules provide
    the mesh arrays or MPI topology used by a dynamics module).
    """
    return get_module_uses(module_name)


@mcp.tool()
def namelist_to_code_tool(param: str) -> list[dict]:
    """Return the FESOM2 module(s) that declare a namelist parameter.

    Looks up ``namelist_refs`` (declaration sites) and ``namelist_descriptions``
    (inline comment from the config file) in the DuckDB index. Returns an
    empty list if not found.

    Each result has: param_name, namelist_group, file, module_name, line,
    description (from config file inline comment), config_file.

    If not found, the parameter may be an internal variable rather than a
    namelist input. Try ``search_code_tool(param)`` to find where it is
    declared or used in the source.
    """
    results = namelist_to_code(param)
    if not results:
        return [
            {
                "warning": (
                    f"'{param}' was not found as a namelist parameter. "
                    f"It may be an internal model variable rather than a namelist "
                    f"input. Try search_code_tool('{param}') to find where it is "
                    f"used in the source, or check get_namelist_structure_tool() "
                    f"to find the correct parameter name."
                )
            }
        ]
    return results


# ── Documentation search ──────────────────────────────────────────────────────


@mcp.tool()
def search_docs_tool(query: str, top_k: int = 5) -> list[dict]:
    """Semantic search over FESOM2 RST documentation and namelist descriptions.

    Searches both the ``fesom2_docs`` collection (RST pages) and the
    ``fesom2_namelists`` collection (inline parameter descriptions from config
    files). Returns the ``top_k`` best matches across both.

    Each result has: source ('doc' or 'namelist'), snippet (first 400 chars),
    and source-specific metadata:
    - doc:      file (RST path), section (heading)
    - namelist: param_name, namelist_group, config_file

    Use natural-language queries (e.g. 'GM eddy parameterisation',
    'ALE vertical coordinate', 'EVP sea ice rheology'). For a specific
    parameter name, use ``namelist_to_code_tool`` instead.
    """
    return search_docs(query, top_k=top_k)


@mcp.tool()
def get_doc_source_tool(
    file: str, section: str, offset: int = 0, limit: int = 200
) -> dict | None:
    """Return paginated text of a FESOM2 documentation section.

    Use ``search_docs_tool`` to discover file and section values, then call
    this to read the full content.

    file    : RST file path as returned by search_docs_tool
    section : section heading as returned by search_docs_tool
    offset  : first line to return (0-based)
    limit   : maximum lines to return (default 200)

    Returns {file, section, total_lines, offset, lines} or None if not found.
    """
    return get_doc_source(file, section, offset=offset, limit=limit)


@mcp.tool()
def list_forcing_datasets_tool() -> list[str]:
    """Return names of all available FESOM2 forcing datasets.

    Reads ``FESOM2/setups/forcings.yml`` and returns the dataset identifiers
    (e.g. ``["CORE2", "ERA5", "JRA55", "test_global"]``).

    Returns an empty list if the catalogue file is not present.
    Follow up with ``get_forcing_spec_tool`` to get the full specification.
    """
    return list_forcing_datasets()


@mcp.tool()
def get_forcing_spec_tool(dataset: str) -> dict | None:
    """Return the full specification for a FESOM2 forcing dataset.

    Looks up the named dataset in ``FESOM2/setups/forcings.yml``.
    Lookup is case-insensitive (``"ERA5"`` and ``"era5"`` both work).

    Parameters
    ----------
    dataset : str
        Dataset name, e.g. ``"CORE2"``, ``"JRA55"``, ``"ERA5"``.

    Returns
    -------
    dict or None
        Keys: forcing_exchange_coeff, forcing_bulk, land_ice, age_tracer,
        nam_sbc (if present), and optionally namelist.config overrides.
        age_tracer is always included with safe Fortran defaults because
        setup_model reads it unconditionally from namelist.forcing regardless
        of forcing_opt or toy_ocean. Returns None if the dataset is not found.
    """
    return get_forcing_spec(dataset)


@mcp.tool()
def list_setups_tool(
    name: str | None = None,
    source: str | None = None,
    names_only: bool = False,
) -> list[dict]:
    """Return FESOM2 setup records, optionally filtered.

    Each record has:

    name     : identifier (e.g. ``"toy_neverworld2"``, ``"test_pi_cavity"``)
    source   : ``"reference_namelist"`` or ``"ci_setup"``
    mesh     : mesh name (str or None)
    forcing  : forcing dataset (str or None)
    namelists: namelist file → group → param → value
               For ``reference_namelist``: param value is
               ``{"value": str, "comment": str}`` (inline doc comment preserved)
               For ``ci_setup``: param value is the raw Python value
    fcheck   : variable → expected float (CI setups only; {} for reference)
    notes    : free-text description

    Parameters
    ----------
    name : str or None
        Substring filter on the record name (case-insensitive).
        E.g. ``name="neverworld2"`` returns ``toy_neverworld2`` and
        ``test_neverworld2``. Omit to return all names.
    source : str or None
        Exact filter on source type: ``"reference_namelist"`` or
        ``"ci_setup"``. Omit to return both.
    names_only : bool
        If True, return only ``{name, source, mesh, forcing, notes}`` per
        record — omit ``namelists`` and ``fcheck``. Use this to enumerate
        available setups without overflowing the token budget, then follow up
        with a targeted ``name=`` call to retrieve full namelist details.

    **When to use:**
    - ``names_only=True`` → enumerate all setups (≥ 16 records) cheaply.
    - ``source="reference_namelist"`` → complete annotated starting configs
      (toy_neverworld2, toy_dbgyre, toy_soufflet, forcing presets).
    - ``source="ci_setup"`` → sparse overrides for CI-tested physics variants
      (cavity, icepack, zstar, linfs, icebergs, floatice, visc7, partial).
    - ``name="cavity"`` → all cavity-related setups across both sources.
    """
    records = list_setups()
    if name is not None:
        needle = name.lower()
        records = [r for r in records if needle in r["name"].lower()]
    if source is not None:
        records = [r for r in records if r["source"] == source]
    if names_only:
        records = [
            {k: r[k] for k in ("name", "source", "mesh", "forcing", "notes")}
            for r in records
        ]
    return records


# ── Domain knowledge ──────────────────────────────────────────────────────────


@mcp.tool()
def translate_lab_params_tool(
    Lx: float,
    Ly: float,
    depth: float,
    Omega: float,
    delta_T: float | None = None,
    Nx: int | None = None,
    Ny: int | None = None,
    Nz: int | None = None,
    nu: float = 1e-6,
    kappa: float = 1.4e-7,
    alpha: float = 2e-4,
) -> dict:
    """Translate physical lab/ocean parameters to model namelist values.

    All length parameters in metres; Omega in rad/s; nu and kappa in m²/s;
    alpha in K⁻¹; delta_T in Kelvin.

    Parameters
    ----------
    Lx : float
        Domain length in x in metres.
    Ly : float
        Domain length in y in metres.
    depth : float
        Water depth in metres.
    Omega : float
        Rotation rate in rad/s (0 for non-rotating).
    delta_T : float or None
        Temperature contrast in Kelvin (optional).
    Nx : int or None
        Grid cells in x (omit to skip horizontal grid output).
    Ny : int or None
        Grid cells in y.
    Nz : int or None
        Vertical grid cells.
    nu : float
        Kinematic viscosity in m²/s (default 1e-6, water at 20 °C).
    kappa : float
        Thermal diffusivity in m²/s (default 1.4e-7, water at 20 °C).
    alpha : float
        Thermal expansion coefficient in K⁻¹ (default 2e-4, water at 20 °C).

    Returns
    -------
    dict
        Keys: PARM01, EOS_PARM01, PARM04 (if grid given), derived, notes.
        Use "derived" values to set step_per_day, visc_sh_limit, K_hor,
        A_ver, and tAlpha in FESOM2 namelists.
    """
    return translate_lab_params(
        Lx=Lx, Ly=Ly, depth=depth, Omega=Omega,
        delta_T=delta_T, Nx=Nx, Ny=Ny, Nz=Nz,
        nu=nu, kappa=kappa, alpha=alpha,
    )


@mcp.tool()
def check_scales_tool(
    Lx: float,
    Ly: float,
    depth: float,
    Omega: float,
    delta_T: float | None = None,
    dx: float | None = None,
    dy: float | None = None,
    dz: float | None = None,
    dt: float | None = None,
    U: float | None = None,
    nu: float = 1e-6,
    alpha: float = 2e-4,
) -> dict:
    """Compute dimensionless numbers and flag issues for a FESOM2 configuration.

    All length parameters in metres; Omega in rad/s; dt in seconds;
    U in m/s; nu in m²/s; alpha in K⁻¹; delta_T in Kelvin.

    Parameters
    ----------
    Lx : float
        Domain length in x in metres.
    Ly : float
        Domain length in y in metres.
    depth : float
        Water depth in metres.
    Omega : float
        Rotation rate in rad/s (0 for non-rotating).
    delta_T : float or None
        Temperature contrast in Kelvin (needed for N and Bu).
    dx : float or None
        Horizontal grid spacing in x in metres.
    dy : float or None
        Horizontal grid spacing in y in metres.
    dz : float or None
        Vertical grid spacing in metres.
    dt : float or None
        Time step in seconds (= 86400 / step_per_day for FESOM2).
    U : float or None
        Velocity scale in m/s (needed for Ro and CFL).
    nu : float
        Kinematic viscosity in m²/s (default 1e-6).
    alpha : float
        Thermal expansion coefficient in K⁻¹ (default 2e-4).

    Returns
    -------
    dict
        Keys: "numbers" (dict of dimensionless numbers and scales),
              "flags" (list of {"level": "warning"|"info", "message": str}).
    """
    return check_scales(
        Lx=Lx, Ly=Ly, depth=depth, Omega=Omega,
        delta_T=delta_T, dx=dx, dy=dy, dz=dz, dt=dt, U=U,
        nu=nu, alpha=alpha,
    )


@mcp.tool()
def lookup_gotcha_tool(topic: str) -> list[dict]:
    """Search the FESOM2 gotcha catalogue by keyword.

    Case-insensitive keyword search over a curated catalogue of known
    FESOM2 configuration traps.

    Parameters
    ----------
    topic : str
        Free-text search string. Examples: ``"ALE"``, ``"step_per_day"``,
        ``"EVP sea ice"``, ``"forcing interpolation"``, ``"METIS partition"``,
        ``"namelist.io output"``, ``"ice shelf cavity"``.

    Returns
    -------
    list[dict]
        Matching entries with keys: title, keywords, summary, detail.
        Empty list if no match.
    """
    return lookup_gotcha(topic)


@mcp.tool()
def get_run_interface_tool() -> dict:
    """Return the FESOM2 experiment directory layout and Docker run interface.

    Use this when setting up a new experiment or when an agent needs to know
    how to run FESOM2 in the self-contained Docker image. Returns the
    three-layer directory structure (mesh/, input/, output/), the Docker
    mount interface, what the entrypoint patches vs reads from the namelists,
    and the gitignore conventions.

    Returns
    -------
    dict
        Keys: ``description``, ``directory_structure``, ``docker_interface``
        (with ``command_template``, ``mounts``, ``entrypoint_contract``),
        ``gitignore_convention``, ``notes``.
    """
    return get_run_interface()


@mcp.tool()
def suggest_experiment_config_tool(experiment_type: str) -> dict | None:
    """Return a skeleton FESOM2 namelist configuration for a known experiment type.

    Returns a structured dict with namelist stanzas and setup notes for the
    experiment class. FESOM2 is CMake-based — no CPP options are included.

    Recognised types: ``"baroclinic_channel"``, ``"pi_control"``,
    ``"rotating_convection"``. Common aliases: ``"channel"``,
    ``"neverworld2"``, ``"control"``, ``"convection"``.
    Lookup is case-insensitive.

    Parameters
    ----------
    experiment_type : str
        Experiment class name or alias.

    Returns
    -------
    dict or None
        Keys: ``experiment_type``, ``description``,
        ``namelists`` (dict of file → group → param → value),
        ``notes`` (list of str).
        Returns None if the experiment type is not recognised.
    """
    return suggest_experiment_config(experiment_type)


@mcp.tool()
def get_namelist_structure_tool() -> dict[str, dict[str, str]]:
    """Return the FESOM2 namelist file → group → description map.

    Use this to orient yourself when you know the domain (e.g. 'vertical
    coordinate', 'sea ice dynamics', 'output frequency') but do not yet know
    which namelist file and group to edit.

    Returns
    -------
    dict[str, dict[str, str]]
        { namelist_file: { group_name: description } }
        Sorted alphabetically by file name.
        Example files: namelist.config, namelist.oce, namelist.tra,
        namelist.dyn, namelist.ice, namelist.forcing, namelist.io,
        namelist.cvmix, namelist.icepack, namelist.transit.
    """
    return get_namelist_structure()


# ── Workflow guidance ─────────────────────────────────────────────────────────


@mcp.tool()
def get_workflow_tool(task: str | None = None) -> dict:
    """Return recommended tool workflows for common FESOM2 tasks.

    Call this at the start of a session to get oriented, or with a specific
    task to get a step-by-step tool sequence.

    This server indexes FESOM2 source code and documentation, and provides
    domain knowledge for FESOM2 experiments. The tools help agents explore
    FESOM2 accurately — surfacing real source, real namelist values, and known
    configuration gotchas rather than generating answers from training data.

    Parameters
    ----------
    task : str or None
        One of ``"design_experiment"``, ``"debug_configuration"``,
        ``"understand_module"``, ``"explore_code"``.
        Omit (or pass None) to return all workflows.

    Returns
    -------
    dict
        Mapping of task name to {description, steps, notes}.
        Each step has {tool, purpose}. Empty dict if task not recognised.
    """
    return get_workflow(task)


if __name__ == "__main__":
    mcp.run()

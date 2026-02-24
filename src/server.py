"""MCP server exposing the M3 code-navigation tools via stdio."""

from mcp.server.fastmcp import FastMCP

from src.tools import (
    diagnostics_fill_to_source,
    find_subroutines,
    get_callees,
    get_callers,
    get_cpp_requirements,
    get_doc_source,
    get_package_flags,
    get_subroutine,
    get_verification_source,
    list_verification_experiments,
    namelist_to_code,
    search_code,
    search_docs,
    search_verification,
)
from src.domain import (
    translate_lab_params,
    check_scales,
    lookup_gotcha,
    suggest_experiment_config,
    get_workflow,
    get_namelist_structure,
)

mcp = FastMCP("mitgcm")


@mcp.tool()
def search_code_tool(query: str, top_k: int = 5) -> list[dict]:
    """Semantic search over MITgcm subroutines.

    Returns up to top_k subroutines whose source most closely matches the
    natural-language query. Requires Ollama and a populated ChromaDB index.
    """
    return search_code(query, top_k=top_k)


@mcp.tool()
def find_subroutines_tool(name: str) -> list[dict]:
    """Return all subroutines matching name, across all packages.

    Name lookup is case-insensitive. Returns an empty list if not found.
    Returns id, name, file, package, line_start, line_end for each match.
    Use this to discover which packages contain a subroutine when the name
    may appear in multiple packages (e.g. DIC_COEFFS_SURF in bling and dic).
    Follow up with get_source_tool(name, package=...) for source lines.
    """
    return find_subroutines(name)


@mcp.tool()
def get_subroutine_tool(name: str, package: str | None = None) -> dict | None:
    """Return metadata for a subroutine by name (no source text).

    Returns id, name, file, package, line_start, line_end.
    Name lookup is case-insensitive. Returns None if not found.
    Use get_source_tool to retrieve the actual source lines.
    When multiple subroutines share the same name across packages, pass
    package= to disambiguate; without it a ValueError is raised. Use
    find_subroutines_tool to discover which packages contain the name.
    """
    result = get_subroutine(name, package=package)
    if result is None:
        return None
    result.pop("source_text", None)
    return result


@mcp.tool()
def get_source_tool(name: str, package: str | None = None, offset: int = 0, limit: int = 100) -> dict | None:
    """Return paginated source lines for a subroutine.

    offset: first line to return (0-based within the subroutine source).
    limit: maximum number of lines to return (default 100).

    Returns {name, total_lines, offset, lines: [...]}. Lines are returned as
    a list of strings without trailing newlines. Returns None if not found.
    Use get_subroutine_tool first to see total line count (line_end - line_start).
    Pass package= when multiple subroutines share the same name to select the
    correct copy; without it a ValueError is raised if the name is ambiguous.
    """
    result = get_subroutine(name, package=package)
    if result is None:
        return None
    all_lines = result["source_text"].splitlines()
    total = len(all_lines)
    page = all_lines[offset : offset + limit]
    return {"name": result["name"], "total_lines": total, "offset": offset, "lines": page}


@mcp.tool()
def get_callers_tool(name: str, package: str | None = None) -> list[dict]:
    """Return all subroutines that call the named subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    Pass package= to restrict the result to callers within a specific package.
    """
    return get_callers(name, package=package)


@mcp.tool()
def get_callees_tool(name: str, package: str | None = None) -> list[dict]:
    """Return all subroutine names called by the named subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    Callees not present in the subroutines table are still returned by name.
    Pass package= to scope the lookup to a specific package copy of the
    subroutine when the name is shared across packages.
    """
    return get_callees(name, package=package)


@mcp.tool()
def namelist_to_code_tool(param: str) -> list[dict]:
    """Return subroutines that reference a namelist parameter.

    Name lookup is case-insensitive. Returns an empty list if not found.
    Each result includes the namelist group (e.g. PARM03).

    Note: results reflect *declaration* sites (where the parameter is read
    from the namelist), not *use* sites (where the value influences
    computation). For most parameters this means INI_PARMS is returned.
    To find where a parameter is actually used, follow up with get_callers
    or search_code with the parameter name as the query.

    If not found, returns a single-item list with a 'warning' key explaining
    why — the parameter may be an internal variable (COMMON block) rather than
    a namelist parameter. Check for a 'warning' key before treating results as
    subroutine records.
    """
    results = namelist_to_code(param)
    if not results:
        return [
            {
                "warning": (
                    f"'{param}' was not found as a namelist parameter. "
                    f"It may be an internal model variable (COMMON block) rather than "
                    f"a namelist input. Try search_code_tool('{param}') to find where "
                    f"it is declared or used in the source, or check "
                    f"get_namelist_structure_tool() to find the correct parameter name "
                    f"for the domain you are interested in."
                )
            }
        ]
    return results


@mcp.tool()
def diagnostics_fill_to_source_tool(field_name: str) -> list[dict]:
    """Return subroutines that fill a MITgcm diagnostics field.

    Comparison trims trailing spaces and folds case — extracted field names
    sometimes carry trailing whitespace. Returns an empty list if not found.
    """
    return diagnostics_fill_to_source(field_name)


@mcp.tool()
def get_cpp_requirements_tool(subroutine_name: str) -> list[str]:
    """Return CPP flags that guard a subroutine.

    Name lookup is case-insensitive. Returns an empty list if none found.
    """
    return get_cpp_requirements(subroutine_name)


@mcp.tool()
def get_package_flags_tool(package_name: str) -> list[dict]:
    """Return CPP flags defined by a MITgcm package.

    Name lookup is case-insensitive. Returns an empty list if not found.
    Each result has cpp_flag and description fields.

    Known limitation: packages that register flags through non-standard
    mechanisms (outside the PACKAGE_OPTIONS file pattern) return an empty
    list even when they do define CPP flags. Confirmed examples: showflops
    (uses TIME_PER_TIMESTEP_SFP, USE_PAPI_FLOPS_SFP, USE_PCL_FLOPS_SFP).
    For such packages, use search_code_tool or search_docs_tool to discover
    their flags.
    """
    return get_package_flags(package_name)


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
    """Translate physical lab parameters to MITgcm namelist values.

    All length parameters are in metres; Omega in rad/s; nu and kappa in m^2/s;
    alpha in K^-1; delta_T in Kelvin.

    Parameters
    ----------
    Lx : float
        Tank length in x in metres (use diameter for a cylindrical tank).
    Ly : float
        Tank length in y in metres (use diameter for a cylindrical tank).
    depth : float
        Water depth in metres.
    Omega : float
        Rotation rate in rad/s (0 for non-rotating).
    delta_T : float or None
        Temperature contrast in Kelvin (optional).
    Nx : int or None
        Grid cells in x.  Omit to skip delX in PARM04.
    Ny : int or None
        Grid cells in y.  Omit to skip delY in PARM04.
    Nz : int or None
        Vertical grid cells.  Omit to skip delZ in PARM04.
    nu : float
        Kinematic viscosity in m^2/s (default 1e-6, water at 20 degC).
    kappa : float
        Thermal diffusivity in m^2/s (default 1.4e-7, water at 20 degC).
    alpha : float
        Thermal expansion coefficient in K^-1 (default 2e-4, water at 20 degC).

    Returns
    -------
    dict
        Keys: PARM01, EOS_PARM01, PARM04 (if grid given), derived, notes.
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
    """Compute dimensionless numbers and flag issues for a MITgcm configuration.

    All length parameters are in metres; Omega in rad/s; dt in seconds;
    U in m/s; nu in m^2/s; alpha in K^-1; delta_T in Kelvin.

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
        Horizontal grid spacing in x in metres (for CFL and Ekman resolution).
    dy : float or None
        Horizontal grid spacing in y in metres (for CFL).
    dz : float or None
        Vertical grid spacing in metres (for Ekman-layer resolution and CFL_v).
    dt : float or None
        Time step in seconds (needed for CFL).
    U : float or None
        Velocity scale in m/s (needed for Ro and CFL).
    nu : float
        Kinematic viscosity in m^2/s (default 1e-6).
    alpha : float
        Thermal expansion coefficient in K^-1 (default 2e-4).

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
    """Search the MITgcm gotcha catalogue by keyword.

    Case-insensitive keyword search over a curated catalogue of known
    MITgcm configuration traps.  Returns all entries whose keyword list
    matches any phrase in the topic string.

    Parameters
    ----------
    topic : str
        Free-text search string.  Examples: "nonhydrostatic", "linear EOS",
        "spin-up", "sidewall", "diagnostics frequency", "rigid lid".

    Returns
    -------
    list[dict]
        Matching entries, each with keys: title, keywords, summary, detail.
        Empty list if no match.
    """
    return lookup_gotcha(topic)


@mcp.tool()
def suggest_experiment_config_tool(experiment_type: str) -> dict | None:
    """Return a skeleton MITgcm configuration for a known experiment type.

    Returns a structured dict with CPP flags, namelist stanzas, and setup notes.
    Recognised types: "rotating_convection", "baroclinic_instability".
    Common aliases: "convection", "rotating convection", "eady", "baroclinic".
    Lookup is case-insensitive.

    Parameters
    ----------
    experiment_type : str
        Experiment class name or alias.

    Returns
    -------
    dict or None
        Keys: "experiment_type", "description", "cpp_options" (list of str),
        "namelists" (dict of file -> group -> param -> value), "notes" (list of str),
        "quickstart" (dict with "directory_structure", "build", "run", "notes").
        Returns None if the experiment type is not recognised.
    """
    return suggest_experiment_config(experiment_type)


@mcp.tool()
def get_namelist_structure_tool() -> dict[str, dict[str, str]]:
    """Return the MITgcm namelist file → group → description map.

    Use this to orient yourself when you know the domain (e.g. 'open boundary
    conditions', 'EOS parameters') but do not yet know which namelist file and
    group to edit.

    The returned dict is keyed by namelist file name (e.g. 'data', 'data.eos',
    'data.obcs') with inner keys as namelist group names (e.g. 'PARM01',
    'OBCS_PARM01') and values as description strings.

    Guidance for avoiding rabbit holes:
    - To find which group reads a *specific parameter by name*, use
      namelist_to_code_tool instead — this tool gives the overview, not the
      per-parameter answer.
    - To understand what a group's parameters do in detail, use search_docs_tool
      with the group name (e.g. search_docs_tool('OBCS_PARM01')).
    - Adjoint/optimisation groups (CTRL_*, cost_nml, ecco_*, grdchk_*, optim,
      obsfit_nml, profiles_nml) are only relevant for adjoint/ECCO runs; skip
      them for forward experiments.
    - ATM2D groups (PARM01_ATM2D, ...) are only relevant for the 2-D atmosphere
      configuration; skip them for ocean / lab experiments.
    - Package groups with no explicit description (generic 'Package X parameters')
      belong to specialised packages — use search_docs_tool to learn more.

    Returns
    -------
    dict[str, dict[str, str]]
        { namelist_file: { group_name: description } }
        Sorted alphabetically by file name.
    """
    return get_namelist_structure()


@mcp.tool()
def get_workflow_tool(task: str | None = None) -> dict:
    """Return recommended tool workflows for common tasks.

    Call this at the start of a session to get oriented, or with a specific
    task to get a step-by-step tool sequence.

    This server indexes MITgcm source code and documentation, and provides
    domain knowledge for MITgcm experiments. The tools help agents
    explore MITgcm resources accurately — they surface real source, real
    namelist values, and known configuration traps rather than generating
    answers from training data.

    Parameters
    ----------
    task : str or None
        One of "design_experiment", "debug_configuration",
        "understand_package", "explore_code".
        Omit (or pass None) to return all workflows.

    Returns
    -------
    dict
        Mapping of task name to {description, steps, notes}.
        Each step has {tool, purpose}. Empty dict if task not recognised.
    """
    return get_workflow(task)


@mcp.tool()
def list_verification_experiments_tool() -> list[dict]:
    """Return structured catalogue of all MITgcm verification experiments.

    No arguments required.  Returns one entry per experiment with:
      name          : directory name (e.g. "tutorial_rotating_tank")
      tutorial      : True if name starts with "tutorial_"
      packages      : list of packages enabled in code/packages.conf
      domain_class  : "ocean" | "atmosphere" | "coupled" | "idealized"
      Nx, Ny, Nr    : total domain dimensions (or None if SIZE.h absent)
      grid_type     : "cartesian" | "spherical_polar" | "curvilinear"
      nonhydrostatic: bool
      free_surface  : bool (True = free surface, False = rigid lid)
      eos_type      : str (e.g. "LINEAR", "JMD95Z")

    Use this to find experiments relevant to your goal before calling
    search_verification_tool or get_doc_source_tool for their namelist content.
    """
    return list_verification_experiments()


@mcp.tool()
def search_verification_tool(query: str, top_k: int = 5) -> list[dict]:
    """Semantic search over MITgcm verification experiment configuration files.

    Searches input/data*, eedata, code/*.h, and packages.conf from all
    verification experiments.  Returns up to top_k results.

    Each result has: experiment, file, filename, snippet (first 400 chars).
    Use get_verification_source_tool with the returned file path to read the
    full content.

    Requires Ollama and pixi run embed-verification to have been run.
    """
    return search_verification(query, top_k=top_k)


@mcp.tool()
def get_verification_source_tool(
    file: str, offset: int = 0, limit: int = 200
) -> dict | None:
    """Return paginated full text of a verification experiment file.

    Retrieves the complete content of a single file from a MITgcm verification
    or tutorial experiment — namelists, SIZE.h, packages.conf, etc.

    file   : path as returned by search_verification_tool
             (e.g. "verification/rotating_convection/input/data")
    offset : first line to return (0-based); default 0
    limit  : max lines to return; default 200

    Returns {file, total_lines, offset, lines} or null if not found.
    Call repeatedly with increasing offset to page through large files.
    Use search_verification_tool first to discover file paths.
    """
    return get_verification_source(file, offset=offset, limit=limit)


@mcp.tool()
def get_doc_source_tool(file: str, section: str, offset: int = 0, limit: int = 200) -> dict | None:
    """Return paginated text of a documentation section or header file.

    Use search_docs_tool to discover file and section values, then call this
    to read the full content. Mirrors get_source_tool for subroutines.

    file    : as returned by search_docs_tool (e.g. "verification/rotating_tank/code/SIZE.h")
    section : as returned by search_docs_tool (e.g. "SIZE.h" or an RST heading)
    offset  : first line to return (0-based)
    limit   : maximum lines to return (default 200)

    Returns {file, section, total_lines, offset, lines} or None if not found.
    """
    return get_doc_source(file, section, offset=offset, limit=limit)


@mcp.tool()
def search_docs_tool(query: str, top_k: int = 5) -> list[dict]:
    """Semantic search over MITgcm documentation sections.

    Returns up to top_k doc sections whose prose most closely matches the
    natural-language query. Requires Ollama and a populated mitgcm_docs
    ChromaDB collection.

    Each result has: file (RST path relative to MITgcm/doc/), section
    (heading text), snippet (first 400 chars of cleaned section text).

    Use natural-language queries (e.g. 'surface wind forcing file',
    'open boundary conditions'). For a specific namelist parameter name
    (e.g. 'zonalWindFile', 'tauRelaxT'), use namelist_to_code_tool instead
    — semantic search on exact camelCase identifiers gives poor results.

    Results may include .h header files (e.g. model/inc/SIZE.h,
    eesupp/inc/EXCH.h, verification/*/code/SIZE.h). For these, the snippet
    skips leading Fortran C-comments and starts at the first declaration.
    Follow up with get_doc_source_tool(file, section) to read the full content.

    Known search gaps: the CD scheme (cd_code package, useCDscheme in PARM01)
    is not surfaced under common search terms like 'CD scheme Coriolis'. If
    you need CD scheme documentation, search for 'cd_code' or use
    search_code_tool('CD_CODE_SCHEME') to read the source directly.
    """
    return search_docs(query, top_k=top_k)


if __name__ == "__main__":
    mcp.run()

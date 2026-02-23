"""Namelist file → group → description map for MITgcm.

get_namelist_structure() returns a two-level dict
    { namelist_file: { group_name: description } }
covering all groups found in the DuckDB namelist_refs table, supplemented
by explicit entries for groups that may not appear in the index but are
important for common experiments (e.g. EOS_PARM01).

Explicit entries have detailed descriptions. Index-derived entries get a
one-line generic description computed from the group name.
"""

import re
from pathlib import Path

DB_PATH = Path("data/index.duckdb")

# ---------------------------------------------------------------------------
# Explicit entries: non-obvious file mappings and/or detailed descriptions.
# {file: {group: description}}
# ---------------------------------------------------------------------------

_EXPLICIT: dict[str, dict[str, str]] = {
    "data": {
        "PARM01": (
            "Core dynamics: viscosity (viscAh, viscAz), diffusivity (diffKhT, diffKzT), "
            "EOS type (eosType), nonhydrostatic flags (nonHydrostatic, useNHMTerms), "
            "boundary conditions (no_slip_sides, no_slip_bottom), free surface / rigid lid "
            "(implicitFreeSurface, rigidLid), rotation (f0, beta), gravity, "
            "reference pressure (EosRefP0)."
        ),
        "PARM02": (
            "Elliptic solvers: conjugate-gradient tolerances and iteration limits for the "
            "2-D barotropic pressure solve (cg2dMaxIter, cg2dTargetResidual) and the "
            "3-D non-hydrostatic pressure solve (cg3dMaxIter, cg3dTargetResidual)."
        ),
        "PARM03": (
            "Time-stepping control: nTimeSteps, deltaT, startTime / endTime, "
            "Adams-Bashforth coefficient (abEps), output frequencies "
            "(dumpFreq, monitorFreq, chkPtFreq, taveFreq)."
        ),
        "PARM04": (
            "Grid geometry: coordinate system (usingCartesianGrid, "
            "usingSphericalPolarGrid, usingCurvilinearGrid), cell spacings "
            "(delX, delY, delZ), origin (xgOrigin, ygOrigin), "
            "f-plane reference latitude (phiMin, thetaMin)."
        ),
        "PARM05": (
            "Binary input file names: initial conditions (hydrogTFile, hydrogSFile, "
            "uVelInitFile, vVelInitFile), surface forcing (zonalWindFile, meridWindFile, "
            "surfQFile, EmPmRFile), topography (bathyFile, topoFile), "
            "climatology restoring files."
        ),
        "PARM01_ATM2D": "ATM2D atmosphere variant of PARM01 — core dynamics for 2-D atmospheric configuration.",
        "PARM02_ATM2D": "ATM2D atmosphere variant of PARM02 — elliptic solver for 2-D atmospheric configuration.",
        "PARM03_ATM2D": "ATM2D atmosphere variant of PARM03 — time-stepping for 2-D atmospheric configuration.",
        "PARM04_ATM2D": "ATM2D atmosphere variant of PARM04 — grid geometry for 2-D atmospheric configuration.",
        "PARM05_ATM2D": "ATM2D atmosphere variant of PARM05 — input files for 2-D atmospheric configuration.",
    },
    "eedata": {
        "EEPARMS": (
            "Execution environment: MPI tiling (nTx, nTy), usingMPI flag, "
            "cubed-sphere exchange (useCubedSphereExchange). "
            "Must be consistent with SIZE.h nPx/nPy decomposition."
        ),
        "RUNCLOCK": (
            "Wall-clock run limit: maxWallClock (seconds). When the limit is reached "
            "MITgcm writes a checkpoint and exits cleanly. Useful on time-limited "
            "HPC queues; ignored on workstations and Docker runs."
        ),
    },
    "data.pkg": {
        "PACKAGES": (
            "Package activation: useXXX=.TRUE./.FALSE. for all optional packages. "
            "Must be consistent with packages.conf (compile-time package selection). "
            "Activating a package here that was not listed in packages.conf causes "
            "an abort at startup."
        ),
    },
    "data.eos": {
        "EOS_PARM01": (
            "Equation of state parameters for LINEAR EOS: tAlpha (thermal expansion "
            "coefficient, K⁻¹), sBeta (haline contraction coefficient, psu⁻¹), Tref "
            "and Sref (reference temperature and salinity). Required when "
            "eosType='LINEAR' is set in PARM01."
        ),
    },
    "data.exf": {
        "EXF_NML_01": (
            "EXF core: atmospheric state forcing file names "
            "(uWindFile, vWindFile, atempFile, aqhFile, precipFile) and basic options."
        ),
        "EXF_NML_02": (
            "EXF time-averaging: forcing period (exf_inscal_*), "
            "cycle length (exf_monperiod), interpolation and repeat options."
        ),
        "EXF_NML_03": (
            "EXF surface fluxes: shortwave / longwave radiation file names "
            "(swdownFile, lwdownFile), runoff (runoffFile), and supplementary forcing."
        ),
        "EXF_NML_04": (
            "EXF bulk formulae: bulk transfer coefficient options and "
            "sea-ice / atmosphere exchange parameters."
        ),
        "EXF_NML_OBCS": "EXF open-boundary forcing: file names and options for OBC fields supplied via EXF.",
    },
    "data.cal": {
        "CAL_NML": (
            "Calendar package: startDate_1 / startDate_2 (YYYYMMDD / HHMMSS integers), "
            "calendarType ('gregorian', 'noleap', or '360-day'). "
            "Required when using the cal package for date-aware forcing."
        ),
    },
    "data.ctrl": {
        "CTRL_NML": "Adjoint CTRL package: control vector parameters for TAF / OpenAD adjoint runs.",
        "CTRL_NML_GENARR": "Adjoint CTRL generic arrays: definition of control vector components and their weights.",
        "CTRL_PACKNAMES": "Adjoint CTRL package name list: which packages contribute control variables.",
    },
    "data.flt": {
        "FLT_NML": (
            "Float / Lagrangian particle tracker: nFloats, initial positions, "
            "output interval, and trajectory file names."
        ),
    },
    "data.mnc": {
        "MNC_01": (
            "MNC (MITgcm NetCDF) package: output file name stems, variable lists, "
            "time-averaging options, and chunking parameters."
        ),
    },
    "data.gmredi": {
        "GM_PARM01": (
            "Gent-McWilliams / Redi eddy parameterisation: GM bolus velocity scheme "
            "(GM_AdvForm, GM_background_K), isopycnal diffusion coefficients "
            "(GM_isopycKx, GM_isopycKy, GM_isopycKz), and tapering options."
        ),
    },
    "data.exch2": {
        "W2_EXCH2_PARM01": (
            "Cubed-sphere tiled exchange (EXCH2 / W2): tile topology definition, "
            "MPI exchange configuration, and tile connectivity."
        ),
    },
    "data.land": {
        "LAND_MODEL_PAR": "Land model: soil layer depths, heat capacity, runoff routing scheme.",
        "LAND_PHYS_PAR": "Land physics: surface albedo, evapotranspiration, stomatal resistance.",
    },
    "data.cpl": {
        "COUPLER_PARAMS": "Atmosphere–ocean–ice coupler: runtime coupling parameters.",
        "COUPLE_PARM": "Coupler exchange: coupling frequency and field exchange list.",
        "CPL_ATM_PARAM": "Coupler atmospheric component interface parameters.",
        "CPL_OCN_PARAM": "Coupler ocean component interface parameters.",
    },
    "data.icefront": {
        "ICEFRONT_PARM01": "Ice-front package: calving geometry and meltwater forcing parameters.",
        "ICEFRONT_EXF_PARM02": "Ice-front EXF forcing: file names for ice-front boundary condition fields.",
    },
    "data.shap_filt": {
        "SHAP_PARM01": (
            "Shapiro filter: filter order, tracer filter time-scale (Shap_Trtau), "
            "momentum filter time-scale (Shap_uvTau), and directional flags."
        ),
    },
    "data.cost": {
        "cost_nml": "ECCO cost function: basic temperature and salinity misfit cost terms.",
    },
    "data.ecco_cost": {
        "ecco_cost_nml": "ECCO adjoint cost: observation weights and normalisation factors.",
    },
    "data.ecco_gencost": {
        "ecco_gencost_nml": "ECCO generic cost: flexible cost term definitions used in ECCO v4.",
    },
    "data.grdchk": {
        "grdchk_nml": "Gradient check package: finite-difference gradient check parameters for adjoint verification.",
    },
    "data.optim": {
        "optim": "Optimisation package: line-search minimiser parameters for adjoint-based optimisation.",
    },
    "data.profiles": {
        "profiles_nml": "Profiles package: observational profile file names and interpolation options.",
    },
    "data.smooth": {
        "smooth_nml": "Smooth package: spatial smoothing operator parameters for regularisation.",
    },
    "data.obsfit": {
        "obsfit_nml": "Observation-fit package: observation error covariance and fitting parameters.",
    },
    "data.diagnostics": {
        "DIAGNOSTICS_LIST": (
            "Diagnostics output streams: one namelist per output file. "
            "Each stream sets frequency (seconds), output fields "
            "(fields(1:n,k)), file name (filename(k)), and time-averaging "
            "options (timePhase, levels). "
            "Requires numDiags in DIAGNOSTICS_SIZE.h ≥ n_fields × Nr; "
            "the default numDiags = 1×Nr fits only one 3-D field."
        ),
        "DIAG_STATIS_PARMS": (
            "Diagnostics statistical (domain-mean) output: output frequency "
            "and file name for the statistics stream, separate from gridded output."
        ),
    },
    "data.obcs": {
        "OBCS_PARM01": (
            "Open boundary conditions core: which boundaries are open "
            "(OBCSnorthActive, OBCSsouthActive, OBCSeastActive, OBCSwestActive), "
            "boundary relaxation time scale (OBCS_relaxTau*), and "
            "boundary scheme (Stevens, Orlanski, or prescribed)."
        ),
        "OBCS_PARM02": (
            "OBC sponge-layer relaxation: nudging thickness (sponge_*Width) "
            "and time scales for tracer and velocity fields near open boundaries."
        ),
        "OBCS_PARM03": (
            "OBC tidal forcing: tidal constituent amplitudes and phases "
            "for barotropic tidal boundary conditions."
        ),
        "OBCS_PARM04": (
            "OBC passive tracer boundary conditions: file names and "
            "relaxation options for passive tracer open boundaries."
        ),
        "OBCS_PARM05": (
            "OBC runoff forcing: river runoff flux file names and options."
        ),
    },
    "data.kpp": {
        "KPP_PARM01": (
            "KPP (K-Profile Parameterisation) vertical mixing: "
            "boundary layer depth scheme (KPPmixing, KPPbbl), "
            "diffusivity minima and maxima, Langmuir turbulence, and "
            "non-local counter-gradient flux options."
        ),
    },
    "data.rbcs": {
        "RBCS_PARM01": (
            "RBCS restoring / sponge core: time scales (tauRelaxT, tauRelaxS), "
            "forcing period and cycle (rbcsForcingPeriod, rbcsForcingCycle), "
            "and mask / target-field file names (relaxMaskFile, relaxTFile, "
            "relaxSFile). tauRelaxT must never be zero even when mask weights "
            "control the restoring strength."
        ),
        "RBCS_PARM02": (
            "RBCS velocity restoring: file names and time scales for "
            "relaxation of U and V velocity fields."
        ),
    },
    "data.seaice": {
        "SEAICE_PARM01": (
            "Sea-ice dynamics and thermodynamics core: rheology "
            "(SEAICEuseDYNAMICS, SEAICE_strength), albedo parameters, "
            "and ice–ocean heat exchange coefficients."
        ),
        "SEAICE_PARM02": (
            "Sea-ice EVP/LSR solver: rheology solver parameters, "
            "iteration counts, and convergence tolerances."
        ),
        "SEAICE_PARM03": (
            "Sea-ice forcing files: ice concentration, thickness, and "
            "velocity initial condition and forcing file names."
        ),
    },
    "data.ptracers": {
        "PTRACERS_PARM01": (
            "Passive tracer package: number of tracers (PTRACERS_num), "
            "initial condition files (PTRACERS_IC_file), advection scheme "
            "flags per tracer, and tracer names."
        ),
    },
    "data.shelfice": {
        "SHELFICE_PARM01": (
            "Ice-shelf sub-shelf melting: parameterisation scheme "
            "(SHELFICEMassStepping, SHELFICEDensity), heat and freshwater "
            "exchange coefficients, and ice-shelf geometry files."
        ),
    },
}

# Flat lookup: group -> file (for the derivation override path)
_GROUP_TO_FILE: dict[str, str] = {
    group: file
    for file, groups in _EXPLICIT.items()
    for group in groups
}

# ---------------------------------------------------------------------------
# Suffix-stripping derivation rule — covers ~70 package groups not listed above.
# ---------------------------------------------------------------------------

_STRIP_RE = re.compile(
    r"(_PARM\d+(?:_\w+)?|_PAR(?:_\w+)?|_PARAMS?|_NML(?:_[\w\d]+)?|"
    r"_CONST|_FORCING|_COST|_PACKNAMES?|_\d+)$"
)


def _derive_file(group: str) -> str:
    """Derive 'data.<pkg>' from a group name using MITgcm suffix conventions."""
    name = _STRIP_RE.sub("", group)
    return f"data.{name.lower().rstrip('_')}"


def _generic_description(group: str) -> str:
    """One-line description for an index-derived group."""
    pkg = _derive_file(group).removeprefix("data.")
    if "_FORCING" in group:
        kind = "forcing file names"
    elif "_CONST" in group:
        kind = "physical constants"
    elif "_COST" in group:
        kind = "adjoint cost function terms"
    elif "_PACKNAMES" in group:
        kind = "package name list"
    else:
        m = re.search(r"_PARM(\d+)", group)
        n = f" (group {m.group(1)})" if m else ""
        kind = f"runtime parameters{n}"
    return f"Package {pkg} {kind}."


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def get_namelist_structure(db_path: Path = DB_PATH) -> dict[str, dict[str, str]]:
    """Return the MITgcm namelist file → group → description map.

    The map covers:
    - All groups in the DuckDB namelist_refs table (index-derived).
    - Explicit entries for core files and non-obvious package mappings.

    Explicit entries have detailed descriptions; index-derived entries have
    one-line generic descriptions derived from the group name.

    If the DuckDB index is not available, the explicit-only subset is returned.

    Returns
    -------
    dict
        Outer keys are namelist file names (e.g. 'data', 'data.eos', 'data.obcs').
        Inner keys are namelist group names (e.g. 'PARM01', 'OBCS_PARM01').
        Values are description strings.
    """
    result: dict[str, dict[str, str]] = {}

    # Seed with explicit entries (always present, may not be in index)
    for file, groups in _EXPLICIT.items():
        result[file] = dict(groups)

    # Supplement with index-derived entries
    explicit_groups = {g for gs in _EXPLICIT.values() for g in gs}
    try:
        import duckdb

        con = duckdb.connect(str(db_path), read_only=True)
        rows = con.execute(
            "SELECT DISTINCT namelist_group FROM namelist_refs ORDER BY namelist_group"
        ).fetchall()
        con.close()
        for (group,) in rows:
            if group in explicit_groups:
                continue
            file = _derive_file(group)
            result.setdefault(file, {})[group] = _generic_description(group)
    except Exception:
        pass  # DB not available; return explicit-only map

    return dict(sorted(result.items()))

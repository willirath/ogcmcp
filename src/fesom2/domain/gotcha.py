"""FESOM2 configuration gotcha catalogue and keyword search.

Each entry has:
- title    : short identifying name
- keywords : list of lowercase search terms
- summary  : one-sentence description
- detail   : longer explanation with concrete namelist advice
"""


CATALOGUE: list[dict] = [
    {
        "title": "step_per_day × dt must equal exactly 86400 seconds",
        "keywords": [
            "step_per_day", "dt", "time step", "timestep", "timing",
            "86400", "day length", "namelist.config",
        ],
        "summary": (
            "step_per_day in namelist.config must satisfy step_per_day × dt = 86400 "
            "exactly. A mismatch silently drifts the model calendar."
        ),
        "detail": (
            "FESOM2 derives the model time step as dt = 86400 / step_per_day. "
            "If you set step_per_day and dt independently and they do not satisfy "
            "step_per_day × dt = 86400 exactly, the model calendar drifts from wall "
            "time and forcing interpolation is evaluated at wrong times — no error is "
            "raised. Example: step_per_day=48 → dt=1800 s. "
            "Always derive one from the other: dt = 86400 / step_per_day."
        ),
    },
    {
        "title": "Mesh must be partitioned with METIS before first run",
        "keywords": [
            "metis", "partition", "partitioning", "part_format", "npart",
            "mesh", "fesom.mesh", "mesh files", "parallel", "mpi",
        ],
        "summary": (
            "FESOM2 requires METIS-partitioned mesh files before the first run. "
            "Without them the model aborts reading the mesh."
        ),
        "detail": (
            "FESOM2 distributes the unstructured mesh across MPI ranks using METIS "
            "partitioning. The partition files live in a dist_N/ subdirectory of the "
            "mesh directory (where N = number of MPI ranks). They must be generated "
            "before the first model run with the FESOM2 partitioner binary. "
            "The number of MPI ranks at run time must match the partition count. "
            "part_format in namelist.config selects the partition file format "
            "('binary' or 'netcdf'). Changing MPI rank count requires re-partitioning. "
            "In the project's Docker runtime image the partitioner binary is available at "
            "/fesom2/bin/fesom_meshpart (serial, no mpirun needed). "
            "Run it from a work directory containing namelist.config. "
            "Set the &machine group: n_levels=1 (single-level flat partition, correct for "
            "almost all use cases; higher values add METIS hierarchy, rarely needed), "
            "n_part=<N_RANKS>. Set MeshPath in the &paths group to point at the mesh "
            "directory. fesom_meshpart reads MeshPath from namelist.config (&paths group, "
            "same key used by fesom.x) and writes dist_<N>/ into that mesh directory."
        ),
    },
    {
        "title": "ALE minimum layer thickness: min_hnode prevents layer collapse",
        "keywords": [
            "ale", "min_hnode", "layer thickness", "layer collapse", "thin layer",
            "vertical coordinate", "adaptive", "z-star",
        ],
        "summary": (
            "With ALE vertical coordinates, layers can collapse near steep topography "
            "or under strong SSH variability. min_hnode in namelist.oce sets the "
            "minimum allowed layer thickness."
        ),
        "detail": (
            "FESOM2's ALE (Arbitrary Lagrangian-Eulerian) vertical coordinate allows "
            "layer thicknesses to evolve with the sea surface. Near steep topography "
            "or in regions of large SSH variability, layers can collapse to near-zero "
            "thickness, causing numerical instability or time-step violations. "
            "Set min_hnode in namelist.oce to a physically reasonable minimum "
            "(e.g. 1–5 m for z*-like coordinates). If ALE_transform = 'zstar' "
            "in namelist.oce, the minimum applies globally; for sigma coordinates "
            "it applies per column. Too large a min_hnode degrades vertical resolution "
            "in shallow water."
        ),
    },
    {
        "title": "Node vs element variables on the unstructured mesh",
        "keywords": [
            "node", "element", "nod2d", "elem2d", "scalar", "velocity",
            "unstructured", "fem", "interpolation", "output", "regrid",
        ],
        "summary": (
            "FESOM2 stores scalars (T, S, SSH) at mesh nodes and velocities (u, v) "
            "at cell centroids. Mixing up locations causes wrong nearest-neighbour "
            "interpolation when regridding output."
        ),
        "detail": (
            "The FESOM2 unstructured mesh has two types of locations: "
            "nodes (vertices of triangular elements, count = nod2d) and "
            "elements (triangle centroids, count = elem2d). "
            "Scalars — temperature T, salinity S, sea surface height SSH, "
            "tracer concentrations — are defined at nodes. "
            "Horizontal velocities u and v are defined at element centroids. "
            "When regridding output to a regular lat-lon grid, "
            "use the node coordinates (fesom.mesh.diag.nc: lon, lat) for scalars "
            "and the element coordinates (lonc, latc) for velocities. "
            "Regridding velocity with node coordinates shifts the field by half an "
            "element width and produces spurious near-boundary values."
        ),
    },
    {
        "title": "EVP sea ice: alpha and beta subcycling parameters need tuning",
        "keywords": [
            "evp", "mevp", "modified evp", "subcycling", "alpha", "beta",
            "sea ice", "rheology", "ice", "whichevp", "namelist.ice",
        ],
        "summary": (
            "FESOM2 supports classic EVP (whichEVP=1) and modified mEVP (whichEVP=2). "
            "mEVP requires tuning alpha and beta; wrong values give unconverged "
            "ice stress and noisy velocity fields."
        ),
        "detail": (
            "FESOM2 sea ice rheology is selected by whichEVP in namelist.ice: "
            "whichEVP=1 is classic EVP (Hunke & Dukowicz 1997) with a fixed subcycling "
            "count (evp_rheol_steps); whichEVP=2 is mEVP (Bouillon et al. 2013, "
            "Danilov et al. 2015) which uses damping parameters alpha and beta. "
            "For mEVP, alpha and beta must be large enough for stability "
            "(rule of thumb: alpha = beta ≈ 500 for typical ocean configurations) "
            "but not so large that convergence is slow. "
            "Wrong alpha/beta produces noisy, unconverged ice velocity fields that "
            "look physically reasonable but violate the rheology. "
            "Validate by checking that the EVP residual decreases over subcycling steps."
        ),
    },
    {
        "title": "Forcing interpolation weights must be precomputed",
        "keywords": [
            "forcing", "interpolation", "weights", "interp", "era5", "core2",
            "jra", "jra55", "fesom.mesh", "forcing.weight", "namelist.forcing",
        ],
        "summary": (
            "FESOM2 interpolates atmospheric forcing from regular grids to the "
            "unstructured mesh using precomputed weight files. Missing weights "
            "cause an immediate abort at the forcing read step."
        ),
        "detail": (
            "FESOM2 does not compute interpolation weights on the fly. "
            "Before the first run with a new mesh or new forcing dataset, "
            "run the FESOM2 interpolation weight generator (or pyFESOM2 tools) "
            "to produce the weight files referenced in namelist.forcing. "
            "The weight files encode the mapping from the atmospheric forcing grid "
            "(ERA5: 0.25°, CORE2: ~1.875°, JRA55: ~0.5625°) to each FESOM2 mesh node. "
            "If the weight files are absent or generated for a different mesh, "
            "the model aborts at the first forcing read with a file-not-found or "
            "dimension-mismatch error. "
            "Recompute weights whenever the mesh changes."
        ),
    },
    {
        "title": "Output fields must be listed explicitly in namelist.io",
        "keywords": [
            "output", "namelist.io", "diagnostic", "io_list", "list", "variable",
            "netcdf", "fesom.nc", "snapshot", "mean", "output_list",
        ],
        "summary": (
            "FESOM2 only writes variables explicitly listed under the output blocks "
            "in namelist.io. Unlisted fields produce no output without any warning."
        ),
        "detail": (
            "Unlike models with a separate diagnostics package, FESOM2 controls "
            "output entirely through namelist.io. Each output block specifies a list "
            "of variable names, an output frequency (in days), and whether to write "
            "snapshots or time means. Variables not listed are silently omitted — "
            "no warning is printed. "
            "Standard 3D ocean variables: 'temp', 'salt', 'u', 'v', 'w'. "
            "Sea ice variables: 'ice_area', 'ice_hice', 'ice_uice', 'ice_vice'. "
            "SSH: 'ssh'. "
            "Check the namelist.io in a reference setup (e.g. setups/CORE/) "
            "for the canonical variable list, and verify that output files are "
            "created immediately after starting the run."
        ),
    },
    {
        "title": "toy_ocean bypasses the forcing pipeline; wind is read from a mesh file",
        "keywords": [
            "toy_ocean", "toy ocean", "which_toy", "neverworld2", "soufflet", "dbgyre",
            "windstress", "windstress@elem", "wind file", "forcing pipeline", "bulk forcing",
            "namelist.forcing", "toy",
        ],
        "summary": (
            "When toy_ocean=.true., FESOM2 bypasses the bulk atmospheric forcing pipeline "
            "entirely. Wind stress is read from a static file (windstress@elem.out) in "
            "MeshPath, not from namelist.forcing. Setting namelist.forcing for a toy run "
            "has no effect and will mislead users expecting CORE2/JRA behaviour."
        ),
        "detail": (
            "FESOM2 toy configurations (which_toy = 'neverworld2', 'soufflet', 'dbgyre') "
            "use hard-coded or file-based forcing defined in the toy source modules "
            "(toy_channel_neverworld2.F90, etc.). "
            "With toy_ocean=.true., the gen_forcing machinery is not entered — "
            "namelist.forcing parameters (wind_data_source, CORE_forcing_dir, etc.) "
            "are read but ignored. "
            "Wind stress for Neverworld2 and Soufflet is loaded from "
            "'windstress@elem.out' in MeshPath at model startup; this file must exist "
            "before the first run. It is generated by the mesh setup scripts, not by "
            "the FESOM2 binary itself. "
            "Temperature restoring (surf_relax_T in namelist.oce) still applies when "
            "toy_ocean=.true. if set to a non-zero value."
        ),
    },
    {
        "title": "Toy module behavioural flags are compile-time constants, not namelist parameters",
        "keywords": [
            "do_wind", "do_trelax", "do_tpert", "tau_inv", "toy module", "toy variable",
            "module variable", "compile time", "runtime", "namelist parameter",
            "neverworld2", "soufflet", "which_toy",
        ],
        "summary": (
            "Key switches in FESOM2 toy modules (do_wind, do_Trelax, do_Tpert, tau_inv) "
            "are Fortran module-level variables, not namelist parameters. They cannot be "
            "changed at runtime — the source must be edited and the model recompiled."
        ),
        "detail": (
            "The FESOM2 toy configuration modules (e.g. toy_channel_neverworld2.F90) "
            "define their behavioural flags as module-level Fortran variables with "
            "default values set in the source: "
            "do_wind (enable wind stress), do_Trelax (enable SST restoring), "
            "do_Tpert (add initial temperature perturbation), tau_inv (restoring rate). "
            "These do not appear in any namelist file and cannot be changed via "
            "namelist.config or any other input file. "
            "To run without wind stress, or to disable temperature restoring, "
            "edit the corresponding module source file and recompile. "
            "namelist_to_code_tool will correctly report these as 'not a namelist "
            "parameter' — this is expected behaviour, not an indexing gap."
        ),
    },
    {
        "title": "use_cavity must be enabled for ice-shelf cavity runs",
        "keywords": [
            "cavity", "ice shelf", "ice-shelf", "use_cavity", "isomip",
            "weddell", "amundsen", "sub-shelf",
        ],
        "summary": (
            "Ice-shelf cavity circulation requires use_cavity=.true. in namelist.config "
            "and a mesh that includes cavity elements. Forgetting either causes a "
            "flat-bottomed ocean with no sub-shelf circulation."
        ),
        "detail": (
            "FESOM2 supports ice-shelf cavity dynamics via the cavity module. "
            "Two conditions must both be met: "
            "(1) use_cavity = .true. in namelist.config to activate the cavity code path; "
            "(2) the mesh must include cavity nodes and elements below the ice-shelf draft. "
            "Using use_cavity=.true. with a mesh that has no cavity produces no error "
            "but the cavity code operates on zero-thickness cavity columns. "
            "Using a cavity mesh without use_cavity=.true. treats the ice-shelf base "
            "as the ocean bottom — the cavity space is ignored and sub-shelf exchange "
            "is absent. Cavity meshes are typically provided as separate mesh directories "
            "(e.g. CORE_mesh_cavity/ vs CORE_mesh/)."
        ),
    },
]


def lookup_gotcha(topic: str) -> list[dict]:
    """Search the FESOM2 gotcha catalogue by keyword.

    Case-insensitive keyword search. Any keyword phrase from the catalogue
    that appears in the topic string triggers a match.

    Parameters
    ----------
    topic : str
        Free-text search string (e.g. "ALE", "EVP sea ice", "forcing").

    Returns
    -------
    list[dict]
        Matching entries with keys: title, keywords, summary, detail.
        Empty list if no match.
    """
    topic_lower = topic.lower()
    results = []
    for entry in CATALOGUE:
        for kw in entry["keywords"]:
            if kw in topic_lower:
                results.append(entry)
                break
    return results

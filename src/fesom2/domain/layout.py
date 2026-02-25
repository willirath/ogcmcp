"""Experiment directory layout and Docker run interface for FESOM2."""

_LAYOUT = {
    "description": (
        "A self-contained FESOM2 experiment has three layers: mesh, namelists, output. "
        "Each is an independent directory mounted into the Docker container at runtime. "
        "Mesh and namelists are version-controlled; output is gitignored and reproduced by running."
    ),
    "directory_structure": {
        "mesh/": {
            "nod2d.out": "Node coordinates: [V; index lon lat tag]",
            "elem2d.out": "Triangular connectivity: [C; n1 n2 n3], 1-indexed",
            "aux3d.out": "Level depths + bathymetry: [L; h_1…h_L; H_1…H_V]",
            "windstress.out": "(toy only) lat/tau wind stress profile, read from MeshPath when wind_opt=1",
            "windstress@elem.out": "(toy only) pre-interpolated wind stress at elements, used when wind_opt=2",
            "edges.out / edge_tri.out / edgenum.out / elvls.out / nlvls.out": (
                "Derived geometry written by fesom_ini.x — commit for small meshes, else regenerate"
            ),
            "dist_N/": (
                "METIS partition files for N MPI ranks (rpart.out, my_list*.out, com_info*.out). "
                "One dist_N/ per target rank count. Generated once by fesom_ini.x; safe to commit "
                "for small meshes."
            ),
            "create_mesh.py": "Mesh provenance script — always commit alongside the generated files",
        },
        "input/": {
            "namelist.config": "Run length, clock, paths (MeshPath/ResultPath/ClimateDataPath patched at runtime), n_part, which_toy",
            "namelist.oce": "Ocean dynamics: EOS, mixing scheme, GM/Redi, bottom drag",
            "namelist.tra": "Tracer physics: diffusivities, IC file references, surface restoring",
            "namelist.dyn": "Momentum viscosity and solver options",
            "namelist.cvmix": "CVMix library coefficients (KPP, TKE, PP)",
            "namelist.ice": "Sea-ice dynamics and thermodynamics",
            "namelist.io": "Output variable list, frequencies, precision",
            "namelist.forcing": "Bulk-formula coefficients and forcing file paths",
        },
        "output/": "NetCDF output + fesom.clock — gitignored, created at runtime",
        "run.sh": "docker run command — commit alongside the experiment",
        "plot.py": "Visualisation script — commit alongside the reference figure",
        "README.md": "Physical setup, file tree, how-to-run",
    },
    "docker_interface": {
        "command_template": (
            "EXP=/path/to/experiment\n"
            "docker run --rm \\\n"
            "  -v $EXP/mesh:/mesh:ro \\\n"
            "  -v $EXP/input:/input:ro \\\n"
            "  -v $EXP/output:/output \\\n"
            "  ghcr.io/willirath/ogcmcp:fesom2-latest"
        ),
        "mounts": {
            "/mesh (ro)": "mesh files: nod2d.out, elem2d.out, aux3d.out, dist_N/, windstress.out, ...",
            "/input (ro)": "all eight namelist files",
            "/output (rw)": "receives NetCDF output and fesom.clock",
        },
        "entrypoint_contract": {
            "patches": [
                "MeshPath → '/mesh/'",
                "ResultPath → '/output/'",
                "ClimateDataPath → '/dev/null/'",
            ],
            "reads_from_namelist": [
                "n_part — number of MPI ranks to launch",
                "timenew / daynew / yearnew — used to write fesom.clock for cold start",
            ],
            "never_overrides": [
                "run_length",
                "step_per_day",
                "which_toy",
                "any physics parameter",
            ],
            "writes": [
                "/output/fesom.clock — cold-start clock (two identical 'time day year' lines)",
            ],
        },
    },
    "gitignore_convention": (
        "experiments/*/*/output/    # model output — always gitignored\n"
        "experiments/*/*/mesh/dist_*/  # add when meshes grow beyond git-friendly size"
    ),
    "notes": [
        (
            "namelist.config must contain MeshPath, ResultPath, ClimateDataPath as empty strings (''); "
            "the entrypoint patches them. Never set real paths in committed namelists."
        ),
        (
            "toy_ocean=.true. bypasses the bulk forcing pipeline. namelist.forcing must still exist "
            "(FESOM2 opens it unconditionally at startup) but its contents are ignored."
        ),
        (
            "For real (NetCDF-forced) experiments, add a fourth mount: "
            "-v /path/to/forcing:/forcing:ro and set forcing paths in namelist.forcing "
            "to '/forcing/<prefix>'."
        ),
        (
            "fesom_ini.x (the METIS partitioner) must be run once per (mesh, n_part) pair "
            "to produce dist_N/. If dist_N/ already exists in /mesh, fesom.x uses it directly "
            "and no repartitioning is needed."
        ),
    ],
}


def get_run_interface() -> dict:
    """Return the FESOM2 experiment directory layout and Docker run interface.

    Returns
    -------
    dict
        Keys:
        - "description"         : str — overview of the three-layer structure
        - "directory_structure" : dict — annotated file tree for one experiment
        - "docker_interface"    : dict — Docker run command, mount semantics,
                                  entrypoint contract (what it patches vs reads)
        - "gitignore_convention": str — gitignore patterns for output and large meshes
        - "notes"               : list[str] — common gotchas and conventions
    """
    return _LAYOUT

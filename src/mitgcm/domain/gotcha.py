"""MITgcm configuration gotcha catalogue and keyword search.

The catalogue is a static list of known MITgcm configuration traps.
Each entry has:
- title    : short identifying name
- keywords : list of lowercase search terms
- summary  : one-sentence description
- detail   : longer explanation with concrete namelist/CPP advice
"""


CATALOGUE: list[dict] = [
    {
        "title": "Non-hydrostatic pressure solve requires CPP flag",
        "keywords": ["nonhydrostatic", "non-hydrostatic", "pressure", "cg3d", "nhm"],
        "summary": (
            "nonHydrostatic = .TRUE. requires ALLOW_NONHYDROSTATIC in CPP_OPTIONS.h "
            "and useNHMTerms = .TRUE. in &PARM01."
        ),
        "detail": (
            "Setting nonHydrostatic = .TRUE. in data &PARM01 is not sufficient on its own. "
            "The package must also be compiled in: add '#define ALLOW_NONHYDROSTATIC' to "
            "CPP_OPTIONS.h and set 'useNHMTerms = .TRUE.' in &PARM01. Without the CPP flag "
            "the non-hydrostatic terms are silently omitted."
        ),
    },
    {
        "title": "Linear EOS in freshwater: sBeta must be explicitly zero",
        "keywords": ["linear eos", "eos", "freshwater", "salinity", "talpha", "sbeta", "data.eos"],
        "summary": (
            "eosType='LINEAR' requires tAlpha and sBeta in data.eos &EOS_PARM01. "
            "Omitting sBeta=0 leaves a spurious salinity-driven density gradient."
        ),
        "detail": (
            "For a freshwater tank with linear EOS: set eosType='LINEAR' in data &PARM01, "
            "then in data.eos &EOS_PARM01 set tAlpha=<value> and sBeta=0. If sBeta is "
            "omitted, MITgcm uses its default, which can produce a spurious density "
            "gradient from the initial salinity field."
        ),
    },
    {
        "title": "Spin-up time to solid-body rotation",
        "keywords": ["spin-up", "spinup", "spin up", "solid-body", "initial condition", "rotation period"],
        "summary": (
            "Solid-body spin-up takes O(Ek^{-1/2}) rotation periods. Imposing forcing "
            "before spin-up is complete contaminates the experiment."
        ),
        "detail": (
            "After filling the tank and starting rotation, the fluid takes approximately "
            "Ek^{-1/2} rotation periods to reach solid-body rotation via Ekman pumping. "
            "For typical lab Ekman numbers (1e-4 to 1e-3) this is 30-100 rotation periods. "
            "A common mistake is starting the thermal forcing immediately. Let the model "
            "(and the lab) spin up first."
        ),
    },
    {
        "title": "Sidewall boundary condition defaults to free-slip",
        "keywords": ["sidewall", "no-slip", "free-slip", "boundary condition", "no_slip_sides", "wall"],
        "summary": (
            "MITgcm defaults to free-slip sidewalls. "
            "Set no_slip_sides = .TRUE. in &PARM01 to match solid tank walls."
        ),
        "detail": (
            "MITgcm's default lateral boundary condition is free-slip (no_slip_sides = .FALSE.). "
            "Real tank walls are no-slip. Set 'no_slip_sides = .TRUE.' in data &PARM01. "
            "Similarly, 'no_slip_bottom = .TRUE.' for a no-slip bottom. Forgetting these in "
            "a tank simulation gives unrealistically strong flows near the walls."
        ),
    },
    {
        "title": "Pressure reference level for shallow tanks",
        "keywords": ["pressure", "reference", "eosrefp0", "boussinesq", "compressibility"],
        "summary": (
            "Set EosRefP0 to surface pressure for a shallow tank. "
            "Use Boussinesq approximation."
        ),
        "detail": (
            "In a shallow tank, the pressure variation with depth is small and "
            "compressibility effects are negligible. Set EosRefP0=1.013e5 (Pa, surface "
            "atmospheric pressure) in data &PARM01 and use the Boussinesq approximation "
            "(implicitFreeSurface or rigidLid). Using ocean-default pressure reference "
            "levels for a 20 cm tank will give incorrect density."
        ),
    },
    {
        "title": "Rigid lid vs free surface",
        "keywords": ["rigid lid", "free surface", "rigidlid", "implicitfreesurface", "barotropic", "surface"],
        "summary": (
            "Use rigidLid=.TRUE. for a tank with a solid lid. "
            "Free surface requires implicitFreeSurface=.TRUE. and a barotropic time-step check."
        ),
        "detail": (
            "For a tank with a physical rigid lid: set 'rigidLid = .TRUE.' in data &PARM01. "
            "For a free surface: set 'implicitFreeSurface = .TRUE.' instead. Free surface "
            "runs require checking the barotropic CFL — the surface gravity wave speed "
            "sqrt(g*H) sets a much shorter time-step limit than the baroclinic CFL. "
            "Consider using the implicit free surface solver with a long barotropic time step."
        ),
    },
    {
        "title": "Diagnostics output frequency is in seconds",
        "keywords": ["diagnostics", "frequency", "output", "data.diagnostics", "time step", "seconds"],
        "summary": (
            "data.diagnostics output frequency (frequency(n), timePh(n)) is in seconds, "
            "not time steps."
        ),
        "detail": (
            "In data.diagnostics, the 'frequency' and 'timePhase' fields for each "
            "diagnostics list are in model seconds, not in number of time steps. A common "
            "error is setting frequency=100 expecting output every 100 time steps, when it "
            "actually means every 100 seconds. Divide your desired output interval in time "
            "steps by deltaT to get the correct value in seconds."
        ),
    },
    {
        "title": "DIAGNOSTICS_SIZE.h numDiags must cover all requested fields",
        "keywords": [
            "diagnostics_size", "numdiags", "diagnostics size", "not enough space",
            "active diagnostics", "diagnostics_set_pointers",
        ],
        "summary": (
            "The compile-time parameter numDiags in DIAGNOSTICS_SIZE.h must be at least "
            "(number of requested fields) × Nr. The default is 1×Nr, which only fits one "
            "3-D diagnostic field."
        ),
        "detail": (
            "MITgcm's default DIAGNOSTICS_SIZE.h sets numDiags = 1*Nr (e.g. 40 for Nr=40). "
            "If you request more than one 3-D field in data.diagnostics, the model aborts at "
            "startup with 'Not enough space for all active diagnostics: numDiags=40 but "
            "needs at least 160'. Fix: copy DIAGNOSTICS_SIZE.h into your experiment's code/ "
            "directory and increase numDiags (e.g. numDiags = 8*Nr gives room for 8 3-D "
            "fields). Because this is a compile-time change, a full clean build is required "
            "— incremental builds do not update the symlinks genmake2 creates."
        ),
    },
    {
        "title": "RBCS mask is dimensionless 0–1; tauRelaxT must be non-zero",
        "keywords": [
            "rbcs", "relaxation", "restoring", "taurelax", "taurelaxT", "relaxmaskfile",
            "relaxtfile", "bottom cooling", "rbcsforcingperiod",
        ],
        "summary": (
            "The RBCS relaxMaskFile contains dimensionless weights in [0,1]; tauRelaxT [s] "
            "is the restoring timescale. tauRelaxT must never be zero even when the mask "
            "is used to control strength."
        ),
        "detail": (
            "RBCS tendency: dT/dt += -mask(i,j,k) * (1/tauRelaxT) * (T - T_relax). "
            "relaxMaskFile holds dimensionless weights (0 = no restoring, 1 = full restoring "
            "at rate 1/tauRelaxT). A common mistake is putting 1/tau directly into the mask "
            "file and leaving tauRelaxT=0, which causes a division-by-zero abort at startup. "
            "Set tauRelaxT to the desired timescale in seconds and put 0–1 weights in the "
            "mask file. For steady forcing set rbcsForcingPeriod=0 and rbcsForcingCycle=0."
        ),
    },
    {
        "title": "readBinaryPrec mismatch: Python writes 64-bit, MITgcm reads 32-bit",
        "keywords": [
            "readbinaryprec", "binary precision", "float64", "float32",
            "precision", "numpy dtype", "input field", "wrong results",
        ],
        "summary": (
            "MITgcm defaults to readBinaryPrec=32 (single-precision). Writing input "
            "fields as float64 (NumPy default) produces silent wrong results."
        ),
        "detail": (
            "MITgcm reads binary input fields at the precision set by readBinaryPrec "
            "in data &PARM01 (default: 32). NumPy's default dtype is float64 (64-bit). "
            "If gen_input.py writes fields with numpy.tofile() without setting dtype, "
            "the bytes are interpreted as 32-bit floats with the wrong bit pattern — "
            "no error is raised, but density, velocity, and temperature fields will be "
            "garbage. Fix: explicitly cast before writing: "
            "array.astype('>f4').tofile('field.bin'). "
            "Alternatively set readBinaryPrec=64 in data &PARM01 and write float64. "
            "The same rule applies to writeBinaryPrec for output fields."
        ),
    },
    {
        "title": "INCLUDE_PHIHYD_CALCULATION_CODE must be defined in CPP_OPTIONS.h",
        "keywords": [
            "phihyd", "include_phihyd", "config_check", "configcheck",
            "hydrostatic pressure", "phihydf", "phihydc",
        ],
        "summary": (
            "INCLUDE_PHIHYD_CALCULATION_CODE must be #defined in CPP_OPTIONS.h. "
            "Without it MITgcm aborts at startup with a CONFIG_CHECK error."
        ),
        "detail": (
            "MITgcm's CONFIG_CHECK subroutine (called at initialisation) verifies "
            "that INCLUDE_PHIHYD_CALCULATION_CODE is defined. If CPP_OPTIONS.h does "
            "not contain '#define INCLUDE_PHIHYD_CALCULATION_CODE', the model aborts "
            "immediately with a message like 'CONFIGURATION ERROR: "
            "INCLUDE_PHIHYD_CALCULATION_CODE is required'. "
            "Add '#define INCLUDE_PHIHYD_CALCULATION_CODE' to code/CPP_OPTIONS.h in "
            "your experiment directory. This flag enables the hydrostatic pressure "
            "calculation and is required for virtually all MITgcm experiments."
        ),
    },
    {
        "title": "packages.conf: gfd group must be listed, not just individual packages",
        "keywords": [
            "packages_check", "packages.conf", "mom_fluxform", "gfd", "package group",
            "packages check", "unknown package",
        ],
        "summary": (
            "packages.conf must include the 'gfd' group line. Listing only individual "
            "packages like 'mom_fluxform' causes PACKAGES_CHECK to abort."
        ),
        "detail": (
            "MITgcm's PACKAGES_CHECK validates packages.conf against a known set of "
            "groups and packages. 'gfd' is the group that includes mom_fluxform, "
            "mom_common, and related dynamics packages. "
            "If packages.conf lists 'mom_fluxform' without the 'gfd' group, the build "
            "may fail or PACKAGES_CHECK aborts at runtime. "
            "Minimal working packages.conf for most experiments:\n"
            "  gfd\n"
            "  diagnostics\n"
            "Add other packages (rbcs, obcs, …) as needed. "
            "Use search_docs_tool('packages.conf') or a verification experiment as reference."
        ),
    },
    {
        "title": "SIZE.h must include MAX_OLX / MAX_OLY PARAMETER block",
        "keywords": [
            "size.h", "max_olx", "max_oly", "overlap", "exch.h", "eesupp",
            "compilation error", "undeclared", "sNx", "sNy",
        ],
        "summary": (
            "A custom SIZE.h must declare MAX_OLX and MAX_OLY PARAMETER statements "
            "in addition to sNx, sNy, Nr, nPx, nPy. Omitting them breaks the EXCH.h "
            "inclusion chain with an 'undeclared' compilation error."
        ),
        "detail": (
            "MITgcm's eesupp/inc/EXCH.h and related headers use MAX_OLX and MAX_OLY "
            "to size exchange buffers. A minimal SIZE.h must contain:\n"
            "      PARAMETER ( sNx = ..., sNy = ..., OLx = 2, OLy = 2,\n"
            "     &             nSx = 1, nSy = 1, nPx = ..., nPy = ...,\n"
            "     &             Nx  = sNx*nSx*nPx, Ny  = sNy*nSy*nPy,\n"
            "     &             Nr  = ... )\n"
            "      PARAMETER ( MAX_OLX = OLx, MAX_OLY = OLy )\n"
            "Without MAX_OLX / MAX_OLY the compilation aborts with 'Symbol "
            "MAX_OLX has not been explicitly declared'. "
            "Use search_docs_tool('SIZE.h') to find a complete example from a "
            "verification experiment."
        ),
    },
    {
        "title": "Vertical CFL limit for explicit advection with convective velocities",
        "keywords": [
            "cfl", "vertical cfl", "convection", "instability", "blow up", "nan",
            "tempadvscheme", "advection scheme", "vertical velocity",
        ],
        "summary": (
            "Implicit diffusion does not stabilise explicit advection. Convective "
            "downwellings can violate the vertical CFL (w*dt/dz < 0.5) even when "
            "diffusion is implicit."
        ),
        "detail": (
            "MITgcm's implicitDiffusion=.TRUE. only applies to the diffusive (Laplacian) "
            "term; tracer and momentum advection remain explicit and are subject to the "
            "CFL criterion w*dt/dz < 0.5. In convection experiments, vertical velocities "
            "can grow to O(mm/s – cm/s) over the first few hundred seconds, causing the "
            "CFL to exceed 0.5 and the solution to diverge to NaN. Two mitigations: "
            "(1) reduce deltaT so that w_max*dt/dz < 0.5 with margin; "
            "(2) add tempAdvScheme=33 (&PARM01) to enable the PPM monotone advection "
            "scheme, which is more stable near sharp gradients. Halving deltaT roughly "
            "doubles the required run time; use nTimeSteps accordingly."
        ),
    },
    {
        "title": "Adams-Bashforth 3: β=0.281105 gives maximum CFL stability limit",
        "keywords": [
            "adams-bashforth", "adams bashforth", "ab3", "adamsbashforth3",
            "beta_ab", "alph_ab", "cfl", "timestep", "stability", "parm03",
        ],
        "summary": (
            "AB-3 has two useful configurations: standard (β=5/12, CFL limit 0.724) "
            "and maximum-stability (β=0.281105, CFL limit 0.786). "
            "Both are controlled via alph_AB and beta_AB in data &PARM03."
        ),
        "detail": (
            "MITgcm's Adams-Bashforth 3 scheme uses:\n"
            "  g^{n+1/2} = (1 + α + β) g^n  -  (α + 2β) g^{n-1}  +  β g^{n-2}\n"
            "Two named configurations:\n"
            "  Standard AB-3:          α=0.5,  β=5/12  → CFL stability limit 0.724\n"
            "  Maximum stability:      β=0.281105       → CFL stability limit 0.786\n"
            "The maximum-stability β is not a named mode — to use it, set:\n"
            "  beta_AB = 0.281105\n"
            "in data &PARM03. Both alph_AB and beta_AB are PARM03 parameters. "
            "The scheme automatically ramps up: first-order at iter 0, AB-2 at iter 1, "
            "AB-3 from iter 2 onward (no instability at startup). "
            "ALLOW_ADAMSBASHFORTH_3 must be defined in CPP_OPTIONS.h; without it "
            "the model falls back to AB-2."
        ),
    },
    {
        "title": "EXACT_CONSERV is now mandatory — #undef causes startup abort",
        "keywords": [
            "exact_conserv", "exactconserv", "cpp_options", "retired",
            "mandatory", "startup abort", "config_check",
        ],
        "summary": (
            "EXACT_CONSERV was once optional but is now a required CPP flag. "
            "Old CPP_OPTIONS.h files with '#undef EXACT_CONSERV' cause a "
            "CONFIG_CHECK startup abort."
        ),
        "detail": (
            "MITgcm's CONFIG_CHECK aborts at startup if '#undef EXACT_CONSERV' "
            "is present in CPP_OPTIONS.h:\n"
            "  #ifndef EXACT_CONSERV\n"
            "    CONFIG_CHECK: #undef EXACT_CONSERV not allowed in CPP_OPTIONS.h\n"
            "    since CPP option EXACT_CONSERV has been retired\n"
            "  #endif\n"
            "The confusing error message says the flag 'has been retired', which "
            "actually means it is now always-on (mandatory). The flag is no longer "
            "optional — you must not undefine it. "
            "Fix: remove the '#undef EXACT_CONSERV' line from code/CPP_OPTIONS.h. "
            "If your CPP_OPTIONS.h was copied from an old verification experiment, "
            "check for this line and delete it."
        ),
    },
    {
        "title": "-mpi flag required in genmake2 even for single-process runs",
        "keywords": [
            "-mpi", "genmake2", "mpi", "single process", "np=1", "mpirun",
            "mpi_home", "mpich", "openmpi", "link error",
        ],
        "summary": (
            "genmake2 must be invoked with -mpi even when NP=1. "
            "Without it the MPI-enabled executable cannot be linked."
        ),
        "detail": (
            "MITgcm's genmake2 configures the build for either serial or MPI execution. "
            "If the Dockerfile uses 'mpirun -np $NP ./mitgcmuv' at runtime, the binary "
            "must be compiled with MPI support — add '-mpi' to the genmake2 invocation "
            "and set MPI_HOME to the MPICH (or OpenMPI) library path. "
            "Omitting -mpi produces a serial binary; mpirun will then fail at startup "
            "with a link error or silently run only one process regardless of NP. "
            "Example for MPICH on amd64:\n"
            "  MPI=true MPI_HOME=/usr/lib/x86_64-linux-gnu/mpich \\\n"
            "    /MITgcm/tools/genmake2 -rootdir /MITgcm -mods /experiment/code \\\n"
            "      -optfile /MITgcm/tools/build_options/linux_amd64_gfortran -mpi\n"
            "Note: MPICH's hydra launcher does NOT support --allow-run-as-root; "
            "omit that flag entirely."
        ),
    },
    {
        "title": "Closed basin leaks volume if boundary rows are ocean cells",
        "keywords": [
            "closed basin", "volume conservation", "ssh drift", "bathymetry",
            "open boundary", "land mask", "halo", "free surface", "land wall",
            "bathy", "perimeter",
        ],
        "summary": (
            "In a closed-basin configuration, all four boundary rows/columns of the "
            "bathymetry file must be set to 0 (land) or fluid leaks into halo ghost "
            "cells, causing steady SSH drift (~cm/day)."
        ),
        "detail": (
            "MITgcm enforces no-normal-flow at a boundary face only when the adjacent "
            "cell has hFacC = 0 (land). If the northernmost or southernmost row (or "
            "east/west column) in bathy.bin is ocean (depth < 0), there is no land cell "
            "to anchor the no-flux condition on that face. The halo exchange then allows "
            "a non-zero normal velocity component into the ghost region, creating a "
            "spurious volume source or sink. The symptom is a monotonic, spatially uniform "
            "drift in area-weighted mean SSH that is absent with zero forcing and grows "
            "linearly with wind stress amplitude. The fix is to ensure all four perimeter "
            "rows/columns of bathy.bin are set to 0. "
            "Note: when using OBCS, boundary rows may legitimately be ocean cells — the "
            "open-boundary machinery provides the correct flux condition. The problem "
            "arises only when OBCS is absent."
        ),
    },
]


def lookup_gotcha(topic: str) -> list[dict]:
    """Search the MITgcm gotcha catalogue by keyword.

    The search is case-insensitive. Any keyword phrase from the catalogue that
    appears in the topic string triggers a match.

    Parameters
    ----------
    topic : str
        Free-text search string (e.g. "nonhydrostatic", "linear EOS").

    Returns
    -------
    list[dict]
        Matching entries, each with keys: title, keywords, summary, detail.
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

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

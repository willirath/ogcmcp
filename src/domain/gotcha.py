"""Rotating-tank MITgcm gotcha catalogue and keyword search.

The catalogue is a static list of known configuration traps for rotating-tank
MITgcm experiments. Each entry has:
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
]


def lookup_gotcha(topic: str) -> list[dict]:
    """Search the rotating-tank gotcha catalogue by keyword.

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

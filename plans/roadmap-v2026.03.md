# Roadmap: v2026.03

Target: code quality, tool surface improvements, and backlog from the
v2026.02.5 pre-release review sessions.

Done when: all checklist items below are ticked, tests pass, both images
built and pushed, release tagged.

---

## Tier 1 — Remaining v2026.02.5 items that slipped

Items that were identified during v2026.02.5 pre-release testing but are
not blocking the v2026.02.5 tag (either addressed partially or deferred).

### 1.1 `.h` snippet quality

`search_docs_tool` results for `.h` files show only the opening Fortran
comment block in the snippet; actual PARAMETER/COMMON declarations start
further in. The fix is query-time (no re-embedding): strip the
`[file] section\n` prefix and leading `C`-comment lines before taking
the 400-char snippet in `search_docs` and `search_verification` in
`src/tools.py`.

- [ ] Fix snippet generation in `search_docs` and `search_verification`
- [ ] Update tests

### 1.2 `get_cpp_requirements_tool` returns hardware-optimisation flags

`get_cpp_requirements_tool("CG3D")` returns `TARGET_NEC_SX` — a 1990s NEC
SX vector-machine flag irrelevant to any modern build. The tool reports all
`#ifdef` guards found anywhere in the translation unit, not just those that
gate the subroutine entry point. Affects user trust in the output.

- [ ] Assess: filter known hardware-platform flags (`TARGET_NEC_SX`,
  `TARGET_SGI`, `TARGET_CRAY_VECTOR`) from `get_cpp_requirements` results,
  or add a note to the tool description
- [ ] Update tool description and/or filter logic
- [ ] Update tests

---

## Tier 2 — Code quality (from 2026-02-23 review)

See `plans/test-sessions/2026-02-23-v2026.02.5-pre-release-test-creative.md`
and the code review output for full context.

### 2.1 Database connection boilerplate

8 functions in `src/tools.py` repeat the same `con = connect(); try: ...
finally: con.close()` pattern (~120 LoC of ceremony). Extract a
`query_db(sql, params, db_path)` helper or context manager.

- [ ] Extract DB connection helper
- [ ] Apply to all 8 query functions
- [ ] Verify tests still pass

### 2.2 ChromaDB accessor deduplication

`src/embedder/store.py` has three near-identical `get_*_collection()`
functions. Collapse to `get_collection(name: str, path: Path) ->
chromadb.Collection`.

- [ ] Refactor to single accessor
- [ ] Update all call sites in `src/tools.py`

### 2.3 Ollama embed call extraction

Model name `"nomic-embed-text"` and `_normalize_query()` are repeated at
three call sites in `src/tools.py`. Extract to
`def embed_query(query: str) -> list[float]`.

- [ ] Extract helper
- [ ] Apply to `search_code`, `search_docs`, `search_verification`

### 2.4 Positional column indexing in SQL results

8 query functions in `src/tools.py` use `r[0], r[1], r[2]` positional
indexing on DuckDB result rows. Brittle on schema changes. Switch to named
tuples or row factory.

- [ ] Use DuckDB named column access or `fetchdf()`
- [ ] Apply to all 8 query functions

---

## Tier 3 — Gotcha catalogue and docs additions

### 3.1 Adams-Bashforth 3 maximum-stability coefficient

The `β = 0.281105` optimum (CFL stability limit 0.786 vs 0.724 for
standard AB-3) is documented only in source comments in `ADAMS_BASHFORTH3`.
Users trying to push timestep size have no way to find it.

- [ ] Add note to gotcha catalogue or `docs/mcp-server.md`
- [ ] Both `alph_AB` and `beta_AB` confirmed as PARM03 parameters

### 3.2 CD scheme search gap

`search_docs_tool("CD scheme Coriolis Crank Nicolson momentum")` returns
Adams-Bashforth sections, not CD scheme docs. The `cd_code` package is not
surfaced under user-facing search terms.

- [ ] Assess whether the RST docs for cd_code are indexed and under what
  section heading
- [ ] If gap is in indexing: no action needed (add to known limitations)
- [ ] If gap is in search terms: add a cross-reference in relevant docs

### 3.3 `showflops` and unregistered package flags

`get_package_flags_tool("showflops")` returns empty because the package's
CPP flags are not registered in the standard options file. Affects any
package that uses non-standard flag registration.

- [ ] Audit which other packages have the same gap
- [ ] Decide: document as known limitation, or extend the flags indexer

---

## Tier 4 — Docker hardening

From 2026-02-23 security review (grade A — no vulnerabilities, hardening only).

### 4.1 Non-root user in MCP image

Container currently runs as root. Add a dedicated non-root user.

- [ ] Add `RUN useradd -u 1000 -m -s /sbin/nologin mitgcm` and `USER mitgcm`
  to `docker/mcp/Dockerfile`
- [ ] Verify entrypoint still works
- [ ] Add `--security-opt no-new-privileges` to install docs example

---

## Release checklist

- [ ] All tier 1–4 items done
- [ ] `pixi run test` passes
- [ ] Both images built and pushed (`mcp-v2026.03.x`, `runtime-v2026.03.x`)
- [ ] GitHub release created
- [ ] Git tag pushed
- [ ] `docs/release.md` VERSION updated
- [ ] `.mcp.json` updated
- [ ] `README.md` and `docs/mcp-server.md` install commands updated

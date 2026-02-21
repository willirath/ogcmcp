# Roadmap

## M0 — Environment

Set up the project environment and pin the MITgcm source.

- [x] `pixi.toml` with all dependencies (`fastapi`, `chromadb>=1.0`, `duckdb`, `tree-sitter` via PyPI; `ollama`, `python=3.13` via conda-forge)
- [x] MITgcm added as a git submodule (pinned at `decd05a`)
- [x] `ollama pull nomic-embed-text` confirmed working (Docker: `docker compose up -d`, then `docker compose exec ollama ollama pull nomic-embed-text`)
- [x] `docs/environment.md`
- [x] `docs/mitgcm-source.md`
- [x] `docs/embeddings.md`

**Done when:** `pixi run python -c "import duckdb, chromadb, ollama, tree_sitter"` succeeds. ✓

---

## M1 — DuckDB index

Parse MITgcm source into the code graph.

- [x] Regex-based extraction from raw `.F` and `.F90` (no CPP pre-pass needed; tree-sitter too fragile for MITgcm fixed-form)
- [x] Populate `metadata`, `subroutines`, `calls`, `namelist_refs`, `diagnostics_fills`, `cpp_guards` (2433 subroutines indexed)
- [x] 51 tests (unit + adversarial); two bugs found and fixed
- [x] `docs/parsing.md`, `docs/duckdb.md`, `docs/indexer.md`, `docs/testing.md`

**Done when:** SQL query `SELECT * FROM namelist_refs WHERE param_name = 'cg3dMaxIters'` returns results. ✓

---

## M2 — ChromaDB index

Embed subroutines and load into the vector store.

- [x] Embed each subroutine with `nomic-embed-text` via Ollama (chunked, MAX_CHARS=4000, OVERLAP=200)
- [x] Write to ChromaDB with metadata (file, package, subroutine name, db_id, chunk index)
- [x] `docs/chromadb.md`

**Done when:** A natural-language query for `"non-hydrostatic pressure solve"` returns relevant subroutines. ✓

---

## M3 — Core tools

Implement code-navigation tools as plain Python functions (no server yet).

- [x] `search_code(query, top_k)`
- [x] `get_subroutine(name)`
- [x] `get_callers(name)` / `get_callees(name)`
- [x] `namelist_to_code(param, namelist_file=None)`
- [x] `diagnostics_fill_to_source(field_name)`
- [x] `get_cpp_requirements(subroutine_name)`
- [x] `get_package_flags(package_name)`
- [x] `docs/tools.md`

**Done when:** Can answer "what code reads `cg3dMaxIter`, and what CPP flag guards it?" from a Python shell. ✓

---

## M4 — MCP server

Wrap tools and expose them over MCP.

- [x] `mcp` (FastMCP, stdio) server wiring all M3 tools
- [x] Confirmed connectable from Claude Code / Claude Desktop
- [x] `docs/mcp-server.md`

**Done when:** A live LLM session can call `get_subroutine("cg3d")` and get source back. ✓

---

## M5 — Domain knowledge skeleton

First version of the tank-specific knowledge layer.

- [x] `translate_lab_params` — formula-based, covers f0, deltaX, viscAz, tAlpha
- [x] `check_scales` — computes Ro, Ek, Bu, δ, CFL; flags issues
- [x] `lookup_gotcha` — queryable catalogue, seeded from MITgcm rotating tank tutorial and docs
- [x] `suggest_experiment_config` — skeleton configs for at least rotating convection and Eady setups
- [x] `docs/domain-knowledge.md`

**Done when:** Given tank radius, depth, and rotation rate, the system returns a draft namelist and a scaling summary with any flags. ✓

---

## M6 — MITgcm runtime environment

Containerised MITgcm build that runs locally via Docker and translates directly to Singularity on HPC.

- [x] `Dockerfile` — single-stage: gfortran + OpenMPI + NetCDF-Fortran on Ubuntu 24.04
- [x] `scripts/build-experiment.sh` and `scripts/run-experiment.sh` wrappers with volume mounts for MITgcm source and experiment directory
- [x] `pixi run build-image` / `build-tutorial` / `run-tutorial` tasks
- [x] `experiments/tutorial_rotating_tank/` — committed code and input text files; SIZE.h modified for nPx=2
- [x] `scripts/setup-tutorial.sh` — copies binary input files from submodule
- [x] `docs/runtime.md`

**Done when:** `verification/tutorial_rotating_tank` runs to completion inside the container and produces output pickup files. ✓

---

## M7 — First real experiment

Walk one experiment end-to-end through the system.

- [x] Pick a concrete tank setup (e.g. rotating convection)
- [x] Use the system to generate a full configuration
- [x] Run it in MITgcm via the M6 runtime
- [x] Feed gaps and corrections back into M5

**Done when:** The experiment runs to completion and any knowledge gaps found are closed. ✓

---

## M8 — MITgcm documentation index

Index the MITgcm documentation into a second ChromaDB collection so the
system can answer questions from prose (parameter descriptions, package
tutorials, known gotchas) rather than only from source code.

- [x] Parse RST source from `MITgcm/doc/` — strip directives, extract plain text per section
- [x] Embed and load into a `mitgcm_docs` ChromaDB collection (same embedding pipeline as M2)
- [x] `search_docs(query, top_k)` tool in `src/tools.py` and exposed via MCP server
- [x] `docs/docs-index.md`

**Done when:** A query for `"how to set the Coriolis parameter"` returns the relevant parameter reference section from the MITgcm docs. ✓

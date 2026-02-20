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

- [ ] `search_code(query, top_k)`
- [ ] `get_subroutine(name)`
- [ ] `get_callers(name)` / `get_callees(name)`
- [ ] `namelist_to_code(param, namelist_file=None)`
- [ ] `diagnostics_fill_to_source(field_name)`
- [ ] `get_cpp_requirements(subroutine_name)`
- [ ] `get_package_flags(package_name)`
- [ ] `docs/tools.md`

**Done when:** Can answer "what code reads `cg3dMaxIter`, and what CPP flag guards it?" from a Python shell.

---

## M4 — MCP server

Wrap tools and expose them over MCP.

- [ ] FastAPI + `mcp` server wiring all M3 tools
- [ ] Confirmed connectable from Claude Code / Claude Desktop
- [ ] `docs/mcp-server.md`

**Done when:** A live LLM session can call `get_subroutine("cg3d")` and get source back.

---

## M5 — Domain knowledge skeleton

First version of the tank-specific knowledge layer.

- [ ] `translate_lab_params` — formula-based, covers f0, deltaX, viscAz, tAlpha
- [ ] `check_scales` — computes Ro, Ek, Bu, δ, CFL; flags issues
- [ ] `lookup_gotcha` — queryable catalogue, seeded from MITgcm rotating tank tutorial and docs
- [ ] `suggest_experiment_config` — skeleton configs for at least rotating convection and Eady setups
- [ ] `docs/domain-knowledge.md`

**Done when:** Given tank radius, depth, and rotation rate, the system returns a draft namelist and a scaling summary with any flags.

---

## M6 — First real experiment

Walk one experiment end-to-end through the system.

- [ ] Pick a concrete tank setup (e.g. rotating convection)
- [ ] Use the system to generate a full configuration
- [ ] Run it in MITgcm
- [ ] Feed gaps and corrections back into M5

**Done when:** The experiment runs to completion and any knowledge gaps found are closed.

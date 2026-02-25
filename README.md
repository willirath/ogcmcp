# OGCMCP

MCP servers that give Claude Code live access to ocean model source code,
documentation, and domain knowledge layers for experiment design.
Currently supports MITgcm and FESOM2; ask questions in natural language
and the tools retrieve answers directly from the indexed source and docs.

---

## Install

Requires Docker and Claude Code or Codex CLI.

### MITgcm MCP server

**Claude Code:**
```bash
claude mcp add --transport stdio --scope user mitgcm -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:mitgcm-mcp-v2026.02.6
```

**Codex CLI:**
```bash
codex mcp add mitgcm -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:mitgcm-mcp-v2026.02.6
```

### FESOM2 MCP server

**Claude Code:**
```bash
claude mcp add --transport stdio --scope user fesom2 -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:fesom2-mcp-v2026.02.6
```

**Codex CLI:**
```bash
codex mcp add fesom2 -- \
  docker run --rm -i ghcr.io/willirath/ogcmcp:fesom2-mcp-v2026.02.6
```

Docker pulls the image on first use (~600 MB per image — includes Ollama,
the embedding model, and pre-built indices).

---

## Example — MITgcm

```
User: How does MITgcm handle MPI processes?

→ search_docs_tool("MPI parallel processing decomposition")
  ["Machine model parallelism", "Tiles", ...]

→ find_subroutines_tool("INI_PROCS")
  [{"name": "INI_PROCS", "file": "eesupp/src/ini_procs.F", ...}]

→ get_source_tool("EEBOOT_MINIMAL", limit=80)
  [MPI_Init → MPI_Cart_create with nPx × nPy topology ...]

Claude: The decomposition is compile-time: SIZE.h sets nPx, nPy, nSx, nSy.
        The number of MPI ranks must equal nPx × nPy.
```

## Example — FESOM2

```
User: What controls GM diffusivity in FESOM2?

→ namelist_to_code_tool("K_GM")
  [{"name": "oce_mixing", "namelist_group": "oce_mixing", "file": "...", ...}]

→ get_source_tool("oce_mixing")
  [K_GM declared in &oce_mixing, used in GM parameterisation routines ...]

Claude: K_GM in namelist.oce controls the Gent-McWilliams isopycnal
        diffusivity. Default is 1000 m²/s; typical tuning range 500–2000.
```

---

## Tools

Call `get_workflow_tool` at the start of a session to get a recommended
tool sequence for your task.

### MITgcm — 23 tools

#### Code navigation

| Tool | What it does |
|---|---|
| `search_code_tool` | Semantic search over subroutine source |
| `find_subroutines_tool` | Find subroutines by name |
| `get_subroutine_tool` | Metadata for a subroutine (no source) |
| `get_source_tool` | Paginated source lines |
| `get_callers_tool` | What calls this subroutine |
| `get_callees_tool` | What this subroutine calls |
| `find_packages_tool` | All packages with subroutine counts |
| `get_package_tool` | Package metadata + subroutine list + CPP flags |
| `namelist_to_code_tool` | Which subroutine reads a namelist parameter |
| `diagnostics_fill_to_source_tool` | Which subroutine fills a diagnostics field |
| `get_cpp_requirements_tool` | CPP flags that guard a subroutine |
| `get_package_flags_tool` | CPP flags defined by a package |

#### Documentation + verification

| Tool | What it does |
|---|---|
| `search_docs_tool` | Semantic search over RST docs and `.h` headers |
| `get_doc_source_tool` | Full text of a doc section or header file |
| `list_verification_experiments_tool` | Catalogue of all verification experiments |
| `search_verification_tool` | Semantic search over verification configs |
| `get_verification_source_tool` | Full text of a verification experiment file |

#### Domain knowledge + workflow

| Tool | What it does |
|---|---|
| `translate_lab_params_tool` | Physical parameters → namelist values |
| `check_scales_tool` | Dimensionless numbers, CFL/Ekman flags |
| `lookup_gotcha_tool` | Known configuration traps by keyword |
| `suggest_experiment_config_tool` | Skeleton config for an experiment type |
| `get_namelist_structure_tool` | Map of all namelist files → groups |
| `get_workflow_tool` | Recommended tool sequence for a task |

### FESOM2 — 20 tools

#### Code navigation

| Tool | What it does |
|---|---|
| `search_code_tool` | Semantic search over subroutine source |
| `find_modules_tool` | Find F90 modules by name |
| `get_module_tool` | Module metadata + contained subroutines |
| `get_module_uses_tool` | Modules USEd by a module (dependency tracing) |
| `find_subroutines_tool` | Find subroutines by name |
| `get_subroutine_tool` | Metadata for a subroutine (no source) |
| `get_source_tool` | Paginated source lines |
| `get_callers_tool` | What calls this subroutine |
| `get_callees_tool` | What this subroutine calls |
| `namelist_to_code_tool` | Which subroutine reads a namelist parameter |

#### Documentation + setups

| Tool | What it does |
|---|---|
| `search_docs_tool` | Semantic search over FESOM2 RST docs and namelist descriptions |
| `get_doc_source_tool` | Full text of a doc section |
| `list_setups_tool` | Reference namelists and CI setup catalogue |

#### Domain knowledge + workflow

| Tool | What it does |
|---|---|
| `translate_lab_params_tool` | Physical parameters → namelist values |
| `check_scales_tool` | Dimensionless numbers, CFL/Ekman flags |
| `lookup_gotcha_tool` | Known configuration traps by keyword |
| `get_run_interface_tool` | Experiment directory layout and Docker mount interface |
| `suggest_experiment_config_tool` | Skeleton namelists for an experiment type |
| `get_namelist_structure_tool` | Map of all namelist files → groups |
| `get_workflow_tool` | Recommended tool sequence for a task |

---

## For developers

Requires [pixi](https://pixi.sh) and Docker.

```bash
git clone --recurse-submodules https://github.com/willirath/ogcmcp
cd ogcmcp
pixi install

# Start the Ollama embedding server
docker compose up -d
docker compose exec ollama ollama pull nomic-embed-text   # first time only

# Build the MITgcm indices
pixi run mitgcm-index    # Fortran → DuckDB (~2 min)
pixi run mitgcm-embed    # subroutines → ChromaDB (~45 min)

# Build the FESOM2 indices
pixi run fesom2-index
pixi run fesom2-embed
pixi run fesom2-embed-docs
pixi run fesom2-embed-namelists

# Run tests
pixi run test

# Start servers (Claude Code launches automatically via .mcp.json)
pixi run mitgcm-serve
pixi run fesom2-serve
```

---

## Layout

```
.
├── src/
│   ├── shared/        Physics utilities shared by both backends
│   ├── mitgcm/        MITgcm backend (server, tools, indexer, embedder, domain)
│   └── fesom2/        FESOM2 backend (server, tools, indexer, embedder, domain)
├── tests/
│   ├── shared/        Tests for shared physics utilities
│   ├── mitgcm/        MITgcm backend tests
│   └── fesom2/        FESOM2 backend tests
├── experiments/
│   ├── mitgcm/        MITgcm experiment definitions
│   └── fesom2/        FESOM2 experiment definitions
├── docker/
│   ├── mitgcm/        MITgcm build image
│   ├── mcp/           MITgcm MCP image (Ollama + model + indices)
│   ├── fesom2/        FESOM2 runtime image
│   └── fesom2-mcp/    FESOM2 MCP image
├── docs/              Implementation notes and design rationale
├── plans/             Design docs and release roadmap
├── MITgcm/            MITgcm source (git submodule, pinned @ decd05a — checkpoint69k)
├── FESOM2/            FESOM2 source (git submodule, pinned @ 1b58e7f)
├── compose.yml        Ollama service for development
└── .mcp.json          Claude Code MCP server config
```

## Docs

| File | Covers |
|---|---|
| [`docs/architecture.md`](docs/architecture.md) | Dual-backend design, tool inventory, Docker images |
| [`docs/mcp-server.md`](docs/mcp-server.md) | MITgcm MCP tools in detail |
| [`docs/fesom2-experiment-defs.md`](docs/fesom2-experiment-defs.md) | FESOM2 three-layer experiment structure |
| [`docs/fesom2-runtime.md`](docs/fesom2-runtime.md) | FESOM2 Docker runtime interface |
| [`docs/fesom2-release.md`](docs/fesom2-release.md) | FESOM2 release process |
| [`docs/release.md`](docs/release.md) | MITgcm release process |
| [`docs/domain-knowledge.md`](docs/domain-knowledge.md) | Domain knowledge layer |
| [`docs/duckdb.md`](docs/duckdb.md) | Code graph schema, example queries |
| [`docs/chromadb.md`](docs/chromadb.md) | Embedding pipeline, chunking |
| [`docs/environment.md`](docs/environment.md) | pixi / Python environment |

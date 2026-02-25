# Architecture

OGCMCP is a collection of MCP servers that give Claude Code live access to
ocean model source code, documentation, and domain knowledge. Each model
backend is an independent, self-contained server; users install only the
backends they need.

---

## Two backends, same pattern

```
src/
├── shared/            physics utilities shared by all backends
│   ├── translate.py   lab/ocean parameters → namelist values
│   └── scales.py      dimensionless numbers and CFL/Ekman checks
├── mitgcm/            MITgcm backend
│   ├── server.py      FastMCP server (21 tools)
│   ├── tools.py       plain Python callables over DuckDB + ChromaDB
│   ├── indexer/       Fortran parser → DuckDB (subroutines, packages, calls)
│   ├── embedder/      embedding pipeline → ChromaDB
│   ├── docs_indexer/  RST doc parser + embedder
│   ├── verification_indexer/  MITgcm verification experiment catalogue
│   └── domain/        gotchas, experiment configs, workflow guidance
└── fesom2/            FESOM2 backend
    ├── server.py      FastMCP server (20 tools)
    ├── tools.py       plain Python callables over DuckDB + ChromaDB
    ├── indexer/       F90 module parser → DuckDB (modules, subroutines, uses)
    ├── embedder/      embedding pipeline → ChromaDB
    ├── setups.py      reference namelist + CI setup catalogue
    └── domain/        gotchas, experiment configs, layout, workflow guidance
```

Both backends follow the same four-layer pattern:

| Layer | Purpose |
|---|---|
| **Code graph** (DuckDB) | Subroutine/module metadata, call graph, namelist refs |
| **Semantic search** (ChromaDB + Ollama) | Natural-language queries over source and docs |
| **Domain knowledge** (pure Python) | Physics calculations, gotchas, skeleton configs |
| **MCP server** (FastMCP) | Exposes everything as tools over stdio |

Indices live in separate namespaces under `data/`:

```
data/
├── mitgcm/
│   ├── index.duckdb   MITgcm code graph
│   └── chroma/        MITgcm embeddings
└── fesom2/
    ├── index.duckdb   FESOM2 code graph
    └── chroma/        FESOM2 embeddings
```

---

## When to use which backend

| If you're working with… | Use |
|---|---|
| MITgcm source, packages, CPP flags, verification experiments | `mitgcm` MCP server |
| FESOM2 F90 modules, namelist parameters, toy experiments | `fesom2` MCP server |
| Lab-to-model parameter translation or dimensionless numbers | Either — both expose `translate_lab_params_tool` and `check_scales_tool` via `src.shared` |

---

## Key architectural differences

| | MITgcm | FESOM2 |
|---|---|---|
| Language | Fortran 77/90 (fixed-form) | Fortran 90 (free-form modules) |
| Compile-time config | CPP flags (`#ifdef`) | CMake only |
| Organisational unit | **package** (logical, multi-file) | **module** (language construct, one file) |
| Experiment config | Namelists + `SIZE.h` CPP + `packages.conf` | 8 namelist files only |
| Index key | `package` column on subroutines | `module_name` column on subroutines |

This drives the tool asymmetry: MITgcm has `get_package_tool`, `find_packages_tool`,
`get_cpp_requirements_tool`, `get_package_flags_tool`, and verification experiment
tools; FESOM2 has `find_modules_tool`, `get_module_tool`, `get_module_uses_tool`,
`list_setups_tool`, and `get_run_interface_tool`. All other tools are symmetric.

---

## Docker images

Four images, two per backend:

| Image | Tag prefix | Purpose |
|---|---|---|
| MITgcm runtime | `ghcr.io/willirath/ogcmcp:mitgcm-runtime-*` | Build environment for experiment Dockerfiles |
| MITgcm MCP | `ghcr.io/willirath/ogcmcp:mitgcm-mcp-*` | Self-contained MCP server (Ollama + indices baked in) |
| FESOM2 runtime | `ghcr.io/willirath/ogcmcp:fesom2-runtime-*` | Run toy/real experiments via Docker mounts |
| FESOM2 MCP | `ghcr.io/willirath/ogcmcp:fesom2-mcp-*` | Self-contained MCP server for FESOM2 |

---

## Shared utilities

`src/shared/` contains model-agnostic physics tools imported by both servers:

- `translate_lab_params` — converts physical lab/ocean geometry to namelist values
- `check_scales` — computes dimensionless numbers (Ro, Ek, Bu, CFL) and flags issues

`src/embed_utils.py` and `src/rst_parser.py` are shared infrastructure used by
the embedding and documentation indexing pipelines of both backends.

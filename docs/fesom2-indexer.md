# FESOM2 indexer

The FESOM2 indexer reads F90 source files and namelist config files, extracts
code structure, and writes a DuckDB code graph to `data/fesom2/index.duckdb`.
It lives in `src/fesom2/indexer/`.

Unlike the MITgcm indexer, the organisational unit here is the **F90 module**
(a language construct, one file per module) rather than a logical package
spanning multiple files. CPP guards are not tracked — FESOM2 uses CMake only.

---

## Modules

### `schema.py` — database setup

Defines `DB_PATH` (`data/fesom2/index.duckdb`) and the DDL. Creates the
parent directory automatically. Tables:

| Table | Purpose |
|---|---|
| `metadata` | Key/value pairs — FESOM2 commit SHA, index timestamp |
| `modules` | One row per F90 MODULE |
| `subroutines` | One row per subroutine or function |
| `uses` | Module-level USE dependencies (`module_name → used_module`) |
| `calls` | Subroutine-level CALL edges (`caller_name, caller_module → callee_name`) |
| `namelist_refs` | Namelist declarations from source (`param_name, group, file, module_name, line`) |
| `namelist_descriptions` | Param descriptions from config files (`param_name, group, config_file, description`) |

The `namelist_refs` table tracks *where* parameters are declared in F90
source. `namelist_descriptions` holds the human-readable descriptions from
`FESOM2/config/namelist.*` inline comments — a separate concern.

### `extract.py` — F90 extractor

Parses free-form Fortran 90. Public entry point:

```python
extract_file(path: Path) -> tuple[list[ModuleRecord], list[SubroutineRecord]]
```

**What gets extracted:**

- `MODULE name … END MODULE` blocks → `ModuleRecord` with line range and lists
  of `uses`, `namelist_groups`
- `SUBROUTINE` / `FUNCTION` inside a module → `SubroutineRecord` with
  `module_name`, line range, source text, `calls`
- Top-level subroutines (outside any `MODULE`) → `module_name` is set to the
  file stem
- `pFUnit` test files (`.pf`): `@macro` lines are stripped before parsing
- `USE module_name` at module or subroutine scope → rolled up to the
  enclosing module's `uses` list
- `CALL target` inside subroutine bodies → `calls` list (deduplicated,
  order-preserving)
- `NAMELIST /group/ param1, param2, …` at module scope → `namelist_groups`
  list with free-form continuation support

CPP `#ifdef`/`#endif` lines are passed through without tracking — FESOM2
uses only CMake configuration.

### `namelist_config.py` — config file parser

Parses `FESOM2/config/namelist.*` files to extract parameter descriptions.

FESOM2 ships reference config files where each parameter assignment carries
an inline comment explaining the parameter:

```fortran
&namelist_oce
K_GM = 1000.0  ! GM diffusivity (m²/s)
/
```

`namelist_config.py` reads all `FESOM2/config/namelist.*` files and
extracts `(config_file, group_name, param_name, description)` tuples. If
a parameter appears in multiple variant files (e.g. `namelist.oce` and
`namelist.oce.core2`), the first-seen description is kept. Results are
written to the `namelist_descriptions` DuckDB table by the pipeline.

### `pipeline.py` — orchestration

Enumerates source files, drives extraction, and writes to DuckDB.

**Source directories indexed:**

```
FESOM2/src/*.F90, *.pf           core dynamics
FESOM2/src/cvmix_driver/         CVMix vertical mixing driver
FESOM2/src/icepack_drivers/      Icepack sea-ice driver
FESOM2/src/ifs_interface/        IFS atmosphere interface
FESOM2/src/int_recom/recom_*.F90 REcoM biogeochemistry drivers (top-level only)
FESOM2/test/fortran/**/*.pf      pFUnit unit tests
FESOM2/test/fortran_parallel/**/*.pf  pFUnit parallel tests
```

`FESOM2/src/int_recom/recom/` (the REcoM library itself) and
`FESOM2/src/async_threads_cpp/` (C++) are excluded.

`run()` in order:

1. Opens DuckDB via `connect()`.
2. Records FESOM2 git HEAD SHA and timestamp in `metadata`.
3. Enumerates source files.
4. Calls `extract_file(path)` for each file; inserts modules, subroutines,
   uses, calls, and namelist_refs.
5. Calls `parse_all_config_files()` and inserts results into
   `namelist_descriptions`.
6. Closes the connection.

---

## Data flow

```
FESOM2/src/**/*.F90, *.pf
FESOM2/test/**/*.pf
        │
        ▼
  extract_file()           ModuleRecord + SubroutineRecord per file
        │
        ▼
  pipeline.run()           iterates files, writes to DuckDB
        │
        ▼
data/fesom2/index.duckdb   modules, subroutines, uses, calls,
                           namelist_refs

FESOM2/config/namelist.*
        │
        ▼
 namelist_config.py         (config_file, group, param, description)
        │
        ▼
data/fesom2/index.duckdb   namelist_descriptions
```

---

## Running the indexer

```sh
pixi run fesom2-index
```

The FESOM2 submodule must exist at `FESOM2/`. On first checkout:

```sh
git submodule update --init FESOM2
```

To rebuild from scratch:

```sh
rm -f data/fesom2/index.duckdb
pixi run fesom2-index
```

After updating the FESOM2 submodule, rebuild the index and all embeddings:

```sh
git submodule update --remote FESOM2
git add FESOM2
git commit -m "Update FESOM2 submodule to <sha>"
rm -f data/fesom2/index.duckdb
pixi run fesom2-index
pixi run fesom2-embed
pixi run fesom2-embed-docs
pixi run fesom2-embed-namelists
```

---

## Key differences from MITgcm indexer

| | MITgcm | FESOM2 |
|---|---|---|
| Fortran dialect | Fixed-form (`.F`) + free-form (`.F90`) | Free-form (`.F90`) only |
| Organisational unit | Package (logical, multi-file) | Module (F90 construct, one file) |
| CPP tracking | `#ifdef`/`#endif` → `cpp_guards` table | Not tracked |
| Namelist descriptions | Not indexed | From `config/namelist.*` inline comments |
| Test files | Not indexed | pFUnit `.pf` files indexed |
| Schema key | `package` column on subroutines | `module_name` column on subroutines |

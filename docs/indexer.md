# Indexer

The indexer reads MITgcm Fortran source files and writes a structured code
graph to DuckDB. It lives in `src/mitgcm/indexer/` and consists of three modules with
a clear one-directional dependency: `pipeline` drives `extract`, which returns
data; `schema` is used by `pipeline` to set up and connect to the database.

## Modules

### `schema.py` — database setup

Defines `DB_PATH` (`data/mitgcm/index.duckdb`) and `DDL` (a multi-statement string
that creates all tables with `CREATE TABLE IF NOT EXISTS`). Exposes a single
function:

```python
connect(path: Path = DB_PATH) -> duckdb.DuckDBPyConnection
```

Calling `connect()` opens or creates the DuckDB file and runs the DDL. The
caller is responsible for closing the connection. The tables created are:

| Table | Purpose |
|---|---|
| `metadata` | Key/value pairs — commit SHA, index timestamp |
| `subroutines` | One row per extracted subroutine |
| `calls` | Each (caller_id, callee_name) edge |
| `namelist_refs` | Each (param_name, subroutine_id, namelist_group) triple |
| `diagnostics_fills` | Each (field_name, subroutine_id, array_name) triple |
| `cpp_guards` | Each (subroutine_id, cpp_flag) pair |
| `package_options` | Package/CPP-flag descriptions (populated externally) |

See `docs/duckdb.md` for the full schema and example queries.

### `extract.py` — Fortran extractor

Contains the regex patterns and parsing logic. The public entry point is:

```python
extract_file(path: Path) -> list[SubroutineRecord]
```

A `SubroutineRecord` dataclass carries everything extracted for one subroutine:

```python
@dataclass
class SubroutineRecord:
    name: str
    file: str
    package: str
    line_start: int       # 1-indexed
    line_end: int         # 1-indexed, inclusive
    source_text: str
    calls: list[str]
    namelist_params: list[tuple[str, str]]   # (param, group)
    diag_fills: list[tuple[str, str]]        # (field_name, array_name)
    cpp_guards: list[str]
```

`extract_file` does two sequential passes over the file's lines:

1. **CPP guard pass** — walks every line and maintains a stack for
   `#ifdef` / `#ifndef` / `#endif`. Produces `line_guards: list[frozenset]`,
   one entry per line, recording which flags are active at that line.

2. **Subroutine pass** — finds `SUBROUTINE name` lines, locates the matching
   `END` / `END SUBROUTINE`, then scans the body for `CALL`, `NAMELIST`,
   and `DIAGNOSTICS_FILL` patterns. For each subroutine it takes the union of
   `line_guards` entries across its line range to produce `cpp_guards`.

The extractor handles both fixed-form (`.F`) and free-form (`.F90`). The
`fixed_form` flag is derived from the file suffix and controls comment
recognition (`C`/`*`/`!` in column 1) and continuation detection (non-blank,
non-zero in column 6).

See `docs/parsing.md` for the regex design and Fortran dialect specifics.

### `pipeline.py` — orchestration

Defines which source directories to walk and drives the full indexing run:

```python
SOURCE_DIRS = [
    MITgcm / "model" / "src",
    MITgcm / "pkg",
    MITgcm / "eesupp" / "src",
]
```

`run(db_path)` does the following in order:

1. Calls `connect()` to open the database.
2. Reads the MITgcm git HEAD SHA via `git rev-parse HEAD` and writes it plus
   the current UTC timestamp to `metadata`.
3. Enumerates all `.F` and `.F90` files under the three source directories.
4. Calls `extract_file(path)` for each file.
5. Writes each `SubroutineRecord` across five tables with a monotonically
   increasing integer `sub_id`.
6. Closes the connection.

`calls` entries are deduplicated at the extractor level (order-preserving).
`diag_fills` entries are not deduplicated — multiple calls to
`DIAGNOSTICS_FILL` for the same field in the same subroutine are all recorded.

## Data flow

```
MITgcm/.F and .F90 files
        |
        v
  extract_file()          one SubroutineRecord per subroutine
        |
        v
   pipeline.run()         iterates files, calls extract_file
        |
        v
  schema.connect()        opens / creates data/mitgcm/index.duckdb
        |
        v
  INSERT statements       subroutines, calls, namelist_refs,
                          diagnostics_fills, cpp_guards
```

## Running the indexer

```sh
pixi run mitgcm-index
```

The MITgcm source tree must exist at `MITgcm/` (it is a git submodule;
run `git submodule update --init MITgcm` on first checkout).
Output is written to `data/mitgcm/index.duckdb`. To rebuild from scratch:

```sh
rm -f data/mitgcm/index.duckdb
pixi run mitgcm-index
```

**Data inputs** — directories walked by the pipeline:

| Directory | Contents |
|---|---|
| `MITgcm/model/src/` | Core dynamical kernel |
| `MITgcm/pkg/` | Optional packages (diagnostics, obcs, kpp, …) |
| `MITgcm/eesupp/src/` | Execution environment (threading, I/O) |

`MITgcm/verification/` is used by the verification embedder
(`pixi run mitgcm-embed-verification`) but not by the indexer.

## Extending the indexer

### Adding a new extraction pattern

1. Add a compiled regex constant to `extract.py` near the other `RE_*`
   definitions.
2. In `extract_file`, inside the body-scanning loop (`while k <= sub_end`),
   apply the regex and append results to a new list on `SubroutineRecord`.
3. Add the new list field to `SubroutineRecord` with a `field(default_factory=list)`.
4. Add a matching column or table to `schema.py` DDL.
5. In `pipeline.run()`, add the corresponding `INSERT` loop after the existing
   ones.
6. Add tests in `tests/mitgcm/indexer/test_extract.py` covering the normal case and
   at least one edge case (continuation lines, comments, etc.).

### Changing which directories are indexed

Edit the `SOURCE_DIRS` list in `pipeline.py`. The extractor itself is
path-agnostic; `_package_from_path` derives the package name from path
components (`pkg/<name>`, `model`, `eesupp`) and falls back to `'unknown'`
for any other location.

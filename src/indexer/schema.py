"""DuckDB schema creation and connection."""

import duckdb
from pathlib import Path

DB_PATH = Path("index.duckdb")

DDL = """
CREATE TABLE IF NOT EXISTS metadata (
    key   TEXT PRIMARY KEY,
    value TEXT
);

CREATE TABLE IF NOT EXISTS subroutines (
    id          INTEGER PRIMARY KEY,
    name        TEXT,
    file        TEXT,
    package     TEXT,
    line_start  INTEGER,
    line_end    INTEGER,
    source_text TEXT
);

CREATE TABLE IF NOT EXISTS calls (
    caller_id   INTEGER,
    callee_name TEXT
);

CREATE TABLE IF NOT EXISTS namelist_refs (
    param_name      TEXT,
    subroutine_id   INTEGER,
    namelist_group  TEXT
);

CREATE TABLE IF NOT EXISTS diagnostics_fills (
    field_name      TEXT,
    subroutine_id   INTEGER,
    array_name      TEXT
);

CREATE TABLE IF NOT EXISTS cpp_guards (
    subroutine_id   INTEGER,
    cpp_flag        TEXT
);

CREATE TABLE IF NOT EXISTS package_options (
    package_name    TEXT,
    cpp_flag        TEXT,
    description     TEXT
);
"""


def connect(path: Path = DB_PATH) -> duckdb.DuckDBPyConnection:
    con = duckdb.connect(str(path))
    con.execute(DDL)
    return con

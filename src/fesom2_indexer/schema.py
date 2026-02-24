"""DuckDB schema for FESOM2 code graph."""

import duckdb
from pathlib import Path

DB_PATH = Path("data/fesom2/index.duckdb")

DDL = """
CREATE TABLE IF NOT EXISTS metadata (
    key   TEXT PRIMARY KEY,
    value TEXT
);

CREATE TABLE IF NOT EXISTS modules (
    id          INTEGER PRIMARY KEY,
    name        TEXT,
    file        TEXT,
    start_line  INTEGER,
    end_line    INTEGER
);

CREATE TABLE IF NOT EXISTS subroutines (
    id          INTEGER PRIMARY KEY,
    name        TEXT,
    module_name TEXT,
    file        TEXT,
    start_line  INTEGER,
    end_line    INTEGER,
    source_text TEXT
);

-- USE statements at module level: which modules does each module depend on
CREATE TABLE IF NOT EXISTS uses (
    module_name  TEXT,
    used_module  TEXT
);

-- CALL statements within subroutines
CREATE TABLE IF NOT EXISTS calls (
    caller_name   TEXT,
    caller_module TEXT,
    callee_name   TEXT
);

-- Namelist parameter references (populated by Tier 2)
CREATE TABLE IF NOT EXISTS namelist_refs (
    param_name     TEXT,
    namelist_group TEXT,
    file           TEXT,
    module_name    TEXT,
    line           INTEGER
);
"""


def connect(path: Path = DB_PATH) -> duckdb.DuckDBPyConnection:
    path.parent.mkdir(parents=True, exist_ok=True)
    con = duckdb.connect(str(path))
    con.execute(DDL)
    return con

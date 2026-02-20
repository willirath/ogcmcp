"""Indexing pipeline: walk MITgcm source, extract, write to DuckDB."""

import subprocess
from datetime import datetime, timezone
from pathlib import Path

from .extract import extract_file
from .schema import connect

MITGCM_ROOT = Path("MITgcm")
SOURCE_DIRS = [
    MITGCM_ROOT / "model" / "src",
    MITGCM_ROOT / "pkg",
    MITGCM_ROOT / "eesupp" / "src",
]


def mitgcm_sha() -> str:
    result = subprocess.run(
        ["git", "-C", str(MITGCM_ROOT), "rev-parse", "HEAD"],
        capture_output=True, text=True,
    )
    return result.stdout.strip()


def source_files() -> list[Path]:
    files = []
    for d in SOURCE_DIRS:
        files.extend(d.rglob("*.F"))
        files.extend(d.rglob("*.F90"))
    return sorted(files)


def run(db_path: Path | None = None) -> None:
    con = connect(db_path) if db_path else connect()

    # Record MITgcm version
    sha = mitgcm_sha()
    con.execute("INSERT OR REPLACE INTO metadata VALUES (?, ?)", ["mitgcm_commit_sha", sha])
    con.execute("INSERT OR REPLACE INTO metadata VALUES (?, ?)", ["indexed_at", datetime.now(timezone.utc).isoformat()])
    print(f"Indexing MITgcm @ {sha[:12]}")

    files = source_files()
    print(f"Found {len(files)} source files")

    sub_id = 1
    for path in files:
        records = extract_file(path)
        for rec in records:
            con.execute(
                "INSERT INTO subroutines VALUES (?, ?, ?, ?, ?, ?, ?)",
                [sub_id, rec.name, rec.file, rec.package,
                 rec.line_start, rec.line_end, rec.source_text],
            )
            for callee in rec.calls:
                con.execute("INSERT INTO calls VALUES (?, ?)", [sub_id, callee])
            for param, group in rec.namelist_params:
                con.execute("INSERT INTO namelist_refs VALUES (?, ?, ?)", [param, sub_id, group])
            for field_name, array_name in rec.diag_fills:
                con.execute("INSERT INTO diagnostics_fills VALUES (?, ?, ?)", [field_name, sub_id, array_name])
            for flag in rec.cpp_guards:
                con.execute("INSERT INTO cpp_guards VALUES (?, ?)", [sub_id, flag])
            sub_id += 1

        if records:
            print(f"  {path.relative_to(MITGCM_ROOT)}: {len(records)} subroutine(s)")

    con.close()
    print(f"\nDone. Indexed {sub_id - 1} subroutines.")


if __name__ == "__main__":
    run()

"""Indexing pipeline: walk FESOM2 source, extract, write to DuckDB."""

import subprocess
from datetime import datetime, timezone
from pathlib import Path

from .extract import extract_file
from .schema import connect

FESOM2_ROOT = Path("FESOM2")

# Directories to recurse for .F90 files — all contain FESOM2 driver code
_F90_DIRS = [
    FESOM2_ROOT / "src",
    FESOM2_ROOT / "src" / "cvmix_driver",
    FESOM2_ROOT / "src" / "icepack_drivers",
    FESOM2_ROOT / "src" / "ifs_interface",
]

# pFUnit test files
_PF_DIRS = [
    FESOM2_ROOT / "test" / "fortran",
    FESOM2_ROOT / "test" / "fortran_parallel",
]

# int_recom: only top-level driver files, not the recom/ library subdir
_INT_RECOM_DIR = FESOM2_ROOT / "src" / "int_recom"

# async_threads_cpp is C++ — skip entirely


def fesom2_sha() -> str:
    result = subprocess.run(
        ["git", "-C", str(FESOM2_ROOT), "rev-parse", "HEAD"],
        capture_output=True, text=True,
    )
    return result.stdout.strip()


def source_files() -> list[Path]:
    files: list[Path] = []

    for d in _F90_DIRS:
        # Don't recurse into subdirs that are already listed separately
        # (cvmix_driver, icepack_drivers, ifs_interface are listed explicitly)
        if d == FESOM2_ROOT / "src":
            files.extend(p for p in d.glob("*.F90") if p.is_file())
            files.extend(p for p in d.glob("*.pf") if p.is_file())
        else:
            files.extend(d.rglob("*.F90"))

    # int_recom: top-level driver files only (recom_*.F90), not the recom/ subdir
    files.extend(_INT_RECOM_DIR.glob("recom_*.F90"))

    # pFUnit tests
    for d in _PF_DIRS:
        files.extend(d.rglob("*.pf"))

    return sorted(set(files))


def run(db_path: Path | None = None) -> None:
    con = connect(db_path) if db_path else connect()

    sha = fesom2_sha()
    con.execute("INSERT OR REPLACE INTO metadata VALUES (?, ?)", ["fesom2_commit_sha", sha])
    con.execute("INSERT OR REPLACE INTO metadata VALUES (?, ?)", ["indexed_at", datetime.now(timezone.utc).isoformat()])
    print(f"Indexing FESOM2 @ {sha[:12]}")

    files = source_files()
    print(f"Found {len(files)} source files")

    mod_id = 1
    sub_id = 1
    for path in files:
        mods, subs = extract_file(path)

        for mod in mods:
            con.execute(
                "INSERT INTO modules VALUES (?, ?, ?, ?, ?)",
                [mod_id, mod.name, mod.file, mod.start_line, mod.end_line],
            )
            for used in mod.uses:
                con.execute(
                    "INSERT INTO uses VALUES (?, ?)",
                    [mod.name, used],
                )
            mod_id += 1

        for sub in subs:
            con.execute(
                "INSERT INTO subroutines VALUES (?, ?, ?, ?, ?, ?, ?)",
                [sub_id, sub.name, sub.module_name, sub.file,
                 sub.start_line, sub.end_line, sub.source_text],
            )
            for callee in sub.calls:
                con.execute(
                    "INSERT INTO calls VALUES (?, ?, ?)",
                    [sub.name, sub.module_name, callee],
                )
            sub_id += 1

        if mods or subs:
            rel = path.relative_to(FESOM2_ROOT)
            print(f"  {rel}: {len(mods)} module(s), {len(subs)} subroutine(s)")

    con.close()
    print(f"\nDone. Indexed {mod_id - 1} modules, {sub_id - 1} subroutines.")


if __name__ == "__main__":
    run()

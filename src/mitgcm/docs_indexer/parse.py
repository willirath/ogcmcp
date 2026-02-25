"""MITgcm-specific header file indexing for embedding.

iter_sections (generic RST parsing) lives in src/rst_parser.py.
"""

import itertools
from pathlib import Path


def iter_headers(mitgcm_root: Path) -> list[dict]:
    """Return one dict per .h file from verification experiments and core headers.

    Covers three locations under mitgcm_root:
      - verification/*/code/*.h  — experiment-level header overrides
      - model/inc/*.h            — core model headers (PARAMS.h, DYNVARS.h, …)
      - eesupp/inc/*.h           — execution environment headers (EXCH.h, …)

    Each dict has keys:
        file    – path relative to mitgcm_root (str, forward slashes)
        section – filename (e.g. "PARAMS.h")
        text    – raw file content

    Empty files are skipped.
    """
    results = []
    globs = [
        mitgcm_root.glob("verification/*/code/*.h"),
        mitgcm_root.glob("model/inc/*.h"),
        mitgcm_root.glob("eesupp/inc/*.h"),
        mitgcm_root.glob("pkg/*/*.h"),
    ]
    for h_path in sorted(itertools.chain(*globs)):
        text = h_path.read_text(encoding="utf-8", errors="replace")
        if not text.strip():
            continue
        results.append({
            "file": h_path.relative_to(mitgcm_root).as_posix(),
            "section": h_path.name,
            "text": text,
        })
    return results

"""Plain Python callables over the DuckDB code graph and ChromaDB semantic index."""

import re
from contextlib import contextmanager
from pathlib import Path

from src.mitgcm.indexer.schema import DB_PATH, connect
from src.mitgcm.embedder.store import (
    CHROMA_PATH,
    COLLECTION_NAME,
    DOCS_COLLECTION_NAME,
    VERIFICATION_COLLECTION_NAME,
    get_collection,
)


_HARDWARE_PLATFORM_FLAGS = frozenset({
    "TARGET_NEC_SX",
    "TARGET_SGI",
    "TARGET_CRAY_VECTOR",
    "TARGET_T3E",
    "TARGET_ALPHA",
})


def _doc_snippet(doc: str) -> str:
    """Extract a readable snippet from a stored ChromaDB document chunk.

    Strips the leading ``[file] section`` header line and any leading
    Fortran C-comment lines (lines whose first non-whitespace character is
    ``C`` or ``c`` followed by a space or end-of-line).  This ensures that
    .h header files return their first PARAMETER/COMMON declaration rather
    than the opening comment block.
    """
    # Strip "[file] section\n" or "[file]\n" header
    if doc.startswith("["):
        nl = doc.find("\n")
        if nl >= 0:
            doc = doc[nl + 1:]
    # Skip leading Fortran C-comment lines (column-1 'C' or 'c')
    lines = doc.splitlines(keepends=True)
    start = 0
    for i, line in enumerate(lines):
        stripped = line.lstrip()
        if stripped and not (stripped[0] in "Cc" and (len(stripped) == 1 or stripped[1] in " \t\n\r")):
            start = i
            break
    return ("".join(lines[start:]))[:400]


def _normalize_query(query: str) -> str:
    """Split camelCase and snake_case identifiers into words before embedding.

    Embedding models handle natural language well but treat camelCase compound
    identifiers as single unknown tokens, producing poor query vectors.
    Examples:
        "zonalWindFile"          -> "zonal Wind File"
        "tauX direction convention" -> "tau X direction convention"
        "nonHydrostatic"         -> "non Hydrostatic"
        "ALLOW_NONHYDROSTATIC"   -> "ALLOW NONHYDROSTATIC"
    """
    # Split camelCase (e.g. zonalWindFile -> zonal Wind File)
    result = re.sub(r"([a-z])([A-Z])", r"\1 \2", query)
    # Replace underscores with spaces
    result = result.replace("_", " ")
    return result


@contextmanager
def _db(db_path: Path):
    """Context manager that opens a DuckDB connection and ensures it is closed."""
    con = connect(db_path)
    try:
        yield con
    finally:
        con.close()


def _embed(query: str) -> list[float]:
    """Embed a query string using the nomic-embed-text model via Ollama."""
    import ollama
    response = ollama.embed(model="nomic-embed-text", input=_normalize_query(query))
    return response.embeddings[0]


def search_code(query: str, top_k: int = 5, _db_path: Path = DB_PATH, _chroma_path: Path = CHROMA_PATH) -> list[dict]:
    """Semantic search over subroutine embeddings; returns top_k subroutines with DuckDB metadata."""
    collection = get_collection(COLLECTION_NAME, _chroma_path)
    embedding = _embed(query)

    results = collection.query(
        query_embeddings=[embedding],
        n_results=top_k * 10,
        include=["metadatas", "distances"],
    )

    # Deduplicate: keep best (lowest distance) chunk per db_id
    best: dict[int, tuple[float, dict]] = {}
    for meta, dist in zip(results["metadatas"][0], results["distances"][0]):
        db_id = int(meta["db_id"])
        if db_id not in best or dist < best[db_id][0]:
            best[db_id] = (dist, meta)

    # Sort by distance, take top_k
    ranked = sorted(best.values(), key=lambda x: x[0])[:top_k]
    db_ids = [int(m["db_id"]) for _, m in ranked]

    if not db_ids:
        return []

    with _db(_db_path) as con:
        placeholders = ", ".join("?" for _ in db_ids)
        rows = con.execute(
            f"SELECT id, name, file, package, line_start, line_end FROM subroutines WHERE id IN ({placeholders})",
            db_ids,
        ).fetchall()

    id_to_row = {r[0]: r for r in rows}
    out = []
    for _, meta in ranked:
        db_id = int(meta["db_id"])
        if db_id not in id_to_row:
            continue
        r = id_to_row[db_id]
        out.append({"id": r[0], "name": r[1], "file": r[2], "package": r[3], "line_start": r[4], "line_end": r[5]})
    return out


def find_subroutines(name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return all subroutines matching name across all packages (case-insensitive).

    Returns an empty list if none found. Does not include source_text.
    Use this when a name may exist in multiple packages and you need to discover
    which packages contain it before calling get_subroutine or get_source_tool
    with package=.
    """
    with _db(_db_path) as con:
        rows = con.execute(
            "SELECT id, name, file, package, line_start, line_end FROM subroutines WHERE upper(name) = upper(?)",
            [name],
        ).fetchall()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "line_start": r[4], "line_end": r[5]} for r in rows]


def get_subroutine(name: str, package: str | None = None, _db_path: Path = DB_PATH) -> dict | None:
    """Return subroutine metadata and source text, or None if not found.

    When package is provided, restricts the lookup to that package.  When
    package is None and exactly one subroutine matches, returns it.  When
    package is None and multiple subroutines share the same name, raises
    ValueError listing the packages; call find_subroutines() first to discover
    which packages contain the name.
    """
    with _db(_db_path) as con:
        if package is not None:
            rows = con.execute(
                "SELECT id, name, file, package, line_start, line_end, source_text FROM subroutines WHERE upper(name) = upper(?) AND upper(package) = upper(?)",
                [name, package],
            ).fetchall()
        else:
            rows = con.execute(
                "SELECT id, name, file, package, line_start, line_end, source_text FROM subroutines WHERE upper(name) = upper(?)",
                [name],
            ).fetchall()

    if not rows:
        return None
    if len(rows) > 1:
        packages = [r[3] for r in rows]
        raise ValueError(
            f"get_subroutine: {len(rows)} subroutines named {name!r} found in packages {packages}; "
            "pass package= to disambiguate, or use find_subroutines() to list all copies"
        )
    row = rows[0]
    return {"id": row[0], "name": row[1], "file": row[2], "package": row[3], "line_start": row[4], "line_end": row[5], "source_text": row[6]}


def get_callers(name: str, package: str | None = None, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines that call the named subroutine.

    When package is provided, restricts the lookup to callers that belong to
    that package (i.e. subroutines within the package that call the named
    subroutine).
    """
    with _db(_db_path) as con:
        if package is not None:
            rows = con.execute(
                """
                SELECT s.id, s.name, s.file, s.package, s.line_start, s.line_end
                FROM subroutines s
                JOIN calls c ON c.caller_id = s.id
                WHERE upper(c.callee_name) = upper(?)
                  AND upper(s.package) = upper(?)
                """,
                [name, package],
            ).fetchall()
        else:
            rows = con.execute(
                """
                SELECT s.id, s.name, s.file, s.package, s.line_start, s.line_end
                FROM subroutines s
                JOIN calls c ON c.caller_id = s.id
                WHERE upper(c.callee_name) = upper(?)
                """,
                [name],
            ).fetchall()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "line_start": r[4], "line_end": r[5]} for r in rows]


def get_callees(name: str, package: str | None = None, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines called by the named subroutine.

    When package is provided, restricts the lookup to the copy of the
    subroutine in that package, returning only its callees.
    """
    with _db(_db_path) as con:
        if package is not None:
            rows = con.execute(
                """
                SELECT DISTINCT c.callee_name
                FROM calls c
                JOIN subroutines s ON s.id = c.caller_id
                WHERE upper(s.name) = upper(?)
                  AND upper(s.package) = upper(?)
                """,
                [name, package],
            ).fetchall()
        else:
            rows = con.execute(
                """
                SELECT DISTINCT c.callee_name
                FROM calls c
                JOIN subroutines s ON s.id = c.caller_id
                WHERE upper(s.name) = upper(?)
                """,
                [name],
            ).fetchall()

    return [{"callee_name": r[0]} for r in rows]


def namelist_to_code(param: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines that reference a namelist parameter."""
    with _db(_db_path) as con:
        rows = con.execute(
            """
            SELECT s.id, s.name, s.file, s.package, nr.namelist_group
            FROM namelist_refs nr
            JOIN subroutines s ON s.id = nr.subroutine_id
            WHERE upper(nr.param_name) = upper(?)
            """,
            [param],
        ).fetchall()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "namelist_group": r[4]} for r in rows]


def diagnostics_fill_to_source(field_name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines that fill a diagnostics field (trims trailing spaces before comparing)."""
    with _db(_db_path) as con:
        rows = con.execute(
            """
            SELECT s.id, s.name, s.file, s.package, df.array_name
            FROM diagnostics_fills df
            JOIN subroutines s ON s.id = df.subroutine_id
            WHERE upper(trim(df.field_name)) = upper(trim(?))
            """,
            [field_name],
        ).fetchall()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "array_name": r[4]} for r in rows]


def get_cpp_requirements(subroutine_name: str, _db_path: Path = DB_PATH) -> list[str]:
    """Return CPP flags that guard a subroutine.

    Known hardware-platform flags (TARGET_NEC_SX, TARGET_SGI,
    TARGET_CRAY_VECTOR, etc.) are excluded — they guard vendor-specific
    optimisations irrelevant to modern builds.
    """
    with _db(_db_path) as con:
        rows = con.execute(
            """
            SELECT cg.cpp_flag
            FROM cpp_guards cg
            JOIN subroutines s ON s.id = cg.subroutine_id
            WHERE upper(s.name) = upper(?)
            """,
            [subroutine_name],
        ).fetchall()

    return [r[0] for r in rows if r[0] not in _HARDWARE_PLATFORM_FLAGS]


def get_package_flags(package_name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return CPP flags defined by a package."""
    with _db(_db_path) as con:
        rows = con.execute(
            "SELECT cpp_flag, description FROM package_options WHERE upper(package_name) = upper(?)",
            [package_name],
        ).fetchall()

    return [{"cpp_flag": r[0], "description": r[1]} for r in rows]


def find_packages(_db_path: Path = DB_PATH) -> list[dict]:
    """Return all packages in the index with subroutine counts.

    Returns a list of dicts with keys: package, subroutine_count.
    Sorted alphabetically by package name.
    """
    with _db(_db_path) as con:
        rows = con.execute(
            "SELECT package, COUNT(*) as n FROM subroutines GROUP BY package ORDER BY package",
        ).fetchall()

    return [{"package": r[0], "subroutine_count": r[1]} for r in rows]


def get_package(package_name: str, _db_path: Path = DB_PATH) -> dict | None:
    """Return metadata for a MITgcm package including its subroutines.

    Returns None if the package is not found.  Includes: package name,
    source files, subroutine list (name, file, line_start, line_end),
    and CPP flags defined by the package.
    """
    with _db(_db_path) as con:
        rows = con.execute(
            "SELECT id, name, file, line_start, line_end FROM subroutines "
            "WHERE upper(package) = upper(?) ORDER BY file, line_start",
            [package_name],
        ).fetchall()

        if not rows:
            return None

        flags = con.execute(
            "SELECT cpp_flag, description FROM package_options WHERE upper(package_name) = upper(?)",
            [package_name],
        ).fetchall()

    subroutines = [{"name": r[1], "file": r[2], "line_start": r[3], "line_end": r[4]} for r in rows]
    files = sorted({r[2] for r in rows})

    return {
        "package": rows[0][2].split("/")[0] if "/" in rows[0][2] else package_name,
        "name": package_name,
        "files": files,
        "subroutine_count": len(subroutines),
        "subroutines": subroutines,
        "cpp_flags": [{"cpp_flag": r[0], "description": r[1]} for r in flags],
    }


def get_doc_source(
    file: str,
    section: str,
    offset: int = 0,
    limit: int = 200,
    _chroma_path: Path = CHROMA_PATH,
) -> dict | None:
    """Return paginated text of a documentation section or header file.

    Retrieves all stored chunks for (file, section), reassembles them in
    chunk order (respecting overlap), and returns paginated lines.

    Use search_docs to discover file and section values.
    Returns {file, section, total_lines, offset, lines} or None if not found.
    """
    from src.embed_utils import OVERLAP

    collection = get_collection(DOCS_COLLECTION_NAME, _chroma_path)
    results = collection.get(
        where={"$and": [{"file": {"$eq": file}}, {"section": {"$eq": section}}]},
        include=["metadatas", "documents"],
    )

    if not results["ids"]:
        return None

    chunks = sorted(
        zip(results["metadatas"], results["documents"]),
        key=lambda x: x[0]["chunk_index"],
    )

    # Strip the prepended header ("[file] section\n" or "[file]\n") from each chunk.
    header = f"[{file}] {section}\n" if section else f"[{file}]\n"
    header_len = len(header)
    raw_chunks = [doc[header_len:] for _, doc in chunks]

    # Reassemble: chunk 0 is taken in full; each subsequent chunk overlaps
    # the previous by OVERLAP chars, so skip those chars when concatenating.
    text = raw_chunks[0]
    for raw in raw_chunks[1:]:
        text += raw[OVERLAP:]

    all_lines = text.splitlines()
    total = len(all_lines)
    return {
        "file": file,
        "section": section,
        "total_lines": total,
        "offset": offset,
        "lines": all_lines[offset: offset + limit],
    }


def get_verification_source(
    file: str,
    offset: int = 0,
    limit: int = 200,
    _chroma_path: Path = CHROMA_PATH,
) -> dict | None:
    """Return paginated full text of a verification experiment file.

    Retrieves all stored chunks for the given file path, reassembles them in
    chunk order (respecting overlap), and returns paginated lines.

    Use search_verification to discover file paths.
    Returns {file, total_lines, offset, lines} or None if not found.
    """
    from src.embed_utils import OVERLAP

    collection = get_collection(VERIFICATION_COLLECTION_NAME, _chroma_path)
    results = collection.get(
        where={"file": {"$eq": file}},
        include=["metadatas", "documents"],
    )

    if not results["ids"]:
        return None

    chunks = sorted(
        zip(results["metadatas"], results["documents"]),
        key=lambda x: x[0]["chunk_index"],
    )

    # Strip the prepended "[file]\n" header from each chunk
    header = f"[{file}]\n"
    header_len = len(header)
    raw_chunks = [doc[header_len:] for _, doc in chunks]

    # Reassemble: chunk 0 in full; each subsequent chunk skips the OVERLAP prefix
    text = raw_chunks[0]
    for raw in raw_chunks[1:]:
        text += raw[OVERLAP:]

    all_lines = text.splitlines()
    total = len(all_lines)
    return {
        "file": file,
        "total_lines": total,
        "offset": offset,
        "lines": all_lines[offset: offset + limit],
    }


_CATALOGUE_PATH = Path("data/mitgcm/verification_catalogue.json")


def list_verification_experiments() -> list[dict]:
    """Return structured catalogue of all MITgcm verification/tutorial experiments.

    Loads from the pre-built JSON catalogue at data/mitgcm/verification_catalogue.json
    (written by pixi run mitgcm-embed-verification). Falls back to building live
    from MITgcm/verification/ when the JSON is absent (development environments).

    Each entry has: name, tutorial, packages, domain_class, Nx, Ny, Nr,
    grid_type, nonhydrostatic, free_surface, eos_type.
    """
    import json
    if _CATALOGUE_PATH.exists():
        return json.loads(_CATALOGUE_PATH.read_text())
    from src.mitgcm.verification_indexer.catalogue import build_catalogue
    return build_catalogue()


def search_verification(query: str, top_k: int = 5, _chroma_path: Path = CHROMA_PATH) -> list[dict]:
    """Semantic search over MITgcm verification experiment configuration files.

    Searches input/data*, eedata, code/*.h, and packages.conf from all
    verification experiments.  Returns up to top_k results, deduplicated
    per (experiment, filename).

    Each result has: experiment, file, filename, snippet (first 400 chars
    of content after stripping the header and leading Fortran C-comments).
    Requires Ollama and a populated mitgcm_verification ChromaDB collection
    (pixi run embed-verification).
    """
    collection = get_collection(VERIFICATION_COLLECTION_NAME, _chroma_path)
    embedding = _embed(query)

    results = collection.query(
        query_embeddings=[embedding],
        n_results=top_k * 5,
        include=["metadatas", "distances", "documents"],
    )

    # Deduplicate: keep best chunk per (experiment, filename)
    best: dict[tuple[str, str], tuple[float, dict, str]] = {}
    for meta, dist, doc in zip(
        results["metadatas"][0], results["distances"][0], results["documents"][0]
    ):
        key = (meta["experiment"], meta["filename"])
        if key not in best or dist < best[key][0]:
            best[key] = (dist, meta, doc)

    ranked = sorted(best.values(), key=lambda x: x[0])[:top_k]
    return [
        {
            "experiment": meta["experiment"],
            "file": meta["file"],
            "filename": meta["filename"],
            "snippet": _doc_snippet(doc),
        }
        for _, meta, doc in ranked
    ]


def search_docs(query: str, top_k: int = 5, _chroma_path: Path = CHROMA_PATH) -> list[dict]:
    """Semantic search over MITgcm documentation sections.

    Returns up to top_k doc sections whose text most closely matches the
    natural-language query.  Requires a running Ollama server and a populated
    mitgcm_docs ChromaDB collection (pixi run embed-docs).

    Each result has keys: file, section, snippet (first 400 chars of content
    after stripping the header and leading Fortran C-comments).
    """
    collection = get_collection(DOCS_COLLECTION_NAME, _chroma_path)
    embedding = _embed(query)

    results = collection.query(
        query_embeddings=[embedding],
        n_results=top_k * 5,
        include=["metadatas", "distances", "documents"],
    )

    # Deduplicate: keep best (lowest distance) chunk per (file, section)
    best: dict[tuple[str, str], tuple[float, dict, str]] = {}
    for meta, dist, doc in zip(
        results["metadatas"][0], results["distances"][0], results["documents"][0]
    ):
        key = (meta["file"], meta["section"])
        if key not in best or dist < best[key][0]:
            best[key] = (dist, meta, doc)

    ranked = sorted(best.values(), key=lambda x: x[0])[:top_k]
    return [
        {
            "file": meta["file"],
            "section": meta["section"],
            "snippet": _doc_snippet(doc),
        }
        for _, meta, doc in ranked
    ]

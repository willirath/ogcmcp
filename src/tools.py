"""Plain Python callables over the DuckDB code graph and ChromaDB semantic index."""

from pathlib import Path

from src.indexer.schema import DB_PATH, connect
from src.embedder.store import CHROMA_PATH, get_docs_collection


def search_code(query: str, top_k: int = 5, _db_path: Path = DB_PATH, _chroma_path: Path = CHROMA_PATH) -> list[dict]:
    """Semantic search over subroutine embeddings; returns top_k subroutines with DuckDB metadata."""
    import ollama
    from src.embedder.store import get_collection

    collection = get_collection(_chroma_path)
    response = ollama.embed(model="nomic-embed-text", input=query)
    embedding = response.embeddings[0]

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

    con = connect(_db_path)
    try:
        placeholders = ", ".join("?" for _ in db_ids)
        rows = con.execute(
            f"SELECT id, name, file, package, line_start, line_end FROM subroutines WHERE id IN ({placeholders})",
            db_ids,
        ).fetchall()
    finally:
        con.close()

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
    con = connect(_db_path)
    try:
        rows = con.execute(
            "SELECT id, name, file, package, line_start, line_end FROM subroutines WHERE upper(name) = upper(?)",
            [name],
        ).fetchall()
    finally:
        con.close()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "line_start": r[4], "line_end": r[5]} for r in rows]


def get_subroutine(name: str, package: str | None = None, _db_path: Path = DB_PATH) -> dict | None:
    """Return subroutine metadata and source text, or None if not found.

    When package is provided, restricts the lookup to that package.  When
    package is None and exactly one subroutine matches, returns it.  When
    package is None and multiple subroutines share the same name, raises
    ValueError listing the packages; call find_subroutines() first to discover
    which packages contain the name.
    """
    con = connect(_db_path)
    try:
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
    finally:
        con.close()

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
    con = connect(_db_path)
    try:
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
    finally:
        con.close()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "line_start": r[4], "line_end": r[5]} for r in rows]


def get_callees(name: str, package: str | None = None, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines called by the named subroutine.

    When package is provided, restricts the lookup to the copy of the
    subroutine in that package, returning only its callees.
    """
    con = connect(_db_path)
    try:
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
    finally:
        con.close()

    return [{"callee_name": r[0]} for r in rows]


def namelist_to_code(param: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines that reference a namelist parameter."""
    con = connect(_db_path)
    try:
        rows = con.execute(
            """
            SELECT s.id, s.name, s.file, s.package, nr.namelist_group
            FROM namelist_refs nr
            JOIN subroutines s ON s.id = nr.subroutine_id
            WHERE upper(nr.param_name) = upper(?)
            """,
            [param],
        ).fetchall()
    finally:
        con.close()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "namelist_group": r[4]} for r in rows]


def diagnostics_fill_to_source(field_name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines that fill a diagnostics field (trims trailing spaces before comparing)."""
    con = connect(_db_path)
    try:
        rows = con.execute(
            """
            SELECT s.id, s.name, s.file, s.package, df.array_name
            FROM diagnostics_fills df
            JOIN subroutines s ON s.id = df.subroutine_id
            WHERE upper(trim(df.field_name)) = upper(trim(?))
            """,
            [field_name],
        ).fetchall()
    finally:
        con.close()

    return [{"id": r[0], "name": r[1], "file": r[2], "package": r[3], "array_name": r[4]} for r in rows]


def get_cpp_requirements(subroutine_name: str, _db_path: Path = DB_PATH) -> list[str]:
    """Return CPP flags that guard a subroutine."""
    con = connect(_db_path)
    try:
        rows = con.execute(
            """
            SELECT cg.cpp_flag
            FROM cpp_guards cg
            JOIN subroutines s ON s.id = cg.subroutine_id
            WHERE upper(s.name) = upper(?)
            """,
            [subroutine_name],
        ).fetchall()
    finally:
        con.close()

    return [r[0] for r in rows]


def get_package_flags(package_name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return CPP flags defined by a package."""
    con = connect(_db_path)
    try:
        rows = con.execute(
            "SELECT cpp_flag, description FROM package_options WHERE upper(package_name) = upper(?)",
            [package_name],
        ).fetchall()
    finally:
        con.close()

    return [{"cpp_flag": r[0], "description": r[1]} for r in rows]


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
    from .embedder.pipeline import OVERLAP

    collection = get_docs_collection(_chroma_path)
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


def search_docs(query: str, top_k: int = 5, _chroma_path: Path = CHROMA_PATH) -> list[dict]:
    """Semantic search over MITgcm documentation sections.

    Returns up to top_k doc sections whose text most closely matches the
    natural-language query.  Requires a running Ollama server and a populated
    mitgcm_docs ChromaDB collection (pixi run embed-docs).

    Each result has keys: file, section, snippet (first 400 chars of text).
    """
    import ollama

    collection = get_docs_collection(_chroma_path)
    response = ollama.embed(model="nomic-embed-text", input=query)
    embedding = response.embeddings[0]

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
            "snippet": doc[:400],
        }
        for _, meta, doc in ranked
    ]

"""Plain Python callables over the FESOM2 DuckDB code graph and ChromaDB semantic index."""

import re
from contextlib import contextmanager
from pathlib import Path

from src.fesom2.indexer.schema import DB_PATH, connect
from src.fesom2.embedder.store import (
    CHROMA_PATH,
    FESOM2_SUBROUTINES_COLLECTION,
    FESOM2_DOCS_COLLECTION,
    FESOM2_NAMELISTS_COLLECTION,
    get_collection,
)


# ── Helpers ───────────────────────────────────────────────────────────────────


def _normalize_query(query: str) -> str:
    """Split camelCase and snake_case identifiers into words for embedding."""
    result = re.sub(r"([a-z])([A-Z])", r"\1 \2", query)
    return result.replace("_", " ")


@contextmanager
def _db(db_path: Path = DB_PATH):
    con = connect(db_path)
    try:
        yield con
    finally:
        con.close()


def _embed(query: str) -> list[float]:
    import ollama
    response = ollama.embed(model="nomic-embed-text", input=_normalize_query(query))
    return response.embeddings[0]


def _doc_snippet(doc: str) -> str:
    """Return first 400 chars of a ChromaDB document, stripping header lines."""
    if doc.startswith("["):
        nl = doc.find("\n")
        if nl >= 0:
            doc = doc[nl + 1:]
    return doc[:400]


# ── Code navigation ───────────────────────────────────────────────────────────


def search_code(
    query: str,
    top_k: int = 5,
    _db_path: Path = DB_PATH,
    _chroma_path: Path = CHROMA_PATH,
) -> list[dict]:
    """Semantic search over FESOM2 subroutine embeddings."""
    collection = get_collection(FESOM2_SUBROUTINES_COLLECTION, _chroma_path)
    embedding = _embed(query)

    results = collection.query(
        query_embeddings=[embedding],
        n_results=top_k * 10,
        include=["metadatas", "distances"],
    )

    best: dict[int, tuple[float, dict]] = {}
    for meta, dist in zip(results["metadatas"][0], results["distances"][0]):
        db_id = int(meta["db_id"])
        if db_id not in best or dist < best[db_id][0]:
            best[db_id] = (dist, meta)

    ranked = sorted(best.values(), key=lambda x: x[0])[:top_k]
    db_ids = [int(m["db_id"]) for _, m in ranked]

    if not db_ids:
        return []

    with _db(_db_path) as con:
        placeholders = ", ".join("?" for _ in db_ids)
        rows = con.execute(
            f"SELECT id, name, module_name, file, start_line, end_line "
            f"FROM subroutines WHERE id IN ({placeholders})",
            db_ids,
        ).fetchall()

    id_to_row = {r[0]: r for r in rows}
    return [
        {
            "id": r[0],
            "name": r[1],
            "module_name": r[2],
            "file": r[3],
            "start_line": r[4],
            "end_line": r[5],
        }
        for _, meta in ranked
        if (r := id_to_row.get(int(meta["db_id"]))) is not None
    ]


def find_modules(name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return all modules matching name (case-insensitive)."""
    with _db(_db_path) as con:
        rows = con.execute(
            "SELECT id, name, file, start_line, end_line FROM modules "
            "WHERE upper(name) = upper(?)",
            [name],
        ).fetchall()
    return [
        {"id": r[0], "name": r[1], "file": r[2], "start_line": r[3], "end_line": r[4]}
        for r in rows
    ]


def get_module(name: str, _db_path: Path = DB_PATH) -> dict | None:
    """Return metadata for a module including its contained subroutines."""
    with _db(_db_path) as con:
        mrows = con.execute(
            "SELECT id, name, file, start_line, end_line FROM modules "
            "WHERE upper(name) = upper(?)",
            [name],
        ).fetchall()
        if not mrows:
            return None
        mod = mrows[0]
        srows = con.execute(
            "SELECT name, start_line, end_line FROM subroutines "
            "WHERE upper(module_name) = upper(?) ORDER BY start_line",
            [name],
        ).fetchall()
    return {
        "id": mod[0],
        "name": mod[1],
        "file": mod[2],
        "start_line": mod[3],
        "end_line": mod[4],
        "subroutines": [
            {"name": r[0], "start_line": r[1], "end_line": r[2]} for r in srows
        ],
    }


def find_subroutines(name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return all subroutines matching name (case-insensitive)."""
    with _db(_db_path) as con:
        rows = con.execute(
            "SELECT id, name, module_name, file, start_line, end_line FROM subroutines "
            "WHERE upper(name) = upper(?)",
            [name],
        ).fetchall()
    return [
        {"id": r[0], "name": r[1], "module_name": r[2], "file": r[3],
         "start_line": r[4], "end_line": r[5]}
        for r in rows
    ]


def get_subroutine(
    name: str, module: str | None = None, _db_path: Path = DB_PATH
) -> dict | None:
    """Return subroutine metadata and source text, or None if not found.

    When module is provided, restricts to that module. When None and multiple
    subroutines share the same name, raises ValueError.
    """
    with _db(_db_path) as con:
        if module is not None:
            rows = con.execute(
                "SELECT id, name, module_name, file, start_line, end_line, source_text "
                "FROM subroutines WHERE upper(name) = upper(?) AND upper(module_name) = upper(?)",
                [name, module],
            ).fetchall()
        else:
            rows = con.execute(
                "SELECT id, name, module_name, file, start_line, end_line, source_text "
                "FROM subroutines WHERE upper(name) = upper(?)",
                [name],
            ).fetchall()

    if not rows:
        return None
    if len(rows) > 1:
        modules = [r[2] for r in rows]
        raise ValueError(
            f"get_subroutine: {len(rows)} subroutines named {name!r} found in "
            f"modules {modules}; pass module= to disambiguate"
        )
    row = rows[0]
    return {
        "id": row[0], "name": row[1], "module_name": row[2], "file": row[3],
        "start_line": row[4], "end_line": row[5], "source_text": row[6],
    }


def get_callers(name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines that call the named subroutine."""
    with _db(_db_path) as con:
        rows = con.execute(
            """
            SELECT DISTINCT c.caller_name, c.caller_module
            FROM calls c
            WHERE upper(c.callee_name) = upper(?)
            """,
            [name],
        ).fetchall()
    return [{"caller_name": r[0], "caller_module": r[1]} for r in rows]


def get_callees(name: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return subroutines called by the named subroutine."""
    with _db(_db_path) as con:
        rows = con.execute(
            """
            SELECT DISTINCT c.callee_name
            FROM calls c
            WHERE upper(c.caller_name) = upper(?)
            """,
            [name],
        ).fetchall()
    return [{"callee_name": r[0]} for r in rows]


def get_module_uses(module_name: str, _db_path: Path = DB_PATH) -> list[str]:
    """Return modules USEd by the named module."""
    with _db(_db_path) as con:
        rows = con.execute(
            "SELECT used_module FROM uses WHERE upper(module_name) = upper(?)",
            [module_name],
        ).fetchall()
    return [r[0] for r in rows]


def namelist_to_code(param: str, _db_path: Path = DB_PATH) -> list[dict]:
    """Return modules that declare a namelist parameter, with description if available."""
    with _db(_db_path) as con:
        refs = con.execute(
            """
            SELECT DISTINCT param_name, namelist_group, file, module_name, line
            FROM namelist_refs
            WHERE upper(param_name) = upper(?)
            ORDER BY module_name
            """,
            [param],
        ).fetchall()
        descs = con.execute(
            """
            SELECT description, namelist_group, config_file
            FROM namelist_descriptions
            WHERE upper(param_name) = upper(?)
            LIMIT 1
            """,
            [param],
        ).fetchall()

    description = descs[0][0] if descs else None
    namelist_group = descs[0][1] if descs else None
    config_file = descs[0][2] if descs else None

    return [
        {
            "param_name": r[0],
            "namelist_group": r[1],
            "file": r[2],
            "module_name": r[3],
            "line": r[4],
            "description": description,
            "config_file": config_file,
        }
        for r in refs
    ]


# ── Documentation search ──────────────────────────────────────────────────────


def search_docs(
    query: str, top_k: int = 5, _chroma_path: Path = CHROMA_PATH
) -> list[dict]:
    """Semantic search over FESOM2 RST docs and namelist descriptions.

    Searches both the ``fesom2_docs`` and ``fesom2_namelists`` collections
    and returns the ``top_k`` best matches across both.
    """
    embedding = _embed(query)

    results: list[tuple[float, dict]] = []

    for coll_name, source_type in [
        (FESOM2_DOCS_COLLECTION, "doc"),
        (FESOM2_NAMELISTS_COLLECTION, "namelist"),
    ]:
        try:
            coll = get_collection(coll_name, _chroma_path)
            r = coll.query(
                query_embeddings=[embedding],
                n_results=top_k * 3,
                include=["metadatas", "distances", "documents"],
            )
            for meta, dist, doc in zip(
                r["metadatas"][0], r["distances"][0], r["documents"][0]
            ):
                results.append((dist, {**meta, "_doc": doc, "_source": source_type}))
        except Exception:
            pass

    results.sort(key=lambda x: x[0])
    out = []
    for dist, meta in results[:top_k]:
        record: dict = {"source": meta["_source"]}
        if meta["_source"] == "doc":
            record["file"] = meta.get("file", "")
            record["section"] = meta.get("section", "")
        else:
            record["param_name"] = meta.get("param_name", "")
            record["namelist_group"] = meta.get("namelist_group", "")
            record["config_file"] = meta.get("config_file", "")
        record["snippet"] = _doc_snippet(meta["_doc"])
        out.append(record)
    return out


def get_doc_source(
    file: str,
    section: str,
    offset: int = 0,
    limit: int = 200,
    _chroma_path: Path = CHROMA_PATH,
) -> dict | None:
    """Return paginated text of a FESOM2 documentation section."""
    from src.embed_utils import OVERLAP

    collection = get_collection(FESOM2_DOCS_COLLECTION, _chroma_path)
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

    header = f"[{file}] {section}\n" if section else f"[{file}]\n"
    header_len = len(header)
    raw_chunks = [doc[header_len:] for _, doc in chunks]

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


def list_forcing_datasets() -> list[str]:
    """Return names of all available FESOM2 forcing datasets."""
    from src.fesom2.domain.forcing import list_forcing_datasets as _list
    return _list()


def get_forcing_spec(dataset: str) -> dict | None:
    """Return the full specification for a FESOM2 forcing dataset (case-insensitive)."""
    from src.fesom2.domain.forcing import get_forcing_spec as _get
    return _get(dataset)


def list_setups(_fesom2_root: Path | None = None) -> list[dict]:
    """Return all FESOM2 setup records (reference namelists + CI setups)."""
    from src.fesom2.setups import list_setups as _list_setups

    if _fesom2_root is None:
        _fesom2_root = Path("FESOM2")
    return _list_setups(_fesom2_root)

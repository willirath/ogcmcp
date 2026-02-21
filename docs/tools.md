# Tools

`src/tools.py` exposes eight plain Python callables over the DuckDB code graph
and ChromaDB semantic index built in M1–M2. No server or framework is involved;
functions return plain dicts and lists so they can be easily serialised for the
M4 MCP layer.

## Module layout

```
src/
└── tools.py        eight public functions
```

## Functions

### `search_code(query, top_k=5, _db_path=..., _chroma_path=...)`

Semantic similarity search over subroutine embeddings.

1. Embeds `query` with `nomic-embed-text` via Ollama.
2. Queries ChromaDB for `top_k * 10` nearest chunks.
3. Deduplicates by `db_id`, keeping the chunk with the lowest cosine distance
   per subroutine.
4. Returns the top `top_k` subroutines with DuckDB metadata joined in.

Return shape:

```python
[
    {
        "id": 42,
        "name": "CG3D",
        "file": "model/src/cg3d.F",
        "package": "model",
        "line_start": 1,
        "line_end": 350,
    },
    ...
]
```

### `get_subroutine(name, package=None, _db_path=...)`

Fetch one subroutine by name (case-insensitive). Returns `None` if not found.
Raises `ValueError` if the name matches subroutines in multiple packages and no
`package=` is given; call `find_subroutines()` first to discover which packages
contain the name.

```python
{
    "id": 42,
    "name": "CG3D",
    "file": "model/src/cg3d.F",
    "package": "model",
    "line_start": 1,
    "line_end": 350,
    "source_text": "SUBROUTINE CG3D\n...",
}
```

### `find_subroutines(name, _db_path=...)`

Return all subroutines matching `name` across all packages (case-insensitive).
Returns an empty list if none found. Does not include `source_text`.

Use this when a name may exist in multiple packages and you need to discover
which packages contain it before calling `get_subroutine` or `get_source_tool`
with `package=`.

Return shape:

```python
[
    {"id": 20, "name": "DIC_COEFFS_SURF", "file": "bling/src/...", "package": "bling", "line_start": 1, "line_end": 80},
    {"id": 21, "name": "DIC_COEFFS_SURF", "file": "dic/src/...",   "package": "dic",   "line_start": 1, "line_end": 90},
]
```

### `get_callers(name, package=None, _db_path=...)`

Return all subroutines that call `name` (case-insensitive). Empty list if none.

```python
[{"id": 7, "name": "PRE_CG3D", "file": "...", "package": "model", "line_start": 1, "line_end": 50}]
```

### `get_callees(name, package=None, _db_path=...)`

Return all subroutine names called by `name` (case-insensitive). Empty list if
none.

```python
[{"callee_name": "CG3D"}]
```

### `namelist_to_code(param, _db_path=...)`

Find subroutines that reference a namelist parameter (case-insensitive).

```python
[
    {
        "id": 42,
        "name": "CG3D",
        "file": "model/src/cg3d.F",
        "package": "model",
        "namelist_group": "PARM03",
    }
]
```

### `diagnostics_fill_to_source(field_name, _db_path=...)`

Find subroutines that fill a diagnostics field. Compares after trimming trailing
spaces and folding case — needed because extracted field names sometimes carry
trailing whitespace (e.g. `'WdRHO_P '`).

```python
[
    {
        "id": 42,
        "name": "CG3D",
        "file": "model/src/cg3d.F",
        "package": "model",
        "array_name": "rho3d",
    }
]
```

### `get_cpp_requirements(subroutine_name, _db_path=...)`

Return CPP flags that guard `subroutine_name` (case-insensitive). Empty list if
none.

```python
["ALLOW_NONHYDROST"]
```

### `get_package_flags(package_name, _db_path=...)`

Return CPP flags defined by a package (case-insensitive). Empty list if not
found.

```python
[{"cpp_flag": "ALLOW_NONHYDROST", "description": "Enable non-hydrostatic solver"}]
```

### `search_docs(query, top_k=5, _chroma_path=...)`

Semantic similarity search over MITgcm RST documentation sections.

1. Embeds `query` with `nomic-embed-text` via Ollama.
2. Queries the `mitgcm_docs` ChromaDB collection for nearest chunks.
3. Deduplicates by `(file, section)`, returns top `top_k` sections.

Return shape:

```python
[
    {
        "file": "phys_pkgs/rbcs.rst",
        "section": "RBCS — Relaxation Boundary Conditions for T and S",
        "snippet": "The RBCS package allows ...",  # first 400 chars
    },
    ...
]
```

## Connection strategy

Each function opens its own DuckDB connection via `connect(path)` from
`src/indexer/schema.py` and closes it before returning. The default path is
`DB_PATH` (`data/index.duckdb`). The leading-underscore parameters `_db_path`
and `_chroma_path` allow tests to inject temporary paths; they are not part of
the public API.

## Example session (done-when criterion)

Answer: *"What code reads `cg3dMaxIters`, and what CPP flag guards it?"*

```python
from src.tools import namelist_to_code, get_cpp_requirements

hits = namelist_to_code("cg3dMaxIters")
# [{"id": ..., "name": "CG3D", "file": "model/src/cg3d.F",
#   "package": "model", "namelist_group": "PARM03"}]

flags = get_cpp_requirements(hits[0]["name"])
# ["ALLOW_NONHYDROST"]
```

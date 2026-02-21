# M3: Core Tools

## Context

M0–M2 have built the two indices (DuckDB code graph, ChromaDB semantic index).
M3 exposes them as plain Python callables — no server, no framework — that can
be tested from a shell and then wrapped by the M4 MCP server.

Done-when criterion: answer "what code reads `cg3dMaxIters`, and what CPP flag
guards it?" from a Python shell.

---

## Files to create

### `src/tools.py`

Single file (one module, no premature package). Seven public functions:

```
search_code(query, top_k=5)
get_subroutine(name)
get_callers(name)
get_callees(name)
namelist_to_code(param)
diagnostics_fill_to_source(field_name)
get_cpp_requirements(subroutine_name)
get_package_flags(package_name)
```

**Connection strategy**: each function opens and closes its own DuckDB
connection via `connect(db_path)` from `src/indexer/schema.py`. Default path
is `DB_PATH`. An optional `_db_path` parameter (leading underscore = internal,
not part of the public API) allows tests to inject a temp file path.

`search_code` additionally uses `get_collection(chroma_path)` from
`src/embedder/store.py` and `ollama.embed`. It takes an optional `_chroma_path`
too.

**Return types**: plain dicts and lists — easy to serialise for M4 MCP.

**Case folding**: all name lookups use `upper(col) = upper(?)` so callers
don't need to know MITgcm's capitalisation conventions.

**`diagnostics_fill_to_source`**: use `upper(trim(field_name)) = upper(trim(?))`
because extracted field names may have trailing spaces (e.g. `'WdRHO_P '`).

**`search_code` deduplication**: fetch `top_k * 10` chunks, keep best
(lowest cosine distance) chunk per `db_id`, return top `top_k` subroutines
with DuckDB metadata joined in.

### `tests/tools/__init__.py`  (empty)

### `tests/tools/conftest.py`

Session-scoped pytest fixture `test_db(tmp_path_factory)`:
- Creates a real DuckDB file in a temp directory (`:memory:` won't work
  because each `connect()` call would get a fresh empty DB)
- Uses `connect(path)` from `src/indexer/schema.py` to run DDL
- Inserts minimal synthetic rows:
  - 2 subroutines: `CG3D` (id=1, model) and `PRE_CG3D` (id=2, model)
  - 1 call: PRE_CG3D → CG3D
  - 1 namelist_ref: `cg3dMaxIters` → subroutine 1, group `PARM02`
  - 1 diagnostics_fill: field `RHOAnoma`, subroutine 1, array `rho3d`
  - 1 cpp_guard: subroutine 1 guarded by `ALLOW_NONHYDROST`
  - 1 package_options row: package `model`, flag `ALLOW_NONHYDROST`

### `tests/tools/test_tools.py`

~20 tests, all passing `_db_path=test_db` fixture. `search_code` not unit-tested
(requires live ollama + ChromaDB); verified manually via the done-when check.

| Function | Tests |
|---|---|
| `get_subroutine` | found by name; not found → None; case-insensitive |
| `get_callers` | caller returned; empty when no callers |
| `get_callees` | callee returned; empty when no callees |
| `namelist_to_code` | param found with group; not found → [] |
| `diagnostics_fill_to_source` | field found; padded field name matches; not found → [] |
| `get_cpp_requirements` | flag returned; not found → [] |
| `get_package_flags` | flag + description returned; not found → [] |

### `docs/tools.md`

One doc file covering: purpose, all 7 function signatures + return shapes,
example session answering the done-when question.

---

## Roadmap update

Tick off all M3 checklist items in `plans/roadmap.md`.

---

## Verification

```python
from src.tools import namelist_to_code, get_cpp_requirements

hits = namelist_to_code("cg3dMaxIters")
# → [{"name": "CG3D", "file": "...", "package": "model", "namelist_group": "PARM03"}]

flags = get_cpp_requirements(hits[0]["name"])
# → ["ALLOW_NONHYDROST"]
```

Run `pixi run test` — all tests pass.

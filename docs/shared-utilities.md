# Shared utilities

Code in `src/shared/`, `src/embed_utils.py`, and `src/rst_parser.py` is used
by both MITgcm and FESOM2 backends.

---

## `src/shared/` — physics utilities

Both MCP servers expose `translate_lab_params_tool` and `check_scales_tool`.
The implementations live in `src/shared/` and are imported by both servers.

### `translate_lab_params`

Converts physical lab or ocean geometry parameters to FESOM2/MITgcm namelist
values. Accepts SI inputs (metres, rad/s, K) and returns structured namelist
stanzas plus derived quantities.

```python
from src.shared import translate_lab_params

result = translate_lab_params(
    Lx=1e6, Ly=1e6, depth=4000,
    Omega=7.27e-5, delta_T=20,
    Nx=100, Ny=100, Nz=40,
)
# result["derived"]["dt_s"]         → suggested time step (s)
# result["PARM04"]["delX"]          → grid spacing string
```

Input parameters:

| Parameter | Unit | Description |
|---|---|---|
| `Lx`, `Ly` | m | Domain length in x, y |
| `depth` | m | Water depth |
| `Omega` | rad/s | Rotation rate (0 for non-rotating) |
| `delta_T` | K | Temperature contrast (optional) |
| `Nx`, `Ny`, `Nz` | — | Grid cells (optional; omit to skip grid output) |
| `nu` | m²/s | Kinematic viscosity (default: 1e-6) |
| `kappa` | m²/s | Thermal diffusivity (default: 1.4e-7) |
| `alpha` | K⁻¹ | Thermal expansion coefficient (default: 2e-4) |

Returns a dict with keys `PARM01`, `EOS_PARM01`, `PARM04` (if grid given),
`derived`, `notes`. Use `derived` values to set `step_per_day`,
`visc_sh_limit`, `K_hor`, `A_ver`, and `tAlpha` in namelists.

### `check_scales`

Computes dimensionless numbers (Ro, Ek, Bu, CFL) and flags configuration
issues.

```python
from src.shared import check_scales

result = check_scales(
    Lx=1e6, Ly=1e6, depth=4000,
    Omega=7.27e-5, delta_T=20,
    dx=1e4, dy=1e4, dz=100,
    dt=600, U=0.1,
)
# result["numbers"]["Ro"]    → Rossby number
# result["flags"]            → list of {"level": "warning"|"info", "message": str}
```

Returns `{"numbers": {...}, "flags": [...]}`. Flags warn about CFL
violations, under-resolved boundary layers, and similar issues.

---

## `src/embed_utils.py` — embedding helpers

Shared constants and the chunking function used by all embedding pipelines.

| Symbol | Value | Meaning |
|---|---|---|
| `EMBED_MODEL` | `"nomic-embed-text"` | Ollama model name |
| `MAX_CHARS` | 4000 | Maximum characters per chunk |
| `OVERLAP` | 200 | Overlap between adjacent chunks |
| `BATCH_SIZE` | 10 | Chunks per Ollama request |

```python
_chunk_text(text: str, max_chars: int = MAX_CHARS, overlap: int = OVERLAP) -> list[str]
```

Splits `text` into overlapping chunks of at most `max_chars` characters.
Short texts (≤ `max_chars`) produce a single chunk.

---

## `src/rst_parser.py` — RST section iterator

Shared RST parser used by both `src/mitgcm/docs_indexer/pipeline.py` and
`src/fesom2/embedder/docs_pipeline.py`.

```python
iter_sections(doc_root: Path) -> list[dict]
```

Walks all `.rst` files under `doc_root`, splits each into sections
(heading + body), strips RST markup, and returns a flat list of
`{"file": str, "section": str, "text": str}` dicts. Sections with no
remaining text after stripping are dropped.

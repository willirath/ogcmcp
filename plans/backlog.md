# Backlog

Known limitations and deferred improvements, in no particular priority order.

## short bullets, to be expanded into real backlog items

- Gotchas --> JSON? (Same for encoding other knowledge?)
- Add docs/ or readme to experiment (reflecting the user request, considerations, discussion, etc)
- Add tool for forcing file generation?
- visualisation?
- pixi task for each experiment the way to go? Consider making experiments self contained? (Later...)
- ensure (via claude?) that readme etc. doesn't go stale
- feed back knowledge / gotchas from setting up experiments
- how to ensure tool use instread of direct reading? eg. 
```
 Bash(sed -n '80,130p' /Users/wrath/src/github.com/willirath/2026-dtg/MITg
      cm/pkg/rbcs/rbcs_add_tendency.F)                      
  ⎿            gTendency(i,j) = gTendency(i,j)                             
          &       - RBC_maskV(i,j,k,bi,bj)*rec_tauRlx                   
          &        *( vVel(i,j,k,bi,bj)- RBCvVel(i,j,k,bi,bj) )            
     … +24 lines (ctrl+o to expand)                             
```

---

## CPP guard attribution is per-subroutine, not per-line

**Where**: `src/indexer/extract.py`, `cpp_guards` table

**Problem**: The extractor attributes every `#ifdef` / `#ifndef` found anywhere
in a subroutine to the subroutine as a whole. For large routines like
`INI_PARMS` (~1500 lines), this produces many unrelated flags —
`SHORTWAVE_HEATING`, `ALLOW_EXCH2`, etc. — none of which guard the specific
line being queried.

**Observed via**: `get_cpp_requirements_tool("INI_PARMS")` after asking which
CPP flag guards the reading of `cg3dMaxIters`. The flags returned are
technically present in the subroutine but irrelevant to that parameter.

**Fix**: Track the active `#ifdef` stack at each line and store
`(subroutine_id, cpp_flag, line_start, line_end)` rather than just
`(subroutine_id, cpp_flag)`. `get_cpp_requirements` could then accept an
optional line range to filter to guards that actually wrap the target code.
This is a meaningful scope increase — the regex extractor would need to
maintain a CPP nesting stack across lines.

---

## Embedding pipeline: GPU Ollama for larger context and faster indexing

**Where**: `src/embedder/pipeline.py`, `compose.yml`

**Problem**: The CPU Docker Ollama container has an effective context window
that is too small for some dense free-form Fortran chunks (~4000 chars).
At least one chunk (`diffusivity_free` chunk 0, `atm_phys`) is skipped with
a "context length exceeded" warning on each run. Indexing 4910 chunks takes
~45 minutes on CPU.

**Opportunity**: The embedding pipeline only needs to run once (or on MITgcm
updates). Running it against a GPU-backed Ollama instance (V100/H100) would:
- Reduce indexing time from ~45 min to minutes
- Allow a larger `num_ctx` (e.g. 8192) to eliminate the skipped chunks
- Enable larger, higher-quality embedding models (e.g. `mxbai-embed-large`)

The MCP server itself requires only a single embedding call per `search_code`
query and runs fine on a laptop. The natural split is: build `data/chroma/`
on a GPU host, ship it to the local machine, query locally.

**Wiring change needed**: make the Ollama base URL configurable (currently
hardcoded to localhost via the `ollama` client default). One-liner in
`pipeline.py` and `tools.py`.

---


## INI_PARMS conflates "reads from namelist" with "uses the parameter"

**Where**: `namelist_refs` table, `namelist_to_code` tool

**Problem**: `INI_PARMS` reads every namelist parameter in MITgcm into COMMON
blocks. A query for `cg3dMaxIters` correctly returns `INI_PARMS` (it appears
in a `&PARM02` block there), but the semantically useful answer is often
`CG3D` — the subroutine that *uses* the parameter.

**Fix**: No schema change needed. Callers should be aware that
`namelist_to_code` finds declaration sites, not use sites. A follow-on
`get_callers` / semantic search step is needed to find the subroutine that
acts on the value. Could also add a `uses_variable` tool that searches
source_text for bare references to a variable name (imprecise but useful).

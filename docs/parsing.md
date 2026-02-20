# Parsing

## Approach

MITgcm's source mixes fixed-form `.F` files (with CPP macros and column-6
continuation markers) and free-form `.F90` files. `tree-sitter-fortran` is
installed but proved too fragile for MITgcm's fixed-form style — `C` comments
in column 1 are misread as code, and column-6 continuation confuses the parser.

The indexer uses **regex directly on raw source files** instead. No CPP
pre-pass is needed. This is more robust for the specific patterns we extract
and easier to maintain.

## What gets extracted

From each `.F` / `.F90` file under `MITgcm/model/src/`, `MITgcm/pkg/`, and
`MITgcm/eesupp/src/`:

| Pattern | How |
|---|---|
| Subroutine names and line ranges | `SUBROUTINE name` / `END SUBROUTINE` boundary detection |
| `CALL` statements | `^\s+CALL\s+(\w+)` per line |
| `NAMELIST` declarations | `NAMELIST /GROUP/` + accumulated continuation lines |
| `DIAGNOSTICS_FILL` calls | `CALL DIAGNOSTICS_FILL(array, 'FIELD', ...)` |
| `#ifdef` / `#ifndef` guards | Raw pre-CPP source, tracked as a stack |

## Fixed-form specifics

- Comment lines: `C`, `*`, or `!` in column 1 — skipped
- Continuation lines: any non-blank, non-zero character in column 6
  (MITgcm convention is `     &` — 5 spaces + ampersand)
- NAMELIST declarations span many continuation lines; the extractor
  accumulates them before parsing parameter names

## Subroutine boundary detection

`SUBROUTINE name` marks the start. `END SUBROUTINE` or bare `END` (with
nothing after it) marks the end. `END DO`, `END IF`, `ENDDO`, `ENDIF`, etc.
are excluded by a separate pattern so they don't prematurely close a
subroutine.

## CPP guard attribution

Before subroutine extraction, a first pass walks all lines and tracks the
`#ifdef` / `#ifndef` / `#endif` stack. Each subroutine receives the union of
all CPP flags active at any point within its line range.

## Entry point

```sh
pixi run python -m src.indexer.pipeline
```

Produces `index.duckdb` in the project root.

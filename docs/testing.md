# Testing

## Overview

The test suite covers two components:

```
tests/
├── indexer/
│   ├── test_extract.py              unit tests for normal extraction behaviour
│   └── test_extract_adversarial.py  edge cases and bug regression tests
└── embedder/
    └── test_pipeline.py             chunking logic tests
```

All tests use synthetic data (inline string constants or synthetic strings).
No real MITgcm source files, DuckDB instance, or ollama server are required.

## Why synthetic Fortran snippets

Real MITgcm files have several drawbacks as test fixtures:

- They are tied to a specific MITgcm commit. A test that passes against one
  version can silently fail when the submodule is updated.
- Large files are slow to read and produce noisy failures — it is hard to tell
  which pattern caused an assertion to fail.
- Real files cannot be constructed to contain exactly one interesting pattern
  in isolation.

Synthetic snippets are short, version-independent, and deliberately minimal.
Each snippet contains exactly the pattern under test and nothing else. The
helper `_write(content, suffix)` writes the content to a `tempfile` and
returns a `Path`, which is then passed directly to `extract_file()`.

## `test_extract.py` — unit tests

Covers the primary extraction paths under normal, well-formed input:

| Test group | What it verifies |
|---|---|
| Subroutine detection (fixed-form) | Name, line range, source text inclusion |
| Subroutine detection (free-form) | Case-insensitive name match in `.F90` |
| Multiple subroutines in one file | All detected; line ranges non-overlapping |
| `CALL` extraction | Correct callee names; duplicates deduplicated |
| `NAMELIST` extraction | Group name and all parameter names captured across continuation lines |
| `DIAGNOSTICS_FILL` extraction | Field name and array name captured |
| CPP guard extraction | Guard flag attributed to subroutine; absent when no guard |
| Package attribution | `model` from `model/src/`, package name from `pkg/<name>/` |

These tests are expected to pass unconditionally.

## `test_extract_adversarial.py` — adversarial tests

Adversarial tests probe patterns that are syntactically unusual or that have
previously caused extraction failures. Each test is prefaced with a comment
block explaining the MITgcm code pattern being exercised and the failure mode
being probed.

The test cases are:

| Edge case | Description |
|---|---|
| 1 — U/O/I continuation markers | MITgcm uses `U`, `O`, `I` in column 6 of argument-list continuations; these are valid continuation chars and must not be confused with comment markers |
| 2 — CBOP/CEOP documentation blocks | The `C !ROUTINE: NAME` line inside a CBOP block mentions the subroutine name; must not create a spurious record |
| 3 — `DIAGNOSTICS_FILL` split across lines | Array name on line N, quoted field string on line N+1; see B1 below |
| 4 — Long NAMELIST over many continuation lines | All 12 parameters across 6 continuation lines must be found |
| 5 — Nested `#ifdef` blocks | Subroutine inside two nested guards must report both flags |
| 6 — `#ifdef`/`#else`/`#endif` | Both subroutines detected; known imprecision in else-branch guard attribution is documented |
| 7 — `END` inside a string or comment | `WRITE(*,*) 'END of computation'` must not close the subroutine |
| 8 — Six-space `END` | Standard MITgcm six-space indent on bare `END` must match `RE_SUB_END` |
| 9 — `ENDDO` / `ENDIF` / `END DO` / `END IF` | Must not terminate subroutine scanning |
| 10 — `CALL` in a comment line | `C CALL GHOST_FUNC` must be skipped; real CALL after it must still be extracted |
| 11 — Subroutine name in a comment | `C ... SUBROUTINE PHANTOM` must not create a record |
| 12 — Free-form `SUBROUTINE` in a `!` comment | Same as 11 for `.F90` |
| 13 — Indented `SUBROUTINE` in free-form | `^\s*SUBROUTINE` must match with leading spaces |
| 14 — `END SUBROUTINE name` form | Named end must be recognised |
| 15 — `END` with trailing inline comment | `END ! comment` must still close the subroutine |
| 16 — `CALL` embedded in `IF` on same line | See B2 below |
| 17 — Multiple `NAMELIST` groups in one subroutine | Each group and its parameters captured separately |
| 18 — `ENDDO` immediately before `END` | Scanner must not confuse `ENDDO` with subroutine end |
| 19 — `DIAGNOSTICS_FILL` with padded field name | Trailing spaces inside quotes are captured |
| 20 — `#endif` after subroutine `END` | Guard still attributed to subroutine |
| 21 — `ENDDO` at column 7 (six leading spaces) | Standard indent must still match `RE_END_BLOCK` |
| 22 — Bare `END` with six spaces (MITgcm norm) | Confirms `\s{0,9}` window in `RE_SUB_END` covers six |
| 23 — `ENDDO` at column 1 | Non-standard; must not close subroutine |
| 24 — Multiple `DIAGNOSTICS_FILL` in one subroutine | Duplicates retained (no dedup on `diag_fills`) |
| 25 — Empty file / comments-only file | Must return `[]` without raising |

### What "adversarial" means here

Adversarial tests are written by reasoning about how the regex patterns can
fail, not by running the extractor and seeing what happens. The process is:

1. Identify a realistic MITgcm code pattern that does not fit the simplest
   reading of the regex (e.g. a keyword split across continuation lines).
2. Write the minimal Fortran snippet that triggers the failure.
3. Write an assertion that expresses the correct result.
4. If the extractor fails, fix the extractor and keep the test as a
   regression guard.

Two bugs were found this way during initial development:

### Bug B1 — `DIAGNOSTICS_FILL` split across continuation lines

**Symptom.** When the array name and quoted field string appear on separate
lines:

```fortran
      CALL DIAGNOSTICS_FILL( rho3d,
     &                       'RHOAnoma',
     &                       0, Nr, 0, 1, 1, myThid )
```

the original regex searched only a single line for the pattern
`CALL\s+DIAGNOSTICS_FILL\s*\(\s*(\w+)\s*,\s*'([^']+)'`. The field name
`'RHOAnoma'` was invisible because it was on a continuation line.

**Fix.** When a line contains `DIAGNOSTICS_FILL`, the extractor now joins
it with any following continuation lines before applying the regex. The
joining respects both fixed-form (column-6 marker) and free-form (`&`
trailing/leading) continuation conventions.

**Test.** Edge case 3 (`test_diag_fill_split_across_continuation_lines`).

### Bug B2 — inline `IF (cond) CALL FOO` pattern

**Symptom.** MITgcm uses single-line conditional calls:

```fortran
      IF ( flag ) CALL HIDDEN_FUNC( x )
```

`RE_CALL` requires the line to start with whitespace followed immediately by
`CALL`. When `IF` appears first, the regex does not match and `HIDDEN_FUNC`
is lost.

**Fix.** When `RE_CALL` does not match, the extractor falls back to
`RE_CALL_INLINE` (`\bCALL\s+(\w+)`) via `finditer` on the whole line.
Self-calls are filtered to avoid false positives from misparsed subroutine
declarations.

**Test.** Edge case 16 (`test_call_inline_if_extracted`).

## `test_pipeline.py` — embedder chunking tests

Covers `_chunk_text` and `_doc_chunks` from `src/embedder/pipeline.py`.
All tests use synthetic strings — no DuckDB or ollama required.

| Test | What it verifies |
|---|---|
| Short text not split | text ≤ MAX_CHARS → single chunk, unchanged |
| Exact MAX_CHARS not split | boundary case: one chunk |
| Long text splits | text > MAX_CHARS → multiple chunks |
| Each chunk at most MAX_CHARS | no chunk exceeds the size limit |
| Overlap shared between adjacent chunks | `chunk[i][-OVERLAP:] == chunk[i+1][:OVERLAP]` |
| No-overlap joins to original | `"".join(chunks) == text` when `overlap=0` |
| Empty string → one empty chunk | edge case: empty input |
| All positions covered | every char position in original appears in ≥ 1 chunk |
| Chunk ids unique | no id collisions across chunks of one subroutine |
| Ids include db_id | ids are of the form `"{db_id}_{i}"` |
| All chunks carry same db_id | metadata consistent across chunks |
| Chunk index sequential | `chunk_index` is 0, 1, 2, … |
| n_chunks consistent | all chunks agree on the total count |
| Doc text contains header | each doc starts with `SUBROUTINE name [pkg]` |
| Header in every chunk | header present even in non-first chunks |

## Running tests

```sh
pixi run test
```

This runs `pytest tests/ -v`. No MITgcm source tree, DuckDB file, or
ollama server is required.

## Conventions for adding new tests

### Indexer tests

- Add normal-behaviour tests to `test_extract.py`; add edge-case and
  regression tests to `test_extract_adversarial.py`.
- Define the Fortran snippet as a module-level string constant before the
  test function(s) that use it. Name it `FIXED_<DESCRIPTION>` for `.F`
  snippets or `FREE_<DESCRIPTION>` for `.F90` snippets.
- Prefix the constant with a comment block (matching the style in
  `test_extract_adversarial.py`) that names the MITgcm file or pattern being
  exercised and states what failure mode the test probes.
- Use `_write(content)` for `.F` (default) and `_write(content, suffix=".F90")`
  for free-form. Do not reuse the same temp file across tests — each call to
  `_write` creates a new file.
- If you are documenting a known limitation rather than a bug, leave the test
  passing (asserting the imprecise-but-not-crashing behaviour) and add a
  docstring explaining what "correct" would mean and why it is not implemented.

### General conventions

- Each test function should test one assertion. Prefer two small test
  functions over one function with two asserts.
- Tests must not require external services (DuckDB, ChromaDB, ollama).
  Use synthetic fixtures or mock data.

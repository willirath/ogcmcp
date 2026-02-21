# CLAUDE.md

Working notes for Claude Code on conventions in this project.

## Tooling

- **pixi** for all Python dependencies — conda-forge for system tools, PyPI for Python packages
- **No brew** — keep everything OS-agnostic
- **Docker Compose** for service dependencies (currently: ollama embedding server)
- Convenience tasks in `pixi.toml`: `pixi run test`, `pixi run index`

## Project structure

```
src/<component>/     application code
tests/<component>/   mirrors src/ structure
docs/                one .md per component, written when implementing
plans/               design docs and roadmap
data/                generated artifacts (gitignored)
```

- Generated artifacts (DuckDB, ChromaDB, model data) go in `data/` or named dirs — never committed
- `__pycache__/`, `*.pyc` — gitignored

## Docs

- Write docs **when implementing** the respective part, not ahead of time
- One file per component in `docs/`
- Use proper markdown headings, not bold text as pseudo-headings
- Docs should be self-contained — no references to other design documents the reader may not have
- Directory listings use `tree`-style output (with `└──`, `├──`, `│` characters), not flat indented lists

## Planning and roadmap

- Plans go in `plans/` as markdown — do not use Claude Code's built-in plan mode
- Roadmap in `plans/roadmap.md` — tick off checklist items when done, include a "done when" criterion per milestone
- Each milestone checklist includes a docs item

## Commits

- Commit after each logical unit of work
- Commit message summarises *what changed and why*, not just what
- Run `pixi run test` before committing code changes
- Don't commit generated files — check `.gitignore` first

## Citations and factual claims

- Do not invent paper citations or model names — flag uncertainty explicitly
- If unsure whether a package/model exists, say so rather than guessing

## MITgcm source lookups

When investigating MITgcm package behaviour, always use the MCP tools before
reaching for Bash grep/sed on MITgcm source files:

| Question | Use this tool first |
|---|---|
| What does subroutine X do / what is its source? | `get_source_tool("X")` |
| What namelist parameters does subroutine X read? | `namelist_to_code("param")` |
| What CPP flags guard subroutine X? | `get_cpp_requirements_tool("X")` |
| What compile-time flags does package P expose? | `get_package_flags_tool("P")` |
| General question about a package or behaviour | `search_code_tool("...")` |

Direct reads of `MITgcm/pkg/` via Bash (`grep`, `sed`, `cat`) are a last
resort — for things not in the index (e.g. header-only `.h` files with no
subroutine, error message strings).

## Docker

- Pin all base images to a specific digest (`FROM image@sha256:...`) — never use floating tags like `latest` or `3.13-slim`

## Code style

- Prefer simple and focused over abstracted and general
- No premature structure — add `src/<package>/` directories when there is more than one file warranting it
- Tests use synthetic fixtures, not real MITgcm files — keeps tests fast and commit-independent

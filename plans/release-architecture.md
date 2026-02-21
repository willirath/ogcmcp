# Release architecture

Design decisions for turning the current dev repo into a distributable package.
Feedback sections are marked `> **WR:**` — fill in inline.

---

## What gets shipped

The current repo bundles MITgcm as a git submodule and generates indices
locally. For a release this should be inverted: ship the indices, not the
source.

Rationale:
- MITgcm has its own license; bundling it in a pip package is awkward
- Academic motivation: if users have live access to source, the RAG
  contribution is unquantifiable. Pre-built indices for a pinned checkpoint
  are the clean experimental unit.
- Practical: the source is ~1 GB; the indices are much smaller

The release artifact is therefore: **Python package + downloadable pre-built
indices**. The package records which MITgcm checkpoint was indexed as metadata.
Users who want to index a different version can do so, but that is an advanced
use case, not the default path.

> **WR:**
>
> (Do you agree with the "indices not source" framing? Any concern about the
> MITgcm license specifically — it's MIT licensed, so bundling is technically
> fine. Is the academic motivation strong enough to outweigh the convenience of
> bundling? Is re-indexing a supported use case you want to advertise?)

---

## Index hosting

Options:

- **Zenodo** — DOI, citable, suitable for a paper. Immutable once published.
  Versioning requires new deposits. 50 GB limit per record. Good fit if
  this is headed toward a publication.
- **HuggingFace datasets** — easy `datasets` library integration, version
  history, large file support. More software-oriented than academic.
- **GitHub releases** — simple, no extra account. 2 GB per file limit,
  which may be tight for ChromaDB. Less citable.

Recommended: Zenodo for the indexed artifact tied to a paper; GitHub releases
for software-only updates between publications.

> **WR:**
>
> (Which hosting fits your workflow? Is there a paper in view that would
> motivate a Zenodo deposit? What is the approximate size of the current
> indices — is the 2 GB GitHub releases limit a real concern?)

---

## Embedding at query time: Ollama vs sentence-transformers

The current system calls a live Ollama server for every `search_code` /
`search_docs` query. This is fine for development but is a real barrier for a
pip package — users would need to run a Docker service or install Ollama
natively.

**Option A: Keep Ollama**
- Make the server URL configurable via environment variable
  (`MITGCM_MCP_OLLAMA_URL`, defaulting to `http://localhost:11434`)
- Document as a prerequisite
- No index rebuild needed
- Acceptable if the target user is already running LLM infrastructure (e.g.
  researchers with a workstation GPU running Ollama anyway)

**Option B: Switch to sentence-transformers**
- `nomic-ai/nomic-embed-text-v1` is available on HuggingFace, compatible with
  the same embedding space as the Ollama `nomic-embed-text` model
- Runs in-process, no external service
- Heavier install (~500 MB model download on first use)
- All existing indices would need to be rebuilt with the new backend
- Cleaner for a pip package — zero external service dependency

The choice here has downstream consequences: Option B requires rebuilding
all indices and changes the packaging story significantly. It should be decided
before any packaging work starts.

> **WR:**
>
> (Who is the target user? Someone already running Ollama (researcher/developer
> with LLM infrastructure) or someone who just wants `pip install` to work?
> Is the 500 MB model download acceptable? Is index rebuilding a one-time cost
> you're willing to pay?)

---

## Package structure

With pyproject.toml replacing pixi.toml as the package descriptor, the
installable package would expose:

```
mitgcm-mcp serve          # start the MCP stdio server
mitgcm-mcp download       # fetch pre-built indices from hosting
mitgcm-mcp build-index    # (advanced) index a local MITgcm checkout
```

Claude Code's `.mcp.json` would change from `pixi run serve` to:

```json
{"command": "uvx", "args": ["mitgcm-mcp", "serve"]}
```

or after a local install:

```json
{"command": "mitgcm-mcp", "args": ["serve"]}
```

Pixi stays as the development tool; it is not a user-facing requirement.

> **WR:**
>
> (Is the `uvx mitgcm-mcp` invocation style familiar / acceptable? Any
> preference for conda packaging alongside pip? Any concern about the CLI
> surface — should `build-index` be advertised or hidden?)

---

## Package scope

The current server exposes three layers:

1. **Code search** — DuckDB + ChromaDB over Fortran source
2. **Docs search** — ChromaDB over RST documentation
3. **Domain knowledge** — scales, gotchas, suggest_experiment_config,
   translate_lab_params

All three feel like they belong in a single package — the domain knowledge is
what makes this MITgcm-specific rather than generic RAG. But layers 1 and 2
depend on the pre-built indices while layer 3 is pure Python and requires
nothing. One option is to make layers 1 and 2 fail gracefully when indices are
absent (return a helpful error rather than crashing), so the package is usable
— for domain-knowledge queries at least — without running `download`.

> **WR:**
>
> (Should the package be usable without indices? Is there a case for splitting
> domain knowledge into a separate lightweight package that has no data
> dependency? Or is the monorepo / single package the right shape?)

---

## Academic evaluation angle

If the goal is to evaluate how much the RAG layer contributes over a base LLM,
the "no bundled source" design creates a clean experimental setup:

- Condition A: base LLM, no tools
- Condition B: LLM + code RAG (DuckDB + ChromaDB subroutines)
- Condition C: LLM + docs RAG (ChromaDB mitgcm_docs)
- Condition D: LLM + both + domain knowledge layer

A fixed question set with ground-truth answers (e.g. "what namelist parameter
controls the Coriolis frequency?", "what CPP flag enables non-hydrostatic
mode?", "what is the RBCS restoring tendency equation?") would let you measure
correctness and hallucination rate across conditions.

The pre-built indices for a pinned MITgcm checkpoint are the natural citable
artifact for such a study.

> **WR:**
>
> (Is a paper a concrete goal? Does the evaluation framing above match what
> you have in mind? What would a "good" result look like — is the hypothesis
> that RAG reduces hallucination, improves correctness, or something else?)

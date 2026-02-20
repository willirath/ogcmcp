# System diagrams

## Build pipeline

How MITgcm Fortran source is transformed into the two queryable indices.

```mermaid
flowchart LR
    src["MITgcm source (.F / .F90)"]

    subgraph index ["pixi run index"]
        ts["tree-sitter Fortran parser"]
        duck[("DuckDB code graph (data/index.duckdb)")]
    end

    subgraph embed ["pixi run embed"]
        ollama["Ollama / nomic-embed-text (Docker)"]
        chroma[("ChromaDB semantic index (data/chroma/)")]
    end

    src --> ts --> duck
    duck --> ollama --> chroma
```

The DuckDB code graph stores structural relationships: subroutines, call
edges, namelist references, CPP guards, and diagnostics fills. The ChromaDB
index stores vector embeddings of overlapping 4000-character chunks of each
subroutine's source, enabling natural-language search.

---

## Database roles

What each index stores and which tools query it.

```mermaid
flowchart TB
    subgraph duck ["DuckDB — structural index"]
        d1["subroutines (name, file, package, source_text)"]
        d2["calls (caller → callee)"]
        d3["namelist_refs (param, group → subroutine)"]
        d4["cpp_guards (flag → subroutine)"]
        d5["diagnostics_fills (field → subroutine)"]
        d6["package_options (package → CPP flags)"]
    end

    subgraph chroma ["ChromaDB — semantic index"]
        c1["4910 overlapping 4000-char chunks"]
        c2["nomic-embed-text vectors (768-dim)"]
        c3["metadata: db_id, chunk_index, name, package"]
    end

    subgraph tools ["tools.py"]
        t1["get_subroutine / get_source"]
        t2["get_callers / get_callees"]
        t3["namelist_to_code"]
        t4["get_cpp_requirements"]
        t5["diagnostics_fill_to_source"]
        t6["get_package_flags"]
        t7["search_code"]
    end

    t1 & t2 & t3 & t4 & t5 & t6 --> duck
    t7 --> chroma
    t7 --> duck
```

`search_code` queries ChromaDB for the nearest-neighbour chunks, then joins
back to DuckDB on `db_id` to return full subroutine metadata.

---

## Query-time flow

How a question from a user reaches the indices and returns an answer.

```mermaid
sequenceDiagram
    actor User
    participant CC as Claude Code
    participant MCP as MCP server<br/>(src/server.py, stdio)
    participant T as tools.py
    participant DB as DuckDB
    participant OL as Ollama
    participant CR as ChromaDB

    User->>CC: ask a question

    alt structural query
        CC->>MCP: call tool (e.g. get_callers, namelist_to_code)
        MCP->>T: invoke function
        T->>DB: SQL query
        DB-->>T: rows
        T-->>MCP: list / dict
        MCP-->>CC: tool result
    else semantic search
        CC->>MCP: call search_code_tool(query)
        MCP->>T: search_code(query)
        T->>OL: embed query string
        OL-->>T: query vector
        T->>CR: nearest-neighbour search
        CR-->>T: top-k chunks + metadata
        T->>DB: join chunk metadata → subroutine rows
        DB-->>T: enriched results
        T-->>MCP: list of dicts
        MCP-->>CC: tool result
    end

    CC->>User: answer
```

The MCP server is launched automatically by Claude Code as a subprocess when
`.mcp.json` is present. Communication is over stdin/stdout — no network port
is involved.

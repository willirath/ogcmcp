# DuckDB code graph

## Overview

The code graph is a single DuckDB file (`index.duckdb`) containing structured
information extracted from MITgcm's Fortran source. It is the primary store
for code-navigation queries.

## Schema

```sql
metadata(key TEXT PRIMARY KEY, value TEXT)
-- e.g. mitgcm_commit_sha, indexed_at

subroutines(id, name, file, package, line_start, line_end, source_text)
calls(caller_id, callee_name)
namelist_refs(param_name, subroutine_id, namelist_group)
diagnostics_fills(field_name, subroutine_id, array_name)
cpp_guards(subroutine_id, cpp_flag)
package_options(package_name, cpp_flag, description)
```

## Example queries

**What subroutine declares a namelist parameter, and in which group?**
```sql
SELECT nr.param_name, s.name, nr.namelist_group, s.file
FROM namelist_refs nr
JOIN subroutines s ON s.id = nr.subroutine_id
WHERE nr.param_name = 'cg3dMaxIters';
```

**What subroutines call CG3D?**
```sql
SELECT s.name, s.file
FROM calls c
JOIN subroutines s ON s.id = c.caller_id
WHERE c.callee_name = 'CG3D';
```

**What does CG3D call?**
```sql
SELECT c.callee_name
FROM calls c
JOIN subroutines s ON s.id = c.caller_id
WHERE s.name = 'CG3D';
```

**What CPP flags gate CG3D?**
```sql
SELECT g.cpp_flag
FROM cpp_guards g
JOIN subroutines s ON s.id = g.subroutine_id
WHERE s.name = 'CG3D';
```

**What diagnostic fields does a subroutine fill?**
```sql
SELECT df.field_name, df.array_name
FROM diagnostics_fills df
JOIN subroutines s ON s.id = df.subroutine_id
WHERE s.name = 'DIAGS_RHO_G';
```

## Rebuilding

```sh
rm -f index.duckdb
pixi run python -m src.indexer.pipeline
```

Re-run after updating the MITgcm submodule.

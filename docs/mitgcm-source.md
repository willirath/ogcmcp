# MITgcm source

## Submodule

The MITgcm source is pinned as a git submodule at `MITgcm/`:

```sh
git submodule update --init    # first-time checkout
git submodule update --remote  # advance to latest upstream commit
```

The submodule ref (commit SHA) is tracked by git and committed alongside
the rest of the project. This is the version record — no separate lockfile
is needed.

## Updating

When a new MITgcm commit is needed (new feature, bug fix):

```sh
git submodule update --remote   # advance submodule to latest upstream
git add MITgcm
git commit -m "Update MITgcm submodule to <sha>"
```

Then re-run the indexing pipeline (M1) to rebuild the DuckDB and ChromaDB
indexes against the new source.

## Layout

Relevant directories for indexing:

| Directory | Contents |
|---|---|
| `MITgcm/model/src/` | core dynamical kernel |
| `MITgcm/pkg/` | optional packages (diagnostics, obcs, kpp, …) |
| `MITgcm/eesupp/src/` | execution environment (threading, I/O) |
| `MITgcm/verification/` | known-good example configurations |

Files in `model/src/`, `pkg/`, and `eesupp/src/` are indexed. The
`verification/` directory is used to seed the domain knowledge layer.

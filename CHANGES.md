## 0.1.0

Initial release.

- `Eio_fs.Fs`, an in-memory backend for `Eio.Path`: `v` creates an empty
  filesystem, and `load` copies a directory tree into a new one.
- `Eio_fs.Path`, operations on any `Eio.Path`: the disk usage of a tree,
  a walk, and `pp_tree`, which prints a tree as tree(1) lists it.
- `Eio_fs.Chaos`, a directory wrapper that injects short and torn writes,
  failed syncs and power loss, for crash-consistency tests.
- `eio-fs.sync`, durable file operations: atomic publication by a temporary
  and a rename, and directory barriers.
- `eio-fs.unix`, the operations only POSIX systems have, and `eio-fs.path`,
  lexical operations on paths.
- `eio-fs-crunch`, which embeds a directory tree as an OCaml module that
  rebuilds it in memory.

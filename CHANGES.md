## Unreleased

- `Nofollow.run` reads a file the caller opened another way, with `read_file`'s bounds (#13)
- `await_record_lock` takes a `?mode`, whose `` `Shared `` asks for a read lock (#14)
- `clone_trees` clones several trees of one directory into another, all or none (#15)
- `Tree.allocated` is the bytes one entry allocates itself (#16)
- `extents` writes the file back before mapping it, so bytes not yet synced are placed; it answered `EOPNOTSUPP` for them on ext4 (#17)

## 0.1.0 (21/11/2026)

Initial release.

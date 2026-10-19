Sparse publication shares the bounded atomic publisher and its exclusive
rename backend. Extents are ordered, disjoint data within the logical size.

```ocaml
# let sparse path = Eio_fs_sync.publish_sparse ~config:{ durable = false; perm = 0o600; max_bytes = 8388608L } ~mode:`Exclusive ~size_bytes:8388608L path [(0L, Cstruct.of_string "HDR")] in ignore sparse;;
- : unit = ()
```

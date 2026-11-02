# Durable recursive directory creation

The caller supplies a borrowed directory capability and creation permissions.
Existing ancestors and an accepted existing target must be barriered too, so
retry after a failed barrier can establish durability. The directory capability
root must itself already be durable; the operation does not acquire its parent.
Files and symbolic links are refused as traversed directory entries. Callers
exclude deletion, rename and replacement during the operation; concurrent
creation is supported.

```ocaml
let (_ :
    ?exists_ok:bool -> ?barrier:Eio_fs_sync.Barrier.t ->
    perm:Eio.File.Unix_perm.t ->
    Eio.Fs.dir_ty Eio.Path.t -> unit) = Eio_fs_sync.mkdirs
```

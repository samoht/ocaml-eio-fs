# Borrowed final-leaf capability

`Eio_fs_unix.Batch.with_parent` is the shared path traversal operation. Its
input is an Eio capability path; its callback receives the final leaf below
held non-symlink parent directories. This file specifies the operation
missing on the pre-source API; these expected final typing results must pass
in the source successor without editing this immutable specification.

```ocaml
# module Parent_signature : sig end = struct
    let held_parent : _ Eio.Path.t ->
      (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a =
      Eio_fs_unix.Batch.with_parent
  end;;
module Parent_signature : sig end
```

The callback may compute and return an arbitrary result, rather than being
restricted to unit, a Batch outcome, or a string-error result. Its leaf works
with the existing Eio.Path operation vocabulary.

```ocaml
# module Parent_result : sig end = struct
    let leaf_name path =
      Eio_fs_unix.Batch.with_parent path (fun leaf ->
        let (_ : Eio.Fs.dir_ty Eio.Path.t) = leaf in
        Eio.Path.split leaf)
  end;;
module Parent_result : sig end
```

The existing path type is a capability, not a string. This negative control
must remain rejected independently of the missing operation.

```ocaml
# let string_path : Eio.Fs.dir_ty Eio.Path.t = "parent/leaf";;
Line 1, characters 46-59:
Error: This constant has type string but an expression was expected of type
         Eio.Fs.dir_ty Eio.Path.t = Eio.Fs.dir_ty Eio.Fs.dir * string
```

Typing does not prove descriptor lifetime or confinement. The callback's
borrowed leaf must not be used after the callback returns; the path type does
not enforce that lifetime. The source successor owes independent runtime
assertions for each of the following, on both native and descriptorless Eio
backends, using deterministic hooks rather than repeated races:

- A leaf under existing parents reaches its original directory capability,
  and callback return values and exceptions pass through unchanged.
- Empty, absolute, dot, dot-dot, repeated-separator and NUL components refuse
  with typed Eio errors; missing parents raise `Eio.Fs.Not_found`, and a blocked
  parent raises `Eio.Fs.Permission_denied` (wrapped by `Eio.Io`).
- Every parent is non-symlink. A parent replaced between observation and open
  is refused when its observed and opened identities disagree. A rename after
  acquisition cannot redirect the callback to an attacker-selected directory.
- Callback return and exception release the borrowed parents; a final symlink
  remains a final leaf and is never silently followed by traversal itself.
- Portable Batch and Repository removals use this same primitive; no duplicate
  traversal, empty-path reopen, or copied hard-link emulation remains.

A successful signature annotation is no evidence for these runtime claims.
New runtime calls cannot compile against the pre-source API, so they belong
in the separately committed second phase; none is represented as verified here.

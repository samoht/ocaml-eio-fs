# Shared inherited leases

The public `Lease` API owns the BSD `flock(2)` open-file-description lock.
`with_shared` is its reader operation, on the same rendezvous used by
`with_exclusive`. Independent readers can overlap, including readers in the
same process. An exclusive acquisition with `block: \`No` refuses while any
reader holds that rendezvous. Returning from a reader callback closes that
callback's descriptor without `LOCK_UN`: an inherited descriptor retains the
hold through callback return and owner exit. Only the last close releases it.
`Lease.unlock` explicitly performs `LOCK_UN` on the shared open file
description, releasing its hold even while inherited duplicates remain open;
it does not release independently acquired readers.

The path, creation default, file identity checks, cancellation and wait/refusal
policies are those of `with_exclusive`. A reader cannot replace rendezvous
contents. A cancelled acquisition neither enters its callback nor leaves a
hold once its outstanding kernel acquisition is granted and closed. Reader
acquisition refuses a symlink, directory, FIFO or changed rendezvous with the
same `Not_a_lock_file` error as exclusive acquisition. No process-local reader
list can substitute for the inherited kernel hold.

This expression fixes the complete reader signature without opening a file.
The existing `Lease.t`, `fd` and `unlock` operations serve both modes.

```ocaml
# let shared :
    ?block:[ `Yes | `No | `Fibers | `For of float ] ->
    ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t -> (Eio_fs_unix.Lease.t -> 'a) -> 'a =
    Eio_fs_unix.Lease.with_shared;;
val shared :
  ?block:[ `Fibers | `For of float | `No | `Yes ] ->
  ?create:[ `If_missing of int | `Never ] ->
  ?on_wait:(unit -> unit) ->
  [> Eio.Fs.dir_ty ] Eio.Path.t -> (Eio_fs_unix.Lease.t -> 'a) -> 'a = <fun>
```

The inherited descriptor is the canonical lease descriptor, and explicit
unlock uses that same lease value.

```ocaml
# let descriptor lease = Eio_fs_unix.Lease.fd lease;;
val descriptor : Eio_fs_unix.Lease.t -> Eio_unix.Fd.t = <fun>
# let release lease = Eio_fs_unix.Lease.unlock lease;;
val release : Eio_fs_unix.Lease.t -> unit = <fun>
```

An exclusive take that meets a reader's look, a shared lease taken and let
go at once, waits it out with `take_after_readers` and is refused only by an
exclusive holder or another take under way: the look delays the take and
never refuses it. The take holds a gate, a lease on a file of its own that
every exclusive take of the path passes, from before its look at the holders
to its end, so no other take is made between that look and its blocking take
and the wait is for readers alone. The gate is a required argument, so a take
after readers without one does not type.

```ocaml
# fun ~sw ~gate path ->
    Eio_fs_unix.Lease.take_after_readers ~sw ~gate path;;
- : sw:Eio.Switch.t ->
    gate:[> Eio.Fs.dir_ty ] Eio.Path.t ->
    [> Eio.Fs.dir_ty ] Eio.Path.t -> Eio_fs_unix.Lease.t
= <fun>
```

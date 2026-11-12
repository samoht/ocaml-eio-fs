A lease may belong to a switch while its caller continues processing events.
`take` shares the acquisition, file checks and cancellation behavior of
`with_exclusive`. `release` closes the owner's descriptor and gives back its
local admission; an inherited descriptor retains the BSD lease. `unlock`
still explicitly releases the shared open-file-description hold.

```ocaml
let take :
    sw:Eio.Switch.t ->
    ?gate:_ Eio.Path.t ->
    ?block:Eio_fs_unix.block ->
    ?create:Eio_fs_unix.create ->
    ?contents:string -> ?on_wait:(unit -> unit) ->
    _ Eio.Path.t -> Eio_fs_unix.Lease.t =
  Eio_fs_unix.Lease.take

let release : Eio_fs_unix.Lease.t -> unit = Eio_fs_unix.Lease.release
```

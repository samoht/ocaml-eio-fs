(* The directory-durability barrier. A backend with its own barrier
   semantics registers [Dir_sync] in its resource handler: the in-memory
   backend a no-op (volatile storage has nothing to sync), a crash-model
   wrapper its journal commit, a fault-injecting wrapper its faultable sync.
   A backend that registers nothing gets the POSIX route: the directory is
   opened read-only and its handle fsynced, the barrier POSIX defines for
   directory entries. Eio types [File.sync] as write-only, but the POSIX
   backends build every descriptor from one read-write implementation, so
   the operation is probed from the handle. This library depends on nothing
   but eio, so unikernel targets link it freely. *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync : ('t, 't -> path:string -> unit, [> `Dir_sync ]) Eio.Resource.pi

let find (Eio.Resource.T (v, ops)) =
  Option.map (fun f -> f v) (Eio.Resource.get_opt ops Dir_sync)

(* A directory handle that cannot sync is a wiring error, loudly: silence
   here would silently lose durability. *)
let fsync_handle (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Eio.File.Pi.Write with
  | Some (module W) -> W.sync v
  | None ->
      failwith
        "Eio_fs_sync.sync_dir: this backend's directory handle cannot sync; \
         register a Dir_sync barrier on the directory resource"

let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match find res with
  | Some barrier -> barrier ~path
  | None -> (
      Eio.Switch.run @@ fun sw ->
      match Eio.Path.open_in ~sw p with
      | handle -> fsync_handle handle
      | exception (Eio.Io _ as ex) ->
          let bt = Printexc.get_raw_backtrace () in
          Eio.Exn.reraise_with_context ex bt
            "syncing directory (backend registers no Dir_sync barrier)")

(* The durable-publication protocol shared by every store in the tree: write
   the sibling temporary [<target>.tmp] (same directory: a rename must not
   cross filesystems), push its bytes to stable storage, rename it over the
   target (atomic: readers see the old or the new file, never a torn one) and
   barrier the directory so a power cut cannot revert the publication.
   [save_durable_with] is the plumbing the POSIX facade shares; stores call
   {!save_durable}. *)
let save_durable_with ~sync_dir:barrier ~create (dir, target) content =
  let tmp = (dir, target ^ ".tmp") in
  (try
     Eio.Path.with_open_out ~create tmp (fun f ->
         Eio.Flow.copy_string content f;
         Eio.File.sync f)
   with e ->
     (try Eio.Path.unlink tmp with Eio.Io _ -> ());
     raise e);
  Eio.Path.rename tmp (dir, target);
  barrier (dir, Filename.dirname target)

let save_durable ~create path content =
  save_durable_with ~sync_dir ~create path content

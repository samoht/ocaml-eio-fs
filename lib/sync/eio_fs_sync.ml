(* The directory-durability barrier as a probeable resource operation, so it
   stays portable: the in-memory backend registers nothing to do (volatile
   storage has no barrier and needs none), a crash-model wrapper registers its
   journal commit, a block-device filesystem its flush -- and a plain POSIX
   directory, which registers nothing, resolves through the platform fallback
   that eio-fs.unix installs when linked. Eio itself exposes no directory
   sync, which is why this lives here; it depends on nothing but eio, so
   unikernel targets link it freely. *)

type (_, _, _) Eio.Resource.pi +=
  | Dir_sync : ('t, 't -> path:string -> unit, [> `Dir_sync ]) Eio.Resource.pi

let find (Eio.Resource.T (v, ops)) =
  Option.map (fun f -> f v) (Eio.Resource.get_opt ops Dir_sync)

(* A missing registration is a wiring error, not a soft condition: silence
   here would silently lose durability. A POSIX main grants the barrier as a
   capability by wrapping its directory with [Eio_fs_unix.wrap] before
   handing it to a store. *)
let sync_dir ((res, path) : _ Eio.Path.t) =
  match find res with
  | Some barrier -> barrier ~path
  | None ->
      failwith
        "Eio_fs_sync.sync_dir: this backend registers no directory barrier; on \
         POSIX hand the store a directory wrapped with Eio_fs_unix.wrap"

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

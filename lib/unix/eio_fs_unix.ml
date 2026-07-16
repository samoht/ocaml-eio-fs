external full_fsync : Unix.file_descr -> unit = "caml_eio_fs_full_fsync"

(* Eio directory resources intentionally do not expose their descriptor, so
   directory durability needs a native open before the platform sync. *)
let sync_dir p =
  let fd = Unix.openfile (Eio.Path.native_exn p) [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> full_fsync fd)

let sync_dir_if_native p = if Option.is_some (Eio.Path.native p) then sync_dir p

(* The durable-publication protocol shared by every store in the tree: write
   the sibling temporary [<target>.tmp] (same directory: a rename must not
   cross filesystems), push its bytes to stable storage, rename it over the
   target (atomic: readers see the old or the new file, never a torn one) and
   barrier the directory so a power cut cannot revert the publication. *)
let save_durable ?(sync_dir = sync_dir_if_native) ~create (dir, target) content
    =
  let tmp = (dir, target ^ ".tmp") in
  (try
     Eio.Path.with_open_out ~create tmp (fun f ->
         Eio.Flow.copy_string content f;
         Eio.File.sync f)
   with e ->
     (try Eio.Path.unlink tmp with Eio.Io _ -> ());
     raise e);
  Eio.Path.rename tmp (dir, target);
  sync_dir (dir, Filename.dirname target)

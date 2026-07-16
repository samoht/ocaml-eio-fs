external full_fsync : Unix.file_descr -> unit = "caml_eio_fs_full_fsync"

(* Eio directory resources intentionally do not expose their descriptor, so
   directory durability needs a native open before the platform sync. *)
let sync_dir p =
  let fd = Unix.openfile (Eio.Path.native_exn p) [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> full_fsync fd)

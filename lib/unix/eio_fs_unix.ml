external full_fsync : Unix.file_descr -> unit = "caml_eio_fs_full_fsync"

(* A registered barrier keeps its own semantics (the crash model commits its
   journal, the in-memory backend does nothing); a plain POSIX directory is
   opened natively -- Eio directory resources do not expose their descriptor
   -- and pushed through to stable storage, which on Darwin means F_FULLFSYNC
   where the portable fallback's fsync only reaches the drive's cache. *)
let sync_dir ((res, path) as p : _ Eio.Path.t) =
  match Eio_fs_sync.find res with
  | Some barrier -> barrier ~path
  | None ->
      let fd = Unix.openfile (Eio.Path.native_exn p) [ Unix.O_RDONLY ] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> full_fsync fd)

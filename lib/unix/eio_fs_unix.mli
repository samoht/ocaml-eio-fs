(** The POSIX durability barrier.

    The portable barrier machinery lives in {!Eio_fs_sync}: backends register
    the strongest barrier they have, and stores call it unconditionally. This
    facade adds what only POSIX can provide -- the native directory fsync -- in
    two forms: direct calls for unix-linked stores, and {!wrap}, which grants
    the barrier as a capability on a directory before it is handed to a pure-Eio
    store (one that cross-compiles to unikernels and never links unix). *)

val full_fsync : Unix.file_descr -> unit
(** [full_fsync fd] pushes [fd]'s data through to stable storage. On Darwin it
    is [fcntl F_FULLFSYNC] -- [fsync] there only flushes to the drive's cache --
    falling back to [fsync] on a filesystem that refuses it (SMB, exFAT), the
    strongest barrier the volume offers; elsewhere it is [fsync].

    @raise Unix.Unix_error as [fsync]. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place, without which a power cut can
    revert the rename to the prior entry. A backend that registers its own
    barrier ({!Eio_fs_sync.Dir_sync}) answers through it -- the in-memory
    backend does nothing, being volatile; a crash-model wrapper commits its
    journal -- and a plain POSIX directory is opened natively and
    {!full_fsync}ed.

    @raise Unix.Unix_error as [open] or [fsync].
    @raise Invalid_argument
      if [dir] registers no barrier and has no native path. *)

val save_durable : create:Eio.Fs.create -> _ Eio.Path.t -> string -> unit
(** [save_durable ~create path content] is {!Eio_fs_sync.save_durable} with the
    barrier resolved by {!sync_dir}: replace [path]'s contents atomically and
    durably over any backend, including a plain POSIX directory. *)

val wrap : _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [wrap dir] is [dir] with the native POSIX barrier registered as its
    {!Eio_fs_sync.Dir_sync} operation, and every subtree opened through it
    wrapped the same way. What a POSIX main hands to a pure-Eio store: the store
    calls {!Eio_fs_sync.sync_dir} unconditionally and this wrapper answers with
    the platform fsync. Backends that already register a barrier keep their own.
    Directory operations delegate to [dir] unchanged. *)

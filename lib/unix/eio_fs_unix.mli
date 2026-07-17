(** The POSIX durability barrier, at full platform strength.

    The portable machinery lives in {!Eio_fs_sync}: backends with their own
    barrier semantics register {!Eio_fs_sync.Dir_sync}, and a plain POSIX
    directory is fsynced through its own handle. This facade is for programs
    that link unix anyway and want the strongest barrier the platform offers: on
    Darwin {!full_fsync} (F_FULLFSYNC, through to the platter) where the
    portable route's fsync only reaches the drive's cache. Elsewhere the two are
    the same barrier. *)

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

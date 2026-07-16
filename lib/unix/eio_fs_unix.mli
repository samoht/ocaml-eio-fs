(** Durability barriers that need a native file descriptor.

    Eio's [Path] and [Fs] interfaces expose no directory fsync, yet a
    write-temporary-then-rename commit protocol is only durable once the
    directory entry itself reaches stable storage. These helpers open the
    directory natively and issue the strongest barrier the platform offers. They
    live outside the core [eio_fs] library so the in-memory backend stays free
    of unix. *)

val full_fsync : Unix.file_descr -> unit
(** [full_fsync fd] pushes [fd]'s data through to stable storage. On Darwin it
    is [fcntl F_FULLFSYNC] -- [fsync] there only flushes to the drive's cache --
    falling back to [fsync] on a filesystem that refuses it (SMB, exFAT), the
    strongest barrier the volume offers; elsewhere it is [fsync].

    @raise Unix.Unix_error as [fsync]. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place, without which a power cut can
    revert the rename to the prior entry. The directory is opened natively
    ({!Eio.Path.native_exn}), so [dir] must be backed by a real filesystem; a
    crash-model test harness substitutes its own barrier instead of calling
    this.

    @raise Unix.Unix_error as [open] or [fsync].
    @raise Invalid_argument if [dir] has no native path. *)

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

type statvfs = {
  block_size : int64;
  total_blocks : int64;
  available_blocks : int64;
}
(** Raw filesystem capacity returned by POSIX [statvfs]. Policy such as free
    percentages and cache watermarks belongs in the OCaml caller. *)

val statvfs : _ Eio.Path.t -> statvfs
(** [statvfs path] reports the fragment size, total blocks and blocks available
    to an unprivileged process on the filesystem holding [path].

    @raise Unix.Unix_error as POSIX [statvfs].
    @raise Invalid_argument if [path] has no native Unix representation. *)

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

exception Lock_unavailable of string
(** Raised by {!with_lock} in non-blocking mode when the lock is held. The
    string is the native path of the lock file. *)

val with_lock :
  ?block:bool ->
  ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
  ?contents:string ->
  _ Eio.Path.t ->
  (unit -> 'a) ->
  'a
(** [with_lock path fn] opens (or creates) [path], takes an exclusive advisory
    lock, and runs [fn ()] while holding it. Waiting for the operating-system
    lock suspends only the calling fiber, not the Eio scheduler.

    [create] controls whether an absent rendezvous is created. The default,
    [`If_missing 0o600], creates it with owner-only permissions. [`Never] locks
    only an existing file and raises {!Eio.Fs.Not_found} when absent; this is
    suitable for probing a rendezvous without leaving one behind. [contents],
    when present, replaces the lock file's contents after acquiring it. The
    update is made through the locked file handle, preserving the inode on which
    the advisory lock is held. This is useful for recording holder diagnostics
    such as a process identifier.

    If [block] is [false] (default [true]), an immediately unavailable lock
    raises {!Lock_unavailable}.

    Locks are coordinated both between fibers in this process and between
    processes. The lock is always released when [fn] returns or raises.

    @raise Lock_unavailable as described above.
    @raise Eio.Io
      with {!Eio.Fs.Not_found} when [create] is [`Never] and [path] does not
      exist.
    @raise Invalid_argument if [path] has no native Unix representation.
    @raise Unix.Unix_error as [open] or [lockf]. *)

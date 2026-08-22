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
    to an unprivileged process on the filesystem holding [path]. The call is
    made on a system thread, so it suspends only the calling fiber.

    @raise Unix.Unix_error as POSIX [statvfs].
    @raise Invalid_argument if [path] has no native Unix representation. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place, without which a power cut can
    revert the rename to the prior entry. A backend that registers its own
    barrier ({!Eio_fs_sync.Dir_sync}) answers through it -- the in-memory
    backend does nothing, being volatile; a crash-model wrapper commits its
    journal -- and a plain POSIX directory is opened and {!full_fsync}ed on the
    descriptor that open returns.

    The barrier lands on the directory the caller holds, not on whatever [dir]'s
    name resolves to when it is issued: the open resolves every component
    beneath the directory capability [dir] carries, so a name a link has taken
    is refused rather than fsynced in place of the caller's directory, and a
    directory whose name is gone is barriered all the same. A backend whose
    directory handles carry no file descriptor takes the portable barrier
    ({!Eio_fs_sync.sync_dir}) instead of this one.

    The open and the barrier are made off the calling fiber's thread, so waiting
    for the platter suspends only that fiber and not the other fibers of its
    domain. A registered barrier answers on the calling fiber, as its backend
    wrote it.

    @raise Eio.Io if [dir] cannot be opened for its barrier.
    @raise Unix.Unix_error as [fsync]. *)

exception Lock_unavailable of string
(** Raised by {!with_lock} in non-blocking mode when the lock is held. The
    string is the native path of the lock file, or the path as it prints itself
    on a backend that has no native one. *)

type Eio.Exn.Backend.t +=
  | Not_a_lock_file of string
        (** Why {!with_lock} refused a lock path: the name is not the lock's own
            regular file. The string is the native path of the lock file, or the
            path as it prints itself on a backend that has no native one. *)

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
    processes. The lock is always released when [fn] returns or raises. Between
    fibers the lock is the file's, not the name's: two paths that name one file
    take one lock, the file being identified by the device and inode of the
    descriptor the lock is held on. The operating-system lock cannot stand in
    for that, POSIX record locks not excluding within a process.

    The record lock is taken on a Unix file descriptor, and a backend that is
    not the host filesystem hands out files carrying none. Such a rendezvous is
    refused, unless its backend declares itself process-local
    ({!Eio_fs_sync.Process_local}), which says no opener of its files exists
    outside this process -- the in-memory backend's files live in this process's
    heap, with no name to open them by and no descriptor to inherit. Over such a
    backend the exclusion is the fiber lock alone, which is the whole exclusion
    those files admit; over every other one it would be an exclusion in name
    only, so it is refused instead. The rest of the acquisition is the same
    either way: the name is weighed, the opened file is checked against it, and
    [contents] is written through the held handle.

    [path] must be the lock's own regular file. A rendezvous sits in a directory
    another party writes, and resolving its name follows a symbolic link left
    there, so a link under that name would send the record lock to whatever the
    link points at: two processes would hold two locks over one resource and
    both publish. A link, a directory, a FIFO, and an entry that took the name
    between the resolution and the open are all refused with
    {!Eio.Fs.Permission_denied} carrying {!Not_a_lock_file}. What carries that
    refusal is the open itself, not a check of the name, which can change the
    moment after it is read: [create] creates exclusively, and an exclusive open
    refuses a name that exists, a dangling link included, so a refused
    acquisition leaves nothing behind at the far end of one; a name that is
    already there is verified after the open by identity, the descriptor's
    device and inode against the name's own.

    @raise Lock_unavailable as described above.
    @raise Eio.Io
      with {!Eio.Fs.Not_found} when the rendezvous cannot be opened where it is
      named: [create] is [`Never] and [path] does not exist, or the directory
      holding it does not.
    @raise Eio.Io
      with {!Eio.Fs.Permission_denied} carrying {!Not_a_lock_file} when [path]
      is not the lock's own regular file.
    @raise Invalid_argument
      if the opened rendezvous carries no Unix file descriptor and its backend
      does not register {!Eio_fs_sync.Process_local}: there is no
      operating-system lock to take on it, and nothing says the fiber lock left
      excludes every opener. The message names the path.
    @raise Unix.Unix_error as [lockf]. *)

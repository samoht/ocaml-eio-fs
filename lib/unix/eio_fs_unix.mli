(** The POSIX durability barrier at full platform strength, advisory file
    locking, filesystem capacity, and the sparse map.

    The portable machinery lives in {!Eio_fs_sync}; this facade is for programs
    that link unix anyway and want the strongest barrier the platform offers: on
    Darwin {!full_fsync} reaches the platter where the portable route's fsync
    stops at the drive's cache. {!with_lock} takes an exclusive advisory lock
    over a rendezvous file, and {!statvfs}, {!seek_data}, {!seek_hole} and
    {!extents} answer the questions about capacity and sparseness that [Unix]
    cannot. *)

val full_fsync : Unix.file_descr -> unit
(** [full_fsync fd] pushes [fd]'s data through to stable storage. On Darwin it
    is [fcntl F_FULLFSYNC], falling back to [fsync] on a filesystem that refuses
    it (SMB, exFAT); elsewhere it is [fsync].

    @raise Unix.Unix_error as [fsync]. *)

type statvfs = {
  block_size : int64;
  total_blocks : int64;
  free_blocks : int64;
  available_blocks : int64;
}
(** Raw filesystem capacity returned by POSIX [statvfs]. [free_blocks]
    ([f_bfree]) and [available_blocks] ([f_bavail]) differ by the reserve kept
    for the privileged user, so a caller needs both to say whether a filesystem
    is full from an unprivileged writer's position. *)

val statvfs : _ Eio.Path.t -> statvfs
(** [statvfs path] reports the fragment size, total blocks, free blocks and
    blocks available to an unprivileged process on the filesystem holding
    [path]. The call is made on a system thread.

    @raise Unix.Unix_error as POSIX [statvfs].
    @raise Invalid_argument if [path] has no native Unix representation. *)

(** {2 The sparse map} *)

val seek_data : Unix.file_descr -> int64 -> int64 option
(** [seek_data fd offset] is the next offset at or after [offset] that starts
    allocated data, and [None] when there is none. This is [lseek(SEEK_DATA)],
    which [Unix.lseek] has no whence for. The offset is absolute and the call
    moves [fd]'s position as [lseek] does. The call is made on a system thread.

    @raise Unix.Unix_error as [lseek]. *)

val seek_hole : Unix.file_descr -> int64 -> int64 option
(** [seek_hole fd offset] is the next offset at or after [offset] that starts a
    hole, and [None] when there is none. This is [lseek(SEEK_HOLE)].

    @raise Unix.Unix_error as [lseek]. *)

type extent = { logical : int64; physical : int64; length : int64 }
(** One physical piece a file's bytes are stored in: [length] bytes of the file
    starting at [logical] live at [physical] on the device. {!seek_data} and
    {!seek_hole} give the file's logical map; this gives its physical one. *)

val extents :
  Unix.file_descr -> offset:int64 -> length:int64 -> max:int -> extent list
(** [extents fd ~offset ~length ~max] is the physical extents backing
    [[offset], [offset + length])], in increasing logical order, at most [max]
    of them. This is [F_LOG2PHYS_EXT] on Darwin and [FS_IOC_FIEMAP] on Linux,
    neither of which [Unix] binds.

    The answer stops at the first hole, the end of the range, [max], or bytes
    the filesystem accepted but has not placed. A walk steps past the last
    extent returned and asks again, ending when the answer is empty. [max] is
    the batch, so one system-thread hop covers many extents.

    @raise Invalid_argument
      if [max] is not between 1 and 65536, or [offset] or [length] is negative.
    @raise Unix.Unix_error
      as the platform call, and [ENOSYS] on a platform that has neither. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place. A backend that registers a
    {!Eio_fs_sync.Dir_sync} answers through it; a plain POSIX directory is
    opened and {!full_fsync}ed on the descriptor that open returns, which
    resolves every component beneath [dir]'s capability. The open and the
    barrier are made off the calling fiber's thread.

    @raise Eio.Io if [dir] cannot be opened for its barrier.
    @raise Unix.Unix_error as [fsync]. *)

val sync_file : _ Eio.File.rw -> unit
(** [sync_file file] makes [file]'s data durable: the barrier a commit protocol
    issues over the bytes it wrote. A backend that registers a
    {!Eio_fs_sync.File_sync} answers through it, which is asked before any
    descriptor [file] carries, so a model of power loss observes the barrier; a
    plain POSIX file is {!full_fsync}ed on its descriptor. The barrier is made
    off the calling fiber's thread.

    @raise Unix.Unix_error as [fsync]. *)

val read_file_nofollow :
  max_bytes:int ->
  validate:(Eio.File.Stat.t -> (unit, string) result) ->
  _ Eio.Path.t ->
  Eio_fs.Nofollow.result
(** [read_file_nofollow ~max_bytes ~validate path] opens [path] through its
    directory capability while refusing a symbolic link at the final component,
    validates the opened descriptor with [validate], and reads through
    end-of-file with a one-byte bound lookahead. Validation happens before any
    byte is read. The returned [Cstruct.t] belongs to the caller and can be
    overwritten after use.

    Prefix components retain ordinary {!Eio.Path} resolution and confinement;
    only the final component is opened with [O_NOFOLLOW].

    @raise Invalid_argument
      if [max_bytes] is negative or [max_int], [path] names no final component,
      or the directory backend provides neither the registered operation nor a
      POSIX descriptor.
    @raise Eio.Io
      when the open or read fails, including when the final component is a
      symbolic link. *)

val exclusive_rename : Eio_fs_sync.Rename.t
(** [exclusive_rename] is the POSIX backend for atomic no-replace publication.
    Directory wrappers are consulted before resolving the source and target
    parents through their capabilities. Linux uses
    [renameat2(RENAME_NOREPLACE)], Darwin uses [renameatx_np(RENAME_EXCL)];
    unsupported platforms or filesystems refuse. An existing target, including a
    symbolic link, is never overwritten. *)

exception Lock_unavailable of string
(** Raised by {!with_lock} when the lock is held and [block] says not to wait
    for the holder. The string is the native path of the lock file. *)

type Eio.Exn.Backend.t +=
  | Not_a_lock_file of string
        (** Why {!with_lock} refused a lock path: the name is not the lock's own
            regular file. *)

val with_lock :
  ?block:[ `Yes | `No | `Fibers | `For of float ] ->
  ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
  ?contents:string ->
  _ Eio.Path.t ->
  (unit -> 'a) ->
  'a
(** [with_lock path fn] opens (or creates) [path], takes an exclusive advisory
    lock, and runs [fn ()] while holding it. Waiting for the operating-system
    lock suspends only the calling fiber. The lock is released when [fn] returns
    or raises.

    [create] controls whether an absent rendezvous is created; the default,
    [`If_missing 0o600], creates it owner-only, and [`Never] raises
    {!Eio.Fs.Not_found} when it is absent. [contents], when present, replaces
    the lock file's contents through the locked handle. [block] says what a hold
    another caller already has does: [`Yes] waits, [`No] raises
    {!Lock_unavailable}, [`Fibers] waits only for a fiber of this process, and
    [`For seconds] waits up to a monotonic bound. Locks are coordinated between
    fibers by device and inode, and between processes by the POSIX record lock.

    [path] must be the lock's own regular file: a link, a directory, a FIFO, or
    a name that took the entry between resolution and open is refused with
    {!Eio.Fs.Permission_denied} carrying {!Not_a_lock_file}. A backend with no
    Unix descriptor is refused unless it registers {!Eio_fs_sync.Process_local}.

    @raise Lock_unavailable as described above.
    @raise Eio.Io
      with {!Eio.Fs.Not_found} when the rendezvous cannot be opened where it is
      named.
    @raise Eio.Io
      with {!Eio.Fs.Permission_denied} carrying {!Not_a_lock_file} when [path]
      is not the lock's own regular file.
    @raise Invalid_argument
      if the opened rendezvous carries no Unix descriptor and its backend is not
      {!Eio_fs_sync.Process_local}.
    @raise Unix.Unix_error as [lockf]. *)

val lock_section :
  ?block:[ `Yes | `No | `Fibers | `For of float ] ->
  ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
  ?contents:string ->
  _ Eio.Path.t ->
  Eio_fs_sync.Lock.t
(** [lock_section path] is {!with_lock} over the rendezvous [path] as the
    critical section a backend-free component takes from its deployment. Every
    way {!with_lock} can refuse the rendezvous is raised as
    {!Eio_fs_sync.Lock.Unavailable}; what the section's body raises travels out
    untouched.

    @raise Eio_fs_sync.Lock.Unavailable
      from {!Eio_fs_sync.Lock.use}, never from [lock_section] itself. *)

(** {1:xattrs Extended attributes}

    The name-value pairs a filesystem keeps beside a file's data
    ({{:https://man7.org/linux/man-pages/man7/xattr.7.html}xattr(7)}), which
    [Unix] does not bind. Both calls act on the entry [path] names, a symbolic
    link included, and never follow it: [llistxattr], [lgetxattr] and
    [lsetxattr] on Linux, [XATTR_NOFOLLOW] on Darwin. Each call is made on a
    system thread. *)

val xattrs : _ Eio.Path.t -> (string * string) list
(** [xattrs path] is every extended attribute of [path] as [(name, value)]
    pairs, in ascending byte order of name. A value is the attribute's bytes,
    NUL bytes included.

    @raise Unix.Unix_error
      as the platform call: [ENOTSUP] where the filesystem keeps none, [ENOSYS]
      on a platform with neither family.
    @raise Invalid_argument if [path] has no native Unix representation. *)

val set_xattr : _ Eio.Path.t -> string -> string -> unit
(** [set_xattr path name value] sets the extended attribute [name] of [path] to
    [value], creating it or replacing it.

    @raise Unix.Unix_error
      as the platform call: [ENOTSUP] where the filesystem keeps none, [EPERM]
      for a namespace the process may not write ([security.*] and [trusted.*]
      without privilege on Linux), [ENOSYS] on a platform with neither family.
    @raise Invalid_argument
      if [path] has no native Unix representation or [path] or [name] holds a
      NUL byte. *)

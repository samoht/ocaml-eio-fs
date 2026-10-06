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

(** {2 Copy-on-write clones} *)

val clone_tree : _ Eio.Path.t -> _ Eio.Path.t -> bool
(** [clone_tree src dst] clones the file [src], or the directory [src] with
    everything under it, to [dst], which must not exist, in one copy-on-write
    operation that shares the data blocks: [clonefile(2)] on Darwin. The clone
    keeps modes, times, symbolic links and extended attributes, and is owned by
    the caller. It is [true] iff it cloned, and [false], with nothing written,
    when the file system has no such operation or [src] and [dst] are on two
    volumes. Linux clones one file at a time ([FICLONE]) and has no call for a
    tree, so there it is always [false]: a tree cloned file by file costs a call
    per entry, which a caller that wants it walks itself. The call is made on a
    system thread; a clone of a directory is one call whatever it holds.

    @raise Unix.Unix_error
      as [clonefile]: [dst] exists, [src] does not, permission is denied.
    @raise Invalid_argument if a path has no native Unix representation. *)

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

val import_file :
  sw:Eio.Switch.t ->
  close_unix:bool ->
  Unix.file_descr ->
  [ `Unix_fd | Eio.File.rw_ty ] Eio.Resource.t
(** [import_file ~sw ~close_unix fd] is the file [fd], a descriptor on a regular
    file, read and written through the Eio file API as a file
    {!Eio.Path.open_out} opens is: positional reads and writes, [stat], [seek],
    [sync] and [truncate]. The descriptor is closed with the file, or when [sw]
    finishes, iff [close_unix]; {!Eio_unix.Fd.remove} on its
    {!Eio_unix.Resource.fd} takes it back without closing it. This is
    {!Eio_unix.Net.import_socket_stream} for a file, which Eio does not provide:
    a caller that keeps a descriptor open across handles, as SQLite's os_unix.c
    keeps one on its inode while a lock stands, opens no other.

    @raise Invalid_argument if [sw] has finished, as {!Eio_unix.Fd.of_unix}. *)

val exclusive_rename : Eio_fs_sync.Rename.t
(** [exclusive_rename] is the POSIX backend for atomic no-replace publication.
    Directory wrappers are consulted before resolving the source and target
    parents through their capabilities. Linux uses
    [renameat2(RENAME_NOREPLACE)], Darwin uses [renameatx_np(RENAME_EXCL)];
    unsupported platforms or filesystems refuse. An existing target, including a
    symbolic link, is never overwritten. *)

type refusal = {
  path : string;  (** The native path of the lock file. *)
  holder : int option;
      (** The process holding the lock when it was refused: this process when a
          fiber of it holds it, otherwise the process the kernel reports holding
          the record lock ([fcntl F_GETLK]). [None] when the holder let go
          before it could be asked, and for a {!Lease}, whose [flock] the kernel
          names no holder of. *)
  record : string;
      (** The lock file's first 4096 bytes when the lock was refused: what the
          holder wrote through [contents], or [""]. It is read without the lock,
          so it is a diagnostic; {!field-holder} is the answer. *)
}
(** The type for a refused lock: which file, and who holds it. *)

exception Lock_unavailable of refusal
(** Raised by {!with_lock} when the lock is held and [block] says not to wait
    for the holder. It prints as {!pp_refusal} does. *)

val pp_refusal : refusal Fmt.t
(** [pp_refusal] formats a refusal as ["<path> is held by process <pid>"] for
    another process, ["<path> is held by another fiber of this process: <line>"]
    for a fiber of this one, [<line>] the first line its record carries, or
    ["<path> is held"] when its holder is [None]. *)

type Eio.Exn.Backend.t +=
  | Not_a_lock_file of string
        (** Why {!with_lock} refused a lock path: the name is not the lock's own
            regular file. *)

val with_lock :
  ?block:[ `Yes | `No | `Fibers | `For of float ] ->
  ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
  ?contents:string ->
  ?on_wait:(unit -> unit) ->
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

    With [`Yes], a lock another process holds is waited for by the kernel's
    blocking request (POSIX [fcntl] [F_SETLKW]), which answers when that process
    lets go: the lock is asked for once without waiting, [on_wait ()] is called
    once if it is held (it does nothing by default), and the request follows. A
    cancelled wait withdraws the request and answers at the cancel.

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

val hold :
  sw:Eio.Switch.t ->
  ?block:[ `Yes | `No | `Fibers | `For of float ] ->
  ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
  ?contents:string ->
  ?on_wait:(unit -> unit) ->
  _ Eio.Path.t ->
  unit
(** [hold ~sw path] is {!with_lock} over [path] for the rest of [sw]'s life: it
    returns once the lock is taken, and the lock is released when [sw] finishes.
    [block], [create], [contents] and [on_wait] are as {!with_lock}'s.

    @raise Lock_unavailable as {!with_lock} does.
    @raise Eio.Io as {!with_lock} does. *)

val probe : _ Eio.Path.t -> [ `Free of string | `Held of refusal ]
(** [probe path] is the lock over [path] as an acquisition would find it now,
    taken without acquiring it: [`Held r] names its holder as a refusal does,
    and [`Free record] carries the lock file's first 4096 bytes, what its last
    holder left there. The file is opened for reading only, and a descriptor on
    a lock this process holds is never closed while it is held, so a probe from
    the holding process keeps the hold.

    [path] must be the lock's own regular file, as {!with_lock}'s must.

    @raise Eio.Io with {!Eio.Fs.Not_found} when [path] is absent.
    @raise Eio.Io
      with {!Eio.Fs.Permission_denied} carrying {!Not_a_lock_file} when [path]
      is not the lock's own regular file. *)

(** A lock held for as long as its holder says, restated as it goes. *)
module Held : sig
  type t
  (** The type for a lock this process holds. *)

  val take :
    sw:Eio.Switch.t ->
    ?block:[ `Yes | `No | `Fibers | `For of float ] ->
    ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    t
  (** [take ~sw path] is {!with_lock} over [path], held from when it returns
      until {!release} or the end of [sw], whichever comes first. [block],
      [create], [contents] and [on_wait] are as {!with_lock}'s.

      @raise Lock_unavailable as {!with_lock} does.
      @raise Eio.Io as {!with_lock} does. *)

  val restate : t -> string -> unit
  (** [restate h contents] replaces the lock file's contents with [contents]
      through the held descriptor, so no reader finds the file unlocked between
      the writes and the hold is kept. It first takes the record lock again on
      that descriptor, which restores a hold a close of the file elsewhere in
      this process dropped.

      @raise Lock_unavailable
        when another process took the lock after such a close; nothing is
        written.
      @raise Invalid_argument if [h] has been released. *)

  val release : ?contents:string -> t -> unit
  (** [release ~contents h] writes [contents], when present, through the held
      descriptor and lets the lock go, returning once it is free. Releasing a
      released lock, or one whose switch has ended, does nothing. *)
end

module Lease : sig
  type t

  val fd : t -> Eio_unix.Fd.t
  (** [fd lease] is the descriptor whose open file description owns the lease. A
      supervisor can inherit it before the callback returns. Closing the
      parent's descriptor does not release the lease while an inherited
      descriptor remains open. The descriptor must be closed to release the
      lease; do not unlock it explicitly. *)

  val with_exclusive :
    ?block:[ `Yes | `No | `Fibers | `For of float ] ->
    ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    (t -> 'a) ->
    'a
  (** [with_exclusive path fn] opens or creates [path], takes an exclusive BSD
      [flock], and calls [fn] with the descriptor that owns it. The lease is
      released when the last copy of that descriptor closes. The callback's
      descriptor is closed when the callback returns unless another process
      inherited it.

      [create] controls whether an absent rendezvous is created; the default,
      [`If_missing 0o600], creates it owner-only, and [`Never] raises
      {!Eio.Fs.Not_found} when absent. [contents], when present, replaces the
      rendezvous contents while the lease is held. [block] selects the wait
      policy used by {!with_lock}.

      With [block] [`Yes], a lease another process holds is waited for by the
      kernel's blocking [flock], which answers when that process lets go: the
      lease is tried first, [on_wait ()] is called once if it is held (it does
      nothing by default), and the wait follows. A cancelled wait answers at the
      cancel. [flock] is no cancellation point, so its request stays with the
      kernel on a thread of its own, which lets the lease go once granted; the
      lease belongs to the open file description, which no other caller shares,
      so that grant takes nothing from anyone.

      Only a path backed by a native Unix descriptor can hold a lease. The path
      must name its own regular file; a link, directory, FIFO, or name changed
      between resolution and open is refused with {!Eio.Fs.Permission_denied}
      carrying {!Not_a_lock_file}.

      Unlike {!with_lock}, this operation does not unlock when [fn] returns.
      Closing the callback's descriptor releases the lock only when it was the
      last open descriptor referring to the same open file description.

      @raise Lock_unavailable when a hold is refused or exceeds its bound.
      @raise Eio.Io
        with {!Eio.Fs.Not_found} when the rendezvous is absent and [create] is
        [`Never].
      @raise Eio.Io
        with {!Eio.Fs.Permission_denied} carrying {!Not_a_lock_file} when the
        path is not its own regular file.
      @raise Invalid_argument when the filesystem provides no Unix descriptor.
      @raise Unix.Unix_error as [flock]. *)
end

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

module Link : sig
  type t
  type publication = Created | Present

  val with_parent :
    ?mkdir:int ->
    root:_ Eio.Path.t ->
    path:string ->
    (t -> ('a, string) result) ->
    ('a, string) result
  (** [with_parent ~root ~path fn] opens each parent component of [path] beneath
      [root] with [O_DIRECTORY | O_NOFOLLOW], then calls [fn] with the held
      parent and final basename. [path] must be canonical and relative, with no
      empty, dot, dot-dot or NUL components. [mkdir] permits missing parents to
      be created with the given mode, subject to the process umask. Created
      parents survive a later refusal.

      The handle is closed when [fn] returns and must not escape its scope. A
      renamed parent remains the held directory; this does not promise that its
      original textual path still names it. [root] is the caller's chosen
      capability, resolved once before descending. *)

  val publish : t -> target:string -> (publication, string) result
  (** [publish parent ~target] atomically creates an absent symbolic link using
      [symlinkat]. An existing link holding exactly [target] is observed as
      {!constructor-Present}; no filesystem immutability is implied. Every other
      existing entry is refused without replacing it. Errors name the original
      path and the action needed to retry. *)

  val read_regular : ?max_bytes:int -> t -> (string option, string) result
  (** [read_regular parent] opens the final entry beneath the held parent with
      [O_NOFOLLOW] and reads from that descriptor. [None] means the final entry
      is absent. A symbolic link or another non-regular entry refuses; a file
      that exceeds [max_bytes] or grows beyond its size at open also refuses.
      The default is [max_int]. The enclosing {!val-with_parent} refuses
      symbolic links at every parent component. *)
end

(** Confined directory operations, many to one system-thread hop.

    {!Eio.Path} makes one system-thread hop per operation, which dominates the
    cost of writing a large tree of small files. A batch runs a list of
    operations beneath one directory in a single hop and answers each one
    separately.

    Every path is relative to the batch's directory and resolved one component
    at a time with [openat(O_DIRECTORY | O_NOFOLLOW)], as [eio_posix] resolves a
    path where the kernel has no [RESOLVE_BENEATH]. The confinement is stricter
    than {!Eio.Path}'s: an absolute path, an empty, [.] or [..] component, a NUL
    byte, or a symbolic link before the last component is refused, never
    followed, so no operation reaches outside the directory. The last component
    is never followed either. The directories a run opens are held for the run
    only. *)
module Batch : sig
  type t
  (** The type for a directory that batches run beneath. *)

  val v : sw:Eio.Switch.t -> _ Eio.Path.t -> t
  (** [v ~sw dir] opens [dir], resolved by {!Eio.Path}'s own confinement, and
      holds it until [sw] finishes. A wrapper such as {!Eio_fs.Chaos} observes
      the open and the batch then runs beneath the directory it wraps.

      @raise Eio.Io when [dir] cannot be opened as a directory.
      @raise Invalid_argument if the backend provides no POSIX descriptor. *)

  type op =
    | Mkdir of { path : string; perm : int }
        (** [mkdirat(2)] with [perm], subject to the umask. *)
    | Create of { path : string; perm : int; contents : string }
        (** [openat(2)] with [O_CREAT | O_EXCL | O_NOFOLLOW], every byte of
            [contents] written, then [fchmod(2)] to [perm], so the umask does
            not apply. A failed write leaves the file holding what was written.
        *)
    | Symlink of { path : string; target : string }
        (** [symlinkat(2)]; [target] is stored as given and never resolved. *)
    | Unlink of { path : string; dir : bool }
        (** [unlinkat(2)], with [AT_REMOVEDIR] iff [dir]. *)
    | Rename of { src : string; dst : string }
        (** [renameat(2)]; both paths are confined. *)
    | Link of { src : string; dst : string }
        (** [linkat(2)] making [dst] a second name for [src]; both paths are
            confined, and a symbolic link at [src] is linked itself, never
            followed. *)
    | Lstat of string  (** [fstatat(2)] with [AT_SYMLINK_NOFOLLOW]. *)

  type outcome = Done | Stat of Eio.File.Stat.t

  type error =
    [ `Refused of string
      (** The path names nothing beneath the directory; the string says why. *)
    | `Unix of Unix.error * string * string
      (** The system call failed: its error, its name and its argument. *) ]

  val pp_error : error Fmt.t
  (** [pp_error] formats an error for logs and test failures. *)

  val run : t -> op list -> (outcome, error) result list
  (** [run t ops] performs [ops] in order, in one system-thread hop, and is
      their results in the same order. An operation that fails does not stop the
      ones after it.

      @raise Invalid_argument if [t]'s switch has finished. *)

  val hops : unit -> int
  (** [hops ()] is how many system-thread hops this module has made since the
      program started. *)
end

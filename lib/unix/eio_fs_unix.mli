(** POSIX filesystem calls that Eio and [Unix] do not bind.

    [Eio_fs_unix] is for a program that links unix anyway, beside the portable
    machinery of {!Eio_fs_sync}. It holds the {{!barriers}barriers} at full
    platform strength (on Darwin {!full_fsync} reaches the platter where [fsync]
    stops at the drive's cache), a filesystem's {{!capacity}capacity},
    {{!clones}copy-on-write clones}, {{!trees}tree removal} in batches,
    {{!files}positioned reads and writes}, the {{!sparse}sparse map},
    {{!rename}exclusive rename}, POSIX {{!record_locks}record locks},
    {{!lock_files}lock files} and their leases,
    {{!val-xattrs}extended attributes}, {{!links}links} and {{!batches}batches}
    confined beneath a directory, and a root whose calls run
    {{!inline}on the calling thread}.

    A call that takes an [Eio.Path.t] or an [Eio.File.t] reports a failure as
    [Eio.Io], as Eio's own backends do; one that takes a [Unix.file_descr]
    raises [Unix.Unix_error], as [Unix] does. *)

(** {1:barriers Barriers} *)

val full_fsync : Unix.file_descr -> unit
(** [full_fsync fd] pushes [fd]'s data through to stable storage. On Darwin it
    is [fcntl F_FULLFSYNC], falling back to [fsync] on a filesystem that refuses
    it (SMB, exFAT); elsewhere it is [fsync].

    @raise Unix.Unix_error as [fsync]. *)

val sync_dir : _ Eio.Path.t -> unit
(** [sync_dir dir] makes [dir]'s entries durable: the barrier a commit protocol
    issues after renaming a file into place. A backend that registers a
    {!Eio_fs_sync.Dir_sync} answers through it, asked for a full barrier, so
    [Eio_fs.Chaos.Crash] under [~os:`Macos] keeps what it commits; a plain POSIX
    directory is opened and {!full_fsync}ed on the descriptor that open returns,
    which resolves every component beneath [dir]'s capability, off the calling
    fiber's thread. A directory whose handle carries no descriptor takes
    {!Eio_fs_sync.sync_dir}, and raises as it does.

    @raise Eio.Io
      if [dir] cannot be opened for its barrier, as [fsync] fails, or as a
      registered barrier raises. *)

val sync_file : _ Eio.File.rw -> unit
(** [sync_file file] makes [file]'s data durable: the barrier a commit protocol
    issues over the bytes it wrote. A backend that registers a
    {!Eio_fs_sync.File_sync} answers through it, asked for a full barrier and
    before any descriptor [file] carries, so a model of power loss observes the
    barrier; a plain POSIX file is {!full_fsync}ed on its descriptor, off the
    calling fiber's thread. A file with neither takes {!Eio_fs_sync.sync_file},
    and raises as it does.

    @raise Eio.Io as [fsync] fails, or as a registered barrier raises. *)

val barrier : Eio_fs_sync.Barrier.t
(** [barrier] is {!sync_file} and {!sync_dir} as the barriers a publication of
    {!Eio_fs_sync} issues: pass it as [~barrier] for a publication that is
    durable on Darwin through the drive's cache. A file open for reading only is
    synced through its descriptor too. *)

(** {1:capacity Capacity} *)

type statvfs = {
  block_size : int64;  (** The fragment size in bytes ([f_frsize]). *)
  total_blocks : int64;  (** The filesystem's size in blocks ([f_blocks]). *)
  free_blocks : int64;  (** The free blocks ([f_bfree]). *)
  available_blocks : int64;
      (** The free blocks an unprivileged process can use ([f_bavail]). *)
}
(** The type for filesystem capacities, as POSIX [statvfs] reports them.
    [free_blocks] and [available_blocks] differ by the reserve kept for the
    privileged user, so a caller needs both to say whether a filesystem is full
    from an unprivileged writer's position. *)

val statvfs : _ Eio.Path.t -> statvfs
(** [statvfs path] reports the fragment size, total blocks, free blocks and
    blocks available to an unprivileged process on the filesystem holding
    [path]. The call is made on a system thread.

    [path] is reached through its directory, never by its native name: the
    directory holding [path] is opened as {!Eio.Path.open_dir} opens it, so a
    confined directory refuses a [..] above it, an absolute path and a symbolic
    link leading out, and [path]'s last component is opened beneath it without
    being followed ([fstatvfs] on it). A final symbolic link reports the
    filesystem holding the link. The empty path names the directory itself.

    @raise Eio.Io
      as POSIX [fstatvfs] fails, with {!Eio.Fs.Permission_denied} if [path]
      leads outside its directory, and with {!Eio.Fs.Not_native} if the
      directory has no host descriptor.
    @raise Stdlib.Invalid_argument if [path] holds a NUL byte. *)

(** {1:clones Copy-on-write clones} *)

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
    system thread; a clone of a directory is one call whatever it holds. As for
    {!val-statvfs}, [src] and [dst] are each reached through their directory and
    never lead outside it ([clonefileat]); a final symbolic link [src] is cloned
    as a link ([CLONE_NOFOLLOW]).

    @raise Eio.Io
      as [clonefileat] fails: with {!Eio.Fs.Already_exists} if [dst] exists,
      {!Eio.Fs.Not_found} if [src] does not, {!Eio.Fs.Permission_denied} if
      permission is denied or a path leads outside its directory, and
      {!Eio.Fs.Not_native} if a directory has no host descriptor.
    @raise Stdlib.Invalid_argument if a path holds a NUL byte. *)

(** {1:trees Tree removal} *)

type removed = {
  entries : int;  (** The files, links and directories removed. *)
  bytes : int;  (** What the non-directories removed weighed. *)
  refused : string list;
      (** Each path the removal refused, with the system's reason, in the order
          they were met; [[]] when nothing is left at the path. *)
}
(** The type for what {!remove_tree} did. *)

val remove_tree : ?grant:bool -> ?last:string -> _ Eio.Path.t -> removed
(** [remove_tree path] removes [path] and, when it is a directory, everything
    under it, deepest first, each entry reached through [path]'s own directory
    capability ({!Batch}): a symbolic link is removed and never followed. It
    costs a stat and an unlink per entry and a listing per directory, in hops
    per directory: a directory's entries are stated in one and unlinked in the
    next, whatever they number, and the hops of different directories run at
    once, four at a time. A cancellation ends it at the next hop, so none
    outlasts one directory's entries.

    With [~grant:true] (defaults to [false]) each directory is granted its
    owner's read, write and search before it is read, as
    {!Eio_fs.Path.grant_owner} grants it. The entry [last] of [path], if any, is
    removed only once every other entry is gone, so a removal that fails leaves
    it. An entry another process removes meanwhile counts nothing, and a [path]
    that does not exist removes nothing. A refusal stops no other directory's
    removal, leaves the directories above it and [last], and a later call goes
    on with what is left. *)

(** {1:files Positioned reads and writes}

    A program that reads and writes a file at offsets it names, as a database
    reads its pages, makes one call per page. {!Eio.File.pread} reads into a
    {!Cstruct.t} and may hand the call to a system thread; {!val-pread} and
    {!val-pwrite} read into [bytes] and write from a [string] on the calling
    thread, in one system call. A program that holds a host descriptor opens it
    as a file with {!Eio_unix.File.import_rw}. *)

val pread :
  ?cached:bool ->
  _ Eio.File.ro ->
  file_offset:int ->
  bytes ->
  pos:int ->
  len:int ->
  int
(** [pread ~cached file ~file_offset buf ~pos ~len] reads up to [len] bytes of
    [file] from byte [file_offset] into [buf] at [pos], and is the count read,
    [0] at the end of the file. It reads at most 65536 bytes, and [file]'s own
    offset does not move:
    - a file that registers {!Positioned} answers it;
    - a file over a host descriptor is read by POSIX [pread], which [Unix] does
      not bind, retried on [EINTR];
    - any other file is read by one {!Eio.File.pread}.

    On a host descriptor, other threads run during the call. With [cached]
    (defaults to [false]) the read is one the page cache answers, such as a
    database page read again: it keeps the OCaml runtime lock and lands in [buf]
    with no copy, which costs less than giving the lock up and taking it back.
    On Linux it asks with [RWF_NOWAIT] and gives the lock up only for a read
    that would wait on the device; elsewhere it keeps the lock whatever the read
    waits on, so no other thread of the domain runs, and no other domain
    completes a minor collection, until it returns.

    @raise Stdlib.Invalid_argument
      if [file_offset], [pos] or [len] is negative or [pos + len] is past the
      end of [buf].
    @raise Eio.Io as the read fails. *)

val pwrite :
  _ Eio.File.rw -> file_offset:int -> string -> pos:int -> len:int -> int
(** [pwrite file ~file_offset s ~pos ~len] writes up to [len] bytes of [s] from
    [pos] to [file] at byte [file_offset], and is the count written. It writes
    at most 65536 bytes, [file]'s own offset does not move, and [file] answers
    it as it answers {!val-pread}: through {!Positioned}, by POSIX [pwrite] on a
    host descriptor, retried on [EINTR] while other threads run, or by one
    {!Eio.File.pwrite_single}.

    @raise Stdlib.Invalid_argument
      if [file_offset], [pos] or [len] is negative or [pos + len] is past the
      end of [s].
    @raise Eio.Io as the write fails. *)

(** The positioned calls of a file that answers them itself. *)
module type POSITIONED = sig
  type t
  (** The type for the files. *)

  val pread :
    t -> cached:bool -> file_offset:int -> bytes -> pos:int -> len:int -> int
  (** [pread t ~cached ~file_offset buf ~pos ~len] is {!val-pread} on [t]. *)

  val pwrite : t -> file_offset:int -> string -> pos:int -> len:int -> int
  (** [pwrite t ~file_offset s ~pos ~len] is {!val-pwrite} on [t]. *)
end

type (_, _, _) Eio.Resource.pi +=
  | Positioned :
      ( 't,
        (module POSITIONED with type t = 't),
        [> `Positioned ] )
      Eio.Resource.pi
        (** The positioned calls a file registers in its resource handler when
            it answers them itself: a wrapper that carries the descriptor it
            stands in front of, to count, observe or alter its calls, registers
            it, and {!val-pread} and {!val-pwrite} then go through the wrapper.
            The wrapper reaches its base with {!val-pread} and {!val-pwrite}
            too. *)

(** {1:sparse The sparse map} *)

val seek_data : Unix.file_descr -> file_offset:int -> int option
(** [seek_data fd ~file_offset] is the next offset at or after [file_offset]
    that starts allocated data, and [None] when there is none: a hole runs to
    the end of the file, or [file_offset] is at or past it. This is
    [lseek(SEEK_DATA)], which [Unix.lseek] has no whence for. The offset is
    absolute and the call moves [fd]'s position as [lseek] does. The call is
    made on a system thread.

    @raise Unix.Unix_error as [lseek]. *)

val seek_hole : Unix.file_descr -> file_offset:int -> int option
(** [seek_hole fd ~file_offset] is like {!seek_data} for the next offset at or
    after [file_offset] that starts a hole. The end of the file counts as one,
    so this is [None] only when [file_offset] is at or past the end. This is
    [lseek(SEEK_HOLE)].

    @raise Unix.Unix_error as [lseek]. *)

type extent = {
  logical : int;  (** The extent's offset in the file. *)
  physical : int;  (** The extent's offset on the device. *)
  length : int;  (** The extent's length in bytes. *)
}
(** The type for the physical pieces a file's bytes are stored in: [length]
    bytes of the file starting at [logical] live at [physical] on the device.
    {!seek_data} and {!seek_hole} give the file's logical map; this gives its
    physical one. *)

val extents :
  Unix.file_descr -> file_offset:int -> length:int -> max:int -> extent list
(** [extents fd ~file_offset ~length ~max] is the physical extents backing the
    [length] bytes of [fd]'s file from byte [file_offset], in increasing logical
    order, at most [max] of them. This is [F_LOG2PHYS_EXT] on Darwin and
    [FS_IOC_FIEMAP] on Linux, neither of which [Unix] binds.

    The answer stops at the first hole, the end of the range, [max], or bytes
    the filesystem accepted but has not placed, or placed with no block address
    of their own (inline, unaligned, encoded or encrypted data). An empty range
    has no extent. A walk steps past the last extent returned and asks again,
    ending when the answer is empty. [max] is the batch, so one system-thread
    hop covers many extents.

    @raise Stdlib.Invalid_argument
      if [max] is not between 1 and 65536, [file_offset] or [length] is
      negative, or [file_offset + length] is past [max_int].
    @raise Unix.Unix_error
      as the platform call, and [ENOSYS] on a platform that has neither. *)

(** {1:rename Exclusive rename} *)

val exclusive_rename : Eio_fs_sync.Rename.t
(** [exclusive_rename] is the POSIX backend for atomic no-replace publication.
    Directory wrappers are consulted before resolving the source and target
    parents through their capabilities. Linux uses
    [renameat2(RENAME_NOREPLACE)], Darwin uses [renameatx_np(RENAME_EXCL)];
    unsupported platforms or filesystems refuse. An existing target, including a
    symbolic link, is never overwritten.

    Through {!Eio_fs_sync.Rename.exclusive}, it raises [Eio.Io] with
    {!Eio.Fs.Already_exists} for an existing target, [Eio.Io] as the platform
    call fails otherwise (an unsupported platform or filesystem included), and
    [Invalid_argument] for a path that ends in no file name or whose directory
    carries no Unix descriptor. *)

(** {1:record_locks Record locks} *)

val lock_range :
  Unix.file_descr ->
  [ `Read | `Write | `Unlock ] ->
  file_offset:int ->
  length:int ->
  unit
(** [lock_range fd kind ~file_offset ~length] sets a POSIX record lock of [kind]
    on the [length] bytes of [fd]'s file from byte [file_offset], without
    waiting (fcntl [F_SETLK]), placed from the start of the file. [fd]'s file
    offset does not move, where {!Unix.lockf} places its range at the offset and
    so has to be preceded by a seek. A lock this process holds there already is
    replaced.

    @raise Unix.Unix_error
      with [EAGAIN] or [EACCES] where another process holds a lock that
      conflicts. *)

val range_holder :
  Unix.file_descr ->
  file_offset:int ->
  length:int ->
  [ `Free | `Held of int option ]
(** [range_holder fd ~file_offset ~length] is the record lock another process
    holds that would refuse a write lock on the [length] bytes of [fd]'s file
    from byte [file_offset] (fcntl [F_GETLK]), in the shape {!probe} answers:
    [`Held (Some pid)] where process [pid] holds it, [`Held None] where the
    kernel names no process, and [`Free] where none is held. This process's own
    locks never count. *)

val await_record_lock :
  ?mode:[ `Shared | `Exclusive ] -> Unix.file_descr -> unit
(** [await_record_lock ~mode fd] takes the POSIX record lock over [fd], from its
    offset to the end of the file as [lockf] does, by the kernel's blocking
    request ([fcntl] [F_SETLKW]), and returns once the kernel grants it. [mode]
    is [`Exclusive] (a write lock, the default) or [`Shared] (a read lock). The
    request waits on a thread of its own and the calling fiber waits for its
    answer, so the domain goes on running other fibers; a cancel withdraws the
    request, and a lock granted in that instant is let go.

    It is the wait under {!with_lock}, for a caller that keeps its own
    descriptors: the record lock belongs to the process, and closing any
    descriptor on the file drops it.

    @raise Unix.Unix_error as [fcntl] does. *)

(** {1:lock_files Lock files} *)

type block =
  [ `Yes  (** Wait for the holder. *)
  | `No  (** Refuse with {!Lock_unavailable}. *)
  | `Fibers
    (** Wait for a fiber of this process; refuse another process's hold. *)
  | `For of float
    (** Wait up to that many seconds of the monotonic clock, then refuse. *) ]
(** The type for what a take does when another caller holds the lock. *)

type create =
  [ `Never  (** Refuse an absent file with {!Eio.Fs.Not_found}. *)
  | `If_missing of Eio.File.Unix_perm.t
    (** Create an absent file with that mode, less the umask. *) ]
(** The type for what a take does when the lock file is absent. *)

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
(** Raised by a take, such as {!with_lock}, that another holder refuses as its
    {!type-block} policy says. It prints as {!pp_refusal} does. *)

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
  ?block:block ->
  ?create:create ->
  ?contents:string ->
  ?on_wait:(unit -> unit) ->
  _ Eio.Path.t ->
  (unit -> 'a) ->
  'a
(** [with_lock path fn] opens (or creates) [path], takes an exclusive advisory
    lock, and runs [fn ()] while holding it. Waiting for the operating-system
    lock suspends only the calling fiber. The lock is released when [fn] returns
    or raises.

    [create] (defaults to [`If_missing 0o600], owner-only) says what an absent
    rendezvous does. [contents], when present, replaces the lock file's contents
    through the locked handle. [block] (defaults to [`Yes]) says what a hold
    another caller already has does. Locks are coordinated between fibers by
    device and inode, and between processes by the POSIX record lock. It also
    excludes {!Lease} readers and writers. On Linux, where BSD [flock] and POSIX
    record locks are independent, it takes an exclusive [flock] too; on Darwin
    the record lock already conflicts with leases.

    With [`Yes] or [`For seconds], a POSIX record lock another process holds is
    waited for by the kernel's blocking request ([fcntl] [F_SETLKW]), which
    answers when that process lets go: the lock is asked for once without
    waiting, [on_wait ()] is called once if it is held (it does nothing by
    default), and the request follows. A cancelled wait withdraws the request
    and answers at the cancel, and [`For seconds] withdraws it at its bound.
    With [`Yes], [`Fibers] or [`For seconds], a lock a fiber of this process
    holds is waited for in this process, and [on_wait ()] is called before that
    wait too. A wait for a BSD lease on Linux polls as {!Lease.with_exclusive}'s
    does.

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
    @raise Eio.Io as [lockf] or [flock] fails otherwise.
    @raise Stdlib.Invalid_argument
      if the opened rendezvous carries no Unix descriptor and its backend is not
      {!Eio_fs_sync.Process_local}. *)

val probe : _ Eio.Path.t -> [ `Free of string | `Held of refusal ]
(** [probe path] is the lock over [path] as an acquisition would find it now,
    taken without acquiring it: [`Held r] names its holder as a refusal does,
    and [`Free record] carries the lock file's first 4096 bytes, what its last
    holder left there. The file is opened for reading only, and a descriptor on
    a lock this process holds is never closed while it is held, so a probe from
    the holding process keeps the hold.

    [path] must be the lock's own regular file, as {!with_lock}'s must. A name
    given to another regular file while the probe reads it is probed again, for
    the file it holds then.

    @raise Eio.Io
      with {!Eio.Fs.Not_found} when [path] is absent, or goes while the probe
      reads it.
    @raise Eio.Io
      with {!Eio.Fs.Permission_denied} carrying {!Not_a_lock_file} when [path]
      is not the lock's own regular file. *)

(** A lock held for as long as its holder says, restated as it goes. *)
module Held : sig
  type t
  (** The type for a lock this process holds. *)

  val take :
    sw:Eio.Switch.t ->
    ?block:block ->
    ?create:create ->
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
      @raise Stdlib.Invalid_argument if [h] has been released. *)

  val release : ?contents:string -> t -> unit
  (** [release ~contents h] writes [contents], when present, through the held
      descriptor and lets the lock go, returning once it is free. Releasing a
      released lock, or one whose switch has ended, does nothing. *)
end

(** Leases: BSD [flock] locks, which belong to an open file description rather
    than to the process that took them. *)
module Lease : sig
  type t
  (** The type for leases this process holds. *)

  val fd : t -> Eio_unix.Fd.t
  (** [fd lease] is the descriptor whose open file description owns the lease. A
      supervisor can inherit it before the callback returns. Closing the
      parent's descriptor does not release the lease while an inherited
      descriptor remains open. The descriptor must be closed, or the lease
      {!unlock}ed, to release it. *)

  val unlock : t -> unit
  (** [unlock lease] lets go of [lease] by the kernel's [flock] [LOCK_UN] on its
      open file description, so every descriptor sharing that description, one a
      supervisor inherited included, holds nothing from then on. The descriptors
      stay open. An exclusive callback still returns to let go of its
      process-local admission and its gate; a shared acquisition gives that
      admission back immediately. *)

  val take :
    sw:Eio.Switch.t ->
    ?gate:_ Eio.Path.t ->
    ?block:block ->
    ?create:create ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    t
  (** [take ~sw path] acquires the exclusive lease described by
      {!with_exclusive}. Its descriptor, process-local admission and gate remain
      held until {!release} or [sw]'s release hooks run. Acquisition has the
      same defaults, cancellation and refusals as {!with_exclusive}, and raises
      as it does. *)

  val take_after_readers :
    sw:Eio.Switch.t ->
    gate:_ Eio.Path.t ->
    ?create:create ->
    ?contents:string ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    t
  (** [take_after_readers ~sw ~gate path] is [take ~sw ~gate path] that waits
      for [path]'s shared holders alone: a lease only shared holders hold is
      waited for as [`Yes] waits, and an exclusive holder, or another take
      through [gate] under way, refuses it as [`No] does. A reader's look, a
      shared lease taken and let go at once, delays the take and never refuses
      it. [gate] is tried without waiting and held throughout the wait, so no
      exclusive take comes in between this take's look at the holders and its
      own; the wait is sound only if every exclusive take of [path] passes
      [gate]. [create], [contents] and [on_wait] are {!with_exclusive}'s.

      @raise Lock_unavailable
        when an exclusive holder or another take through [gate] refuses it.
      @raise Eio.Io as {!with_exclusive} raises it.
      @raise Stdlib.Invalid_argument
        when the filesystem provides no Unix descriptor. *)

  val release : t -> unit
  (** [release lease] closes this holder's descriptor and gives back its
      process-local admission, then lets its gate go. Repeated calls do nothing.
      An inherited copy of the descriptor keeps its kernel lease until its last
      copy closes; use {!unlock} to release the shared open file description
      immediately. *)

  val with_exclusive :
    ?gate:_ Eio.Path.t ->
    ?block:block ->
    ?create:create ->
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

      [create] (defaults to [`If_missing 0o600], owner-only) says what an absent
      rendezvous does. [contents], when present, replaces the rendezvous
      contents while the lease is held. [block] (defaults to [`Yes]) says what a
      hold another caller already has does.

      [gate], when given, is a lease on a file of its own that every exclusive
      take of [path] passes: the take holds it exclusively before it takes
      [path], and for as long as this descriptor holds [path], until {!release},
      the callback's return or [sw]'s end, so no take of [path] is made between
      another's look at its holders and its take, as {!take_after_readers}
      needs. A held gate is waited for or refused as [block] says of [path], and
      the refusal is that of [path]'s holder when an exclusive one holds it,
      else that of the take under way, whose [contents] (or nothing) the gate's
      file holds. The file is created when absent, with [create]'s permissions
      or [0o600]; its directory must exist. A take that finds [path] absent with
      [create] [`Never] removes the gate it holds. A caller that removes [path]
      holding it removes [gate] first, holding it too: a take that waited on a
      removed gate takes the file its name holds then.

      With [block] [`Yes] or [`For seconds], a lease another process holds is
      waited for by polling: the lease is tried without waiting, [on_wait ()] is
      called once if it is held (it does nothing by default), and it is tried
      again after a sleep that starts at 1 ms and doubles up to 50 ms, until it
      is granted or [`For seconds] reaches its bound. A waiter takes no place in
      the kernel's queue, so waiters are not served in the order they arrived,
      and a release is noticed up to 50 ms late. With [`Yes], [`Fibers] or
      [`For seconds], a lease a fiber of this process holds is waited for in
      this process, and [on_wait ()] is called before that wait too. A cancelled
      wait answers at the cancel and, like a wait that reached its bound, leaves
      no thread, descriptor or kernel request behind.

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
        path or the gate is not its own regular file.
      @raise Eio.Io as [flock] fails otherwise.
      @raise Stdlib.Invalid_argument
        when the filesystem provides no Unix descriptor. *)

  val with_shared :
    ?block:block ->
    ?create:create ->
    ?on_wait:(unit -> unit) ->
    _ Eio.Path.t ->
    (t -> 'a) ->
    'a
  (** [with_shared path fn] acquires a shared BSD [flock] on an independent open
      file description. Readers may overlap, including fibers of this process.
      An exclusive lease or {!with_lock} excludes these readers. The creation,
      wait, cancellation and path checks are those of {!with_exclusive}; readers
      take no gate and do not replace the rendezvous contents.

      Returning closes the callback's descriptor without unlocking. An inherited
      descriptor keeps the hold until its last copy closes. {!unlock} releases
      this reader's hold and its process-local admission immediately, without
      releasing independently acquired readers.

      It raises as {!with_exclusive} does. *)
end

val lock_section :
  ?block:block ->
  ?create:create ->
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
    [Unix] does not bind. As for {!val-statvfs}, [path] is reached through its
    directory and never leads outside it. Both calls act on the entry [path]
    names, a symbolic link included, and never follow it: Darwin opens the entry
    ([O_SYMLINK]) and calls [flistxattr], [fgetxattr] and [fsetxattr] on it, so
    the entry must be readable; Linux calls [llistxattr], [lgetxattr] and
    [lsetxattr] on the entry through its directory's [/proc/self/fd] link, and
    fails with [ENOTSUP] where [/proc] is not mounted. Each call is made on a
    system thread. *)

val xattrs : _ Eio.Path.t -> (string * string) list
(** [xattrs path] is every extended attribute of [path] as [(name, value)]
    pairs, in ascending byte order of name, and [[]] for an entry that has none.
    A value is the attribute's bytes, NUL bytes included. An attribute removed
    between the listing and the read is left out.

    @raise Eio.Io
      as the platform call fails ([ENOTSUP] where the filesystem keeps none,
      [ENOSYS] on a platform with neither family), with
      {!Eio.Fs.Permission_denied} if [path] leads outside its directory, and
      with {!Eio.Fs.Not_native} if the directory has no host descriptor.
    @raise Stdlib.Invalid_argument if [path] holds a NUL byte. *)

val set_xattr : _ Eio.Path.t -> string -> string -> unit
(** [set_xattr path name value] sets the extended attribute [name] of [path] to
    [value], creating it or replacing it.

    @raise Eio.Io
      as the platform call fails ([ENOTSUP] where the filesystem keeps none,
      [EPERM] for a namespace the process may not write, [security.*] and
      [trusted.*] without privilege on Linux, [ENOSYS] on a platform with
      neither family), with {!Eio.Fs.Permission_denied} if [path] leads outside
      its directory, and with {!Eio.Fs.Not_native} if the directory has no host
      descriptor.
    @raise Stdlib.Invalid_argument if [path] or [name] holds a NUL byte. *)

(** {1:links Confined links} *)

type Eio.Exn.Backend.t +=
  | Unsafe_parent of string
        (** Why {!Link.with_parent} or {!Batch.with_parent} refused a path: it
            names nothing beneath its directory, or a parent on the way is not
            one it may descend. *)

(** Symbolic links published beneath a parent directory held open, each
    component opened with [O_NOFOLLOW], so no link on the way can redirect the
    publication. *)
module Link : sig
  type t
  (** The type for a held parent directory and the final component of a path
      beneath it, as {!val-with_parent} passes them. *)

  (** The type for what {!publish} found. *)
  type publication =
    | Created  (** The link was made. *)
    | Present  (** A link holding exactly the target was already there. *)

  val with_parent :
    ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
    _ Eio.Path.t ->
    (t -> 'a) ->
    'a
  (** [with_parent ~create path fn] opens each parent component of [path]
      beneath its directory with [O_DIRECTORY | O_NOFOLLOW], then is [fn]
      applied to the held parent and final basename. [path]'s name must be
      non-empty, canonical and relative, with no empty, dot, dot-dot or NUL
      components. [create] (defaults to [`Never]) says what a missing parent
      does: [`If_missing mode] creates it with [mode] less the umask, and
      [`Never] refuses it. Created parents survive a later refusal.

      The handle is closed when [fn] returns and must not escape its scope. A
      renamed parent remains the held directory; this does not promise that its
      original textual path still names it. Where [fn]'s result or exception
      comes from [fn], it passes through unchanged.

      @raise Eio.Io
        with {!Eio.Fs.Permission_denied} carrying {!Unsafe_parent} if [path]'s
        name is refused, and as [openat] or [mkdirat] fails on a parent
        otherwise ({!Eio.Fs.Not_found} for a missing one, {!Eio.Fs.Symlink} for
        a symbolic link); [fn] is then not called.
      @raise Stdlib.Invalid_argument
        if [path]'s directory carries no Unix descriptor. *)

  val publish : t -> target:string -> (publication, string) result
  (** [publish parent ~target] atomically creates an absent symbolic link
      holding [target] using [symlinkat], and is [Ok Created]. An existing link
      holding exactly [target] is [Ok Present]; no filesystem immutability is
      implied. Every other existing entry, an empty [target], one holding a NUL
      byte and a failed call are [Error msg], and nothing is replaced; [msg]
      names the original path and the action needed to retry. *)

  type regular = { contents : string; perm : Eio.File.Unix_perm.t }
  (** The type for a regular file read: its bytes and its permission bits, both
      from the one descriptor. *)

  val read_regular : max_bytes:int -> t -> (regular option, string) result
  (** [read_regular ~max_bytes parent] opens the final entry beneath the held
      parent with [O_NOFOLLOW | O_NONBLOCK | O_NOCTTY] and is [Ok (Some r)], [r]
      the bytes read from that descriptor and its permission bits by [fstat], or
      [Ok None] if the entry is absent. It is [Error msg] for a symbolic link or
      another non-regular entry (a FIFO is refused without waiting for a writer,
      and a terminal never becomes the controlling one), a file of more than
      [max_bytes] bytes or one that grows beyond its size at open, and a failed
      open. The descriptor is closed before the result. The enclosing
      {!val-with_parent} refuses symbolic links at every parent component.

      {b Warning.} A hard link is the file it names, wherever its other names
      are: a regular file linked beneath the parent from outside the root is
      read. Only a process that can write both directories makes such a link.

      @raise Eio.Io if the opened entry cannot be stated or read.
      @raise Stdlib.Invalid_argument
        if [max_bytes] is negative or [max_int], as {!Eio_fs.Nofollow.run}
        refuses it. *)
end

(** {1:batches Batched directory operations} *)

(** Confined directory operations, many to one system-thread hop.

    {!Eio.Path} makes one system-thread hop per operation, which dominates the
    cost of writing a large tree of small files. A batch runs a list of
    operations beneath one directory in a single hop and answers each one
    separately.

    Every path is relative to the batch's directory. Native directories resolve
    one component at a time with [openat(O_DIRECTORY | O_NOFOLLOW)], as
    [eio_posix] resolves a path where the kernel has no [RESOLVE_BENEATH]. The
    confinement is stricter than {!Eio.Path}'s: an absolute path, an empty, [.]
    or [..] component, a NUL byte, or a symbolic link before the last component
    is refused, never followed, so no operation reaches outside the directory.
    Descriptorless backends hold each Eio parent capability and verify its
    identity before descending. The last component is never followed either. The
    directories a run opens are held for the run only. *)
module Batch : sig
  val with_parent : _ Eio.Path.t -> (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
  (** [with_parent path fn] calls [fn] with the final leaf of [path] beneath
      borrowed non-symlink parent capabilities. The observed and opened
      directory identities must agree. Renaming a held parent cannot redirect
      the leaf. The callback's result or exception passes through unchanged.
      Borrowed parents are released on either exit; the caller's root remains
      usable. A final symbolic link remains the final leaf. The leaf must not be
      used after [fn] returns.

      @raise Eio.Io
        with {!Eio.Fs.Permission_denied} carrying {!Unsafe_parent} for invalid
        path components, a non-directory parent or an identity mismatch; with
        {!Eio.Fs.Not_found} for a missing parent. *)

  type t
  (** The type for a directory that batches run beneath. *)

  val v : sw:Eio.Switch.t -> _ Eio.Path.t -> t
  (** [v ~sw dir] opens [dir], resolved by {!Eio.Path}'s own confinement, and
      holds it until [sw] finishes. A wrapper such as {!Eio_fs.Chaos} observes
      the open and the batch then runs beneath the directory it wraps. An
      already-open directory is held through its dot entry, without an empty
      open pathname. Descriptorless backends use Eio operations through held
      parent capabilities.

      @raise Eio.Io when [dir] cannot be opened as a directory. *)

  (** The type for batch operations, each named after the system call that
      performs it beneath a native directory. *)
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
            followed. Descriptorless backends refuse hard links. *)
    | Lstat of string  (** [fstatat(2)] with [AT_SYMLINK_NOFOLLOW]. *)

  (** The type for the results of operations that succeeded. *)
  type outcome =
    | Done  (** Every operation but {!Lstat} answers this. *)
    | Stat of Eio.File.Stat.t  (** What {!Lstat} answers. *)

  type error =
    [ `Refused of string
      (** The path names nothing beneath the directory; the string says why. *)
    | `Unix of Unix.error * string * string
      (** The system call failed: its error, its name and its argument. *) ]
  (** The type for the errors of operations that failed. *)

  val pp_error : error Fmt.t
  (** [pp_error] formats an error for logs and test failures. *)

  val run : t -> op list -> (outcome, error) result list
  (** [run t ops] performs [ops] in order, in one system-thread hop, and is
      their results in the same order. An operation that fails does not stop the
      ones after it. An empty [ops] is [[]] and makes no hop.

      @raise Stdlib.Invalid_argument if [t]'s switch has finished. *)
end

(** {1:inline Calls on the calling thread} *)

val inline_fs :
  < fs : Eio.Fs.dir_ty Eio.Path.t ; .. > -> Eio.Fs.dir_ty Eio.Path.t
(** [inline_fs env] is [env]'s unconfined root, [Eio.Stdenv.fs env], whose
    [stat], [open_in], [open_out], [read_dir] and [with_dir_entries] make their
    system calls on the calling thread, where eio_posix hands each to a system
    thread and back: a hand-off costs ten to forty microseconds of CPU, the call
    a few. A file it opens is the root's own kind of file, and every other
    operation, a directory opened from it included, is the root's. An error is
    the root's in what it says: the operation, the path, the kind of error and
    its errno; the system call it names is the one made here, which may not be
    the backend's. The calls block the calling domain while they run, which on a
    local disk is microseconds; a filesystem whose calls can block for longer,
    such as a network mount, is better reached through [env]'s own root. *)

(**/**)

(* For this package's tests only. *)
module Private : sig
  val batch_hops : unit -> int
  (** [batch_hops ()] is how many system-thread hops {!Batch} has made since the
      program started. *)
end

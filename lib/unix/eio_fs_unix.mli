(** The POSIX durability barrier at full platform strength, advisory file
    locking, and filesystem capacity.

    The portable machinery lives in {!Eio_fs_sync}: backends with their own
    barrier semantics register {!Eio_fs_sync.Dir_sync} or
    {!Eio_fs_sync.File_sync}, and a plain POSIX directory or file is fsynced
    through its own handle. This facade is for programs that link unix anyway
    and want the strongest barrier the platform offers: on Darwin {!full_fsync}
    (F_FULLFSYNC, through to the platter) where the portable route's fsync only
    reaches the drive's cache. Elsewhere the two are the same barrier. Both
    halves of a commit protocol's durability are here, {!sync_dir} for a
    directory's entries and {!sync_file} for a file's data.

    The other two operations are here for the reason the barrier's upgrade is:
    what they need is the host. {!with_lock} holds an exclusive advisory lock
    over a rendezvous file -- a POSIX record lock, taken between processes and
    between the fibers of this one, refusing anything that is not the lock's own
    regular file ({!Lock_unavailable}, {!Not_a_lock_file}). {!statvfs} reports
    the capacity of the filesystem a path is on, which is the number a cache
    watermark or a free-space check is computed from. {!seek_data}, {!seek_hole}
    and {!extents} report which bytes of a file are allocated and which physical
    pieces they are stored in, the questions [Unix] cannot ask about a sparse
    file. *)

val full_fsync : Unix.file_descr -> unit
(** [full_fsync fd] pushes [fd]'s data through to stable storage. On Darwin it
    is [fcntl F_FULLFSYNC] -- [fsync] there only flushes to the drive's cache --
    falling back to [fsync] on a filesystem that refuses it (SMB, exFAT), the
    strongest barrier the volume offers; elsewhere it is [fsync].

    @raise Unix.Unix_error as [fsync]. *)

type statvfs = {
  block_size : int64;
  total_blocks : int64;
  free_blocks : int64;
  available_blocks : int64;
}
(** Raw filesystem capacity returned by POSIX [statvfs]. Policy such as free
    percentages and cache watermarks belongs in the OCaml caller.

    {!field-free_blocks} ([f_bfree]) and {!field-available_blocks} ([f_bavail])
    are different numbers, and the difference is the reserve a filesystem keeps
    for the privileged user. Both are here because the percentage [df] prints
    needs both: used blocks are total minus free, and what an unprivileged
    writer can still claim is available. A filesystem with nothing but its
    reserve left is full from where that writer stands, and a caller holding
    only {!field-available_blocks} cannot say so -- it would report the reserve
    as headroom. The two are equal on a filesystem that keeps no reserve, APFS
    among them. *)

val statvfs : _ Eio.Path.t -> statvfs
(** [statvfs path] reports the fragment size, total blocks, free blocks and
    blocks available to an unprivileged process on the filesystem holding
    [path]. The call is made on a system thread, so it suspends only the calling
    fiber.

    @raise Unix.Unix_error as POSIX [statvfs].
    @raise Invalid_argument if [path] has no native Unix representation. *)

(** {2 The sparse map} *)

val seek_data : Unix.file_descr -> int64 -> int64 option
(** [seek_data fd offset] is the next offset at or after [offset] that starts
    allocated data, and [None] when there is none: a file whose tail is a hole
    answers so, and so does an offset at or past the end. This is
    [lseek(SEEK_DATA)], which [Unix.lseek] has no whence for.

    The offset is absolute and the call moves [fd]'s position as [lseek] does,
    so a walk that keeps its own state may read and write through the same
    descriptor between two queries without disturbing itself.

    The call is made on a system thread, so it suspends only the calling fiber.

    @raise Unix.Unix_error as [lseek]. *)

val seek_hole : Unix.file_descr -> int64 -> int64 option
(** [seek_hole fd offset] is {!val-seek_data}'s other half: the next offset at
    or after [offset] that starts a hole, and [None] when there is none. This is
    [lseek(SEEK_HOLE)].

    @raise Unix.Unix_error as [lseek]. *)

type extent = { logical : int64; physical : int64; length : int64 }
(** One physical piece a file's bytes are stored in: [length] bytes of the file
    starting at [logical] live at [physical] on the device.

    A file's LOGICAL map and its PHYSICAL map are not the same count, and the
    difference is what fragmentation means: a file written once, sequentially,
    is a handful of large extents, while the same bytes written over months at
    offsets an allocator picked can be in hundreds of thousands of pieces inside
    a couple of allocated ranges. {!seek_data} and {!seek_hole} give the first
    map; this gives the second. *)

val extents :
  Unix.file_descr -> offset:int64 -> length:int64 -> max:int -> extent list
(** [extents fd ~offset ~length ~max] is the physical extents backing
    [[offset], [offset + length])], in increasing logical order and contiguous
    in logical space, at most [max] of them. This is [F_LOG2PHYS_EXT] on Darwin
    and [FS_IOC_FIEMAP] on Linux, neither of which [Unix] binds and neither of
    which has a portable spelling.

    The answer stops at the first hole, at the end of the range, at [max], or at
    bytes the filesystem has accepted but not yet placed. A walk therefore steps
    its offset on past the last extent returned and asks again, and ends when
    the answer is empty -- which is the same termination a hole gives. Two
    answers adjacent both logically and physically are one extent this query
    happened to split; merging them is the caller's, and so is any ceiling on
    how far a walk goes.

    [max] IS THE BATCH, AND IT IS WHY THIS TAKES ONE. The call runs on a system
    thread so that walking a fragmented file does not hold the calling domain,
    and that hop is not free: measured 2026-09-07 on macOS/arm64, a round trip
    through [Eio_unix.run_in_systhread] costs 9.2 us against 0.31 us for the
    same [lseek] made directly, a factor of thirty. One hop per extent would
    therefore spend about 19 s of pure scheduling overhead over two million
    extents and buy nothing; one hop per thousand spends 19 ms and frees the
    domain for the rest. On Linux [max] is also the number of extents one
    [ioctl] returns, so the batch saves the syscalls too.

    @raise Invalid_argument
      if [max] is not between 1 and 65536, or if [offset] or [length] is
      negative. The upper bound is on the buffer the extents are collected in,
      24 bytes each.
    @raise Unix.Unix_error
      as the platform call, and with [ENOSYS] on a platform that has neither. *)

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

val sync_file : _ Eio.File.rw -> unit
(** [sync_file file] makes [file]'s data durable: the barrier a commit protocol
    issues over the bytes it wrote, before the rename that publishes them. A
    backend that registers its own barrier ({!Eio_fs_sync.File_sync}) answers
    through it -- a crash-model wrapper commits that file's data journal -- and
    a plain POSIX file is {!full_fsync}ed on the descriptor it carries, which on
    Darwin reaches the platter where [Eio.File.sync] stops at the drive's cache.

    The registration is asked before the descriptor, and that order is what the
    entry point is for. A descriptor a wrapper carries over is the operations
    the operating system offers on the open file, not a second way past the
    wrapper: a barrier taken on it is one the wrapper never saw, so a model of
    power loss records none and drops at the cut the bytes the caller was told
    had reached the platter. Reaching for the descriptor first therefore makes
    the barrier invisible to exactly the wrapper written to observe it, while
    leaving every real filesystem looking correct.

    A file that registers no barrier and carries no descriptor takes the
    portable one ({!Eio_fs_sync.sync_file}), which is the file's own sync.

    The barrier is made off the calling fiber's thread, so waiting for the
    platter suspends only that fiber and not the other fibers of its domain. A
    registered barrier answers on the calling fiber, as its backend wrote it.

    @raise Unix.Unix_error as [fsync]. *)

exception Lock_unavailable of string
(** Raised by {!with_lock} when the lock is held and [block] says not to wait
    for the holder. The string is the native path of the lock file, or the path
    as it prints itself on a backend that has no native one. *)

type Eio.Exn.Backend.t +=
  | Not_a_lock_file of string
        (** Why {!with_lock} refused a lock path: the name is not the lock's own
            regular file. The string is the native path of the lock file, or the
            path as it prints itself on a backend that has no native one. *)

val with_lock :
  ?block:[ `Yes | `No | `Fibers | `For of float ] ->
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

    [block] says what a lock another caller already holds does. [`Yes] (the
    default) waits for it. [`No] raises {!Lock_unavailable} rather than waiting.
    [`Fibers] waits for a fiber of this process and raises {!Lock_unavailable}
    for a hold taken by any other process: the fibers of one program are its own
    cooperating writers, so one that finds another inside wants its turn --
    refusing it fails work the program itself asked for and could have waited
    out -- while how long another process holds the rendezvous is not this
    program's to bound, and a command-line tool would rather name the busy
    resource to its operator than wait on a holder that may be wedged, with
    nothing printed and no deadline. [`For seconds] waits like [`Yes] but
    answers: once [seconds] have passed without the lock it raises
    {!Lock_unavailable}, so a caller that cannot go on without the lock can say
    what it is waiting for and how long it will wait, and act when the wait is
    over. The bound is one budget for the whole acquisition, the fibers of this
    process and the operating-system lock together, and it is measured on the
    scheduler's monotonic clock rather than on a wall clock that steps under a
    long wait. Since POSIX has no portable timed record lock -- [F_OFD_SETLKW]
    under a timer is Linux's and macOS has none -- the wait for another process
    asks again every fifty milliseconds, which is what a handover costs the
    waiter: the answer comes no sooner than the bound and no later than one
    further ask. A bound is not a place in a queue, nothing ordering the
    processes that contend, so a caller whose bound lapses lost no turn it was
    owed.

    Locks are coordinated both between fibers in this process and between
    processes. The lock is always released when [fn] returns or raises. Between
    fibers the lock is the file's, not the name's: two paths that name one file
    take one lock, the file being identified by the device and inode of the
    descriptor the lock is held on. The operating-system lock cannot stand in
    for that, POSIX record locks not excluding within a process.

    Fibers of this process are granted the lock in the order they entered
    [with_lock], so a caller may number the work it does under the lock by the
    order it was served. Entering queues on [path] as the caller spelled it,
    before the rendezvous is opened, because the open suspends and the exclusion
    it settles is the file's: without that queue the order fibers reach the lock
    in would be the order their opens came back in. Being the name's, the queue
    is the name's too -- two names for one file (a hard link, another spelling)
    take one lock but wait in two queues, and which of the two is served first
    is not ordered. An acquisition that answers rather than waits still waits
    its turn to enter, which is the length of another caller's open and not of
    any critical section, and then answers about the lock itself. Nothing orders
    the processes contending for the operating-system lock; that order is the
    platform's.

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

val lock_section :
  ?block:[ `Yes | `No | `Fibers | `For of float ] ->
  ?create:[ `Never | `If_missing of Eio.File.Unix_perm.t ] ->
  ?contents:string ->
  _ Eio.Path.t ->
  Eio_fs_sync.Lock.t
(** [lock_section path] is {!with_lock} over the rendezvous [path] as the
    critical section a component takes from its deployment
    ({!Eio_fs_sync.Lock.t}): the host answer to that argument, where the
    process-local one is a mutex the program's writers share.

    [block], [create] and [contents] are {!with_lock}'s and mean the same thing.
    [block] is worth naming: a component that reaches its section once per
    operation and can report a missed turn wants [`Fibers], which waits for a
    fiber of this program and answers about a hold another process took, rather
    than the default [`Yes] that waits for either.

    Every way {!with_lock} can refuse the rendezvous -- a hold [block] says not
    to wait for, a name that is not the lock's own regular file, an I/O failure
    reaching it, a backend with neither a descriptor nor
    {!Eio_fs_sync.Process_local} -- is raised as
    {!Eio_fs_sync.Lock.Unavailable}, naming what refused. That translation is
    the reason to build the section here rather than at the component: the
    component is backend-free by design, so this library's exceptions are ones
    it cannot name, and an exception it cannot catch would travel out through an
    interface whose type promises a result.

    What the section's own body raises is untouched and travels out as the body
    raised it. A failure under a lock the caller was granted is not the lock
    refusing, and reporting it as one would send an operator looking for a
    holder that is not there.

    @raise Eio_fs_sync.Lock.Unavailable
      from {!Eio_fs_sync.Lock.use}, never from [lock_section] itself: nothing is
      opened or locked until the section is entered. *)

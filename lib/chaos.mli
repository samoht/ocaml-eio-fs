(** An adversarial {!Eio.Fs.dir} wrapper for fault-injection testing.

    {!wrap} composes with any base directory capability -- the in-memory backend
    from {!Eio_fs.Fs} or a real filesystem -- and perturbs reads, writes, syncs,
    opens and stats according to a {!Schedule.t}. A schedule is either
    {e seeded} (deterministic pseudo-random faults, for fuzzing) or {e scripted}
    (an inspectable list of rules, for pinning an exact scenario such as a torn
    write during a superblock flush).

    All injected faults surface as ordinary {!Eio.Io} exceptions, so code under
    test cannot distinguish an injected failure from a real backend failure. *)

type code = [ `Enoent | `Eacces | `Enospc | `Eio ]
(** POSIX-style error codes an injected fault can surface. *)

(** The fault kinds the wrapper can inject. Byte counts on the short-read,
    short-write and torn-write faults are relative to the current operation's
    buffer. *)
module Fault : sig
  type kind =
    [ `Short_read
    | `Short_write
    | `Torn_write
    | `Write_error
    | `Sync_error
    | `Open_error
    | `Stat_error
    | `Swap ]
  (** The family a fault belongs to, used to enable/disable kinds in a seeded
      schedule. [`Swap] is scripted-only (it carries a concrete symlink target),
      so a seeded schedule never generates it. *)

  type t =
    | Short_read of int
        (** Deliver at most [n] bytes of the requested read ([n >= 1]), forcing
            the caller to loop. *)
    | Short_write of int
        (** Accept at most [n] bytes of the offered write ([n >= 1]), forcing
            the caller to loop. *)
    | Torn_write of int
        (** Commit [n] bytes to the base, then raise: bytes reach the store but
            the operation reports failure -- the durability hazard. *)
    | Write_error of code  (** Commit nothing, raise a write error (ENOSPC). *)
    | Sync_error of code  (** Fail a flush/sync. *)
    | Open_error of code  (** Fail an open/open_dir. *)
    | Stat_error of code  (** Fail a stat. *)
    | Swap_symlink of string
        (** Perform the operation, then replace its path with a symlink to the
            given target -- the check-to-use window a TOCTOU exploits.
            Reproduces, deterministically, a concurrent modifier swapping a
            checked file for a symlink under a caller that is still using it.

            Scripted on a [`Stat] op, the stat still sees the original file
            while the following open on the same path resolves to the symlink.
            Scripted on an [`Open] op -- a file opened for reading or writing,
            or a subtree -- the caller keeps its handle on the file the name
            meant at the open, while everything it asks the path afterwards is
            answered about the target. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same fault. *)

  val pp : t Fmt.t
  (** [pp] formats a fault for logs and test failures. *)
end

(** An operation the schedule is consulted about before it is performed. *)
module Op : sig
  type kind = [ `Read | `Write | `Sync | `Open | `Stat ]

  type t = {
    kind : kind;
    path : string;
    file_offset : int;  (** byte offset for reads/writes, [0] otherwise *)
    length : int;  (** bytes requested/offered, [0] otherwise *)
  }

  val pp : t Fmt.t
  (** [pp] formats an operation for logs and test failures. *)
end

(** The fault schedule: an inspectable value driving fault injection. *)
module Schedule : sig
  type trigger =
    | Nth_op of int
        (** Fire on the [n]th operation the schedule is consulted for (1-based,
            counting reads, writes, syncs, opens and stats). *)
    | Op_on_path of { kind : Op.kind; path : string }
        (** Fire on the first [kind] operation on [path]. *)
    | Write_covering of { path : string; file_offset : int }
        (** Fire on the first write on [path] whose byte range covers
            [file_offset]. Best paired with a write that starts at a known
            offset, since the fault's byte count is operation-relative. *)

  type rule = { trigger : trigger; fault : Fault.t }
  type config = { probability : float; kinds : Fault.kind list }
  type t

  val empty : t
  (** [empty] injects nothing: a pure pass-through wrapper. *)

  val scripted : rule list -> t
  (** [scripted rules] fires each rule the first time an operation matches its
      trigger. A fired rule is consumed; unmatched rules never fire. This pins
      an exact scenario -- for example, a single {!Write_covering} rule carrying
      a {!constructor-Torn_write} fault reproduces a torn superblock flush at a
      chosen offset. *)

  val seeded :
    ?probability:float -> ?kinds:Fault.kind list -> seed:int -> unit -> t
  (** [seeded ~seed ()] injects deterministic pseudo-random faults. For each
      operation it fires a fault with the configured {!field-probability}
      (default [0.5]), choosing uniformly among the enabled {!field-kinds}
      (default: all) applicable to that operation. The same [seed] and operation
      sequence always yield the same fault sequence. *)

  val next : t -> Op.t -> Fault.t option
  (** [next t op] advances the schedule by one operation and returns the fault
      to inject, if any. Called internally by {!wrap}; exposed for testing a
      schedule in isolation. *)

  val pp : t Fmt.t
  (** [pp] formats a schedule for logs (a seed or the count of pending rules).
  *)
end

val wrap : Schedule.t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [wrap schedule base] is [base] with adversarial behaviour driven by
    [schedule]. Directory operations, reads, writes and syncs are delegated to
    [base] but perturbed according to the schedule; opened files and
    subdirectories stay wrapped and share [schedule].

    A wrapper answers for the base it was given. The wrapped directory reports
    the base's own {!Eio_fs_sync.Process_local}, so a caller weighing whether an
    exclusion taken between the fibers of this process is the whole exclusion
    weighs the storage underneath: over the in-memory backend it is, over a real
    directory it is not. A file opened read-write carries whatever the base's
    file carries beyond reading, writing, syncing and closing -- over a real
    directory the Unix descriptor [Eio_unix.Resource.fd_opt] answers with, so a
    caller that must take a record lock on the file or hand it to a launcher
    can; over the in-memory backend there is no descriptor to carry.

    That descriptor is the operations the operating system offers on an open
    file, and not a way past the schedule: a read or write issued on it is not
    an operation the schedule is consulted about. A file opened read-write also
    registers {!Eio_fs_sync.File_sync}, so a barrier taken through
    {!Eio_fs_sync.sync_file} reaches the schedule's own sync rather than the
    carried descriptor, and a sync fault fires on it as on any other. *)

(** A power-loss durability model layered over a base directory capability.

    Every write is tracked as {e un-synced} until a successful {!Eio.File.sync}
    on that file marks all its prior writes durable. A {e crash} rewrites the
    base to a post-crash image in which un-synced writes may be dropped and/or
    reordered relative to each other and to the sync barrier, while synced
    writes are always durable and never move past their barrier. Reads and
    writes are delegated to [base] unchanged while the workload runs, so its
    live view stays consistent; the base is rewritten only at crash time.

    This makes the classic missing-{!Eio.File.sync} hazard demonstrable: a
    pointer write that became durable before the data it references, because no
    barrier separated them.

    {b Directory operations are journaled.} File creation, [mkdir], [unlink],
    [rmdir], [symlink] and [rename] enter a metadata journal in program order.
    An explicit {!sync_metadata} commits the journal whole. A barrier issued
    through the wrapper -- {!Eio_fs_sync.sync_dir} on a directory, which reaches
    the registered {!Eio_fs_sync.Dir_sync} -- commits what fsync(2) commits: the
    entries {e of} that directory, the journal entries whose recorded path names
    one of its children. It says nothing about that directory's own entry in its
    parent, about a sibling of it, or about an entry one level further down, so
    a store that barriers the top of a [mkdirs] and expects the bottom to be
    durable is modelled as losing the bottom. An entry of a directory whose own
    entry is not durable stays pending with it. Syncing a {e file} does not
    commit it: fsync(2) on a descriptor makes that file's data durable and says
    nothing about the directory its name lives in, so a store that removes a
    journal and reaches for the file barrier instead of the directory one is
    modelled as losing the removal. At a crash the policy decides how much of
    the uncommitted journal suffix survives; survival is always a {e prefix} of
    program order, because metadata commits in order. A dropped operation is
    rolled back in the post-crash image: a dropped creation removes the file, a
    dropped unlink restores it with its durable content, a dropped rename moves
    the file back and restores any entry it replaced -- whole, never a mixture.

    The crash rewrite reaches every tracked file through the directory given to
    {!Crash.wrap}, so a file staged through a short-lived subtree
    ({!Eio.Path.with_open_dir}) is rewritten even after that subtree is closed.
    A file created read-only is rewritten too, and keeps its mode. *)
module Crash : sig
  (** What kind of un-synced change a journal entry records. *)
  type op_kind =
    | Data  (** A write or truncation; offset and length locate it. *)
    | Create  (** The open that created the file. *)
    | Mkdir
    | Unlink
    | Rmdir
    | Symlink of { target : string }  (** A new symlink to [target]. *)
    | Rename of { dest : string }  (** An entry move to [dest]. *)

  type unsynced = {
    path : string;
    kind : op_kind;
    offset : int;
    length : int;
    seq : int;
  }
  (** An un-synced change: {!field-path} as opened, the {!field-kind} of change,
      byte {!field-offset} and {!field-length} of a {!Data} write (a truncation
      reports {!field-offset} = new length, {!field-length} = 0; directory
      operations report both as [0]), and the program-order sequence number
      {!field-seq}. *)

  (** How the post-crash image reconstructs from the un-synced changes. *)
  type policy =
    | Drop_unsynced
        (** Every un-synced write is lost and every uncommitted directory
            operation is rolled back; only committed state survives. *)
    | Reorder_unsynced of int
        (** Seeded: each un-synced write survives on an even coin and the
            survivors are applied in a seeded-shuffled order, so a later write
            may land while an earlier one is lost. Uncommitted directory
            operations survive as a seeded-length prefix of program order --
            metadata never reorders. The [int] is the seed; the same seed and
            workload reproduce the same image. *)
    | Survive of (unsynced -> bool)
        (** Scripted: an un-synced write survives iff the predicate holds;
            survivors keep program order. An uncommitted directory operation
            survives iff the predicate holds for it {e and} every earlier
            uncommitted directory operation survived (the prefix rule). Pins an
            exact scenario -- for example keep a generation pointer but drop the
            data block it references. *)

  (** When a crash fires while the workload runs. *)
  type trigger =
    | Manual  (** Only an explicit {!crash} (or {!post_crash}) fires it. *)
    | After_write of { path : string; file_offset : int }
        (** Crash right after the write covering that offset in that file
            commits, before any following sync. *)
    | Nth_op of int
        (** Crash on the [n]th durability op (writes, syncs and directory
            operations, 1-based). *)
    | After_dir_op of { kind : op_kind; path : string }
        (** Crash once the directory operation of that {!type-op_kind} on that
            path has entered the metadata journal, and before any following
            operation can commit it: the window a directory operation with no
            barrier of its own leaves open. [kind] and [path] are matched
            structurally, exactly as {!type-unsynced} records them, so a
            {!Rename} names its destination and a {!Symlink} its target. *)

  type t
  (** A crash controller: tracks durability across the files opened through it
      and holds the crashed image after a crash. *)

  type base_dir = Eio.Fs.dir_ty Eio.Path.t
  (** The backing store handed back after a crash. *)

  val v : ?policy:policy -> ?trigger:trigger -> unit -> t
  (** [v ()] is a fresh controller. The policy defaults to {!Drop_unsynced} and
      the trigger to {!Manual}. *)

  val wrap : t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [wrap t base] is [base] with durability tracked by [t]. Files and
      subdirectories opened through it stay wrapped and share [t].

      A wrapper answers for the base it was given. The wrapped directory reports
      the base's own {!Eio_fs_sync.Process_local}, and a file opened read-write
      carries the base's own descriptor where the base has one -- over a real
      directory that is the descriptor [Eio_unix.Resource.fd_opt] answers with,
      so a store that takes a record lock on a file, or hands one to a launcher,
      runs under the model rather than around it.

      {b The model's authority is the wrapper, not the descriptor.} What that
      descriptor offers is the operations the operating system offers on an open
      file, and none of them enter the journal: a write issued on it is not a
      write the model saw and is not in the post-crash image, and a barrier
      issued on it is not the model's barrier -- a file's writes stay un-synced
      until it is synced through the wrapper. Two calls reach it:
      {!Eio.File.sync}, and {!Eio_fs_sync.sync_file}, which finds the
      {!Eio_fs_sync.File_sync} operation a wrapped read-write file registers and
      so commits this file's data rather than fsyncing the descriptor behind the
      model's back. A crash rebuilds the whole wrapped tree from the model, so a
      change made to that tree outside the wrapper -- through a carried-over
      descriptor, by a child process, or by native name -- is gone from the
      post-crash image whether or not the model would have kept a change of its
      own. *)

  val crash : t -> unit
  (** [crash t] triggers a crash now: it reconstructs the post-crash image per
      {!policy} and rewrites the base to it. Idempotent -- a second call is a
      no-op. After a crash, reads, writes and syncs through the wrapper fail
      with an {!Eio.Io} exception ("power lost"); re-read through {!post_crash}.
  *)

  val is_crashed : t -> bool
  (** [is_crashed t] is [true] once a crash has fired. *)

  val post_crash : t -> base_dir
  (** [post_crash t] crashes [t] if it has not already, then returns the base
      directory over the crashed image -- a fresh, un-perturbed handle a test
      re-mounts and re-reads through. Raises [Invalid_argument] if [t] was never
      {!wrap}ped. *)

  val sync_metadata : t -> unit
  (** [sync_metadata t] commits the metadata journal: every pending directory
      operation becomes durable, as fsync(2) on the enclosing directories would
      make it. The model's stand-in for the directory-fsync barrier Eio cannot
      express; a store whose recovery depends on directory-entry durability
      calls this wherever production code fsyncs a directory. *)

  val unsynced : t -> unsynced list
  (** [unsynced t] lists the changes not yet made durable, in program order. *)

  val pp_unsynced : unsynced Fmt.t
  (** [pp_unsynced] formats an un-synced change for logs and test failures. *)
end

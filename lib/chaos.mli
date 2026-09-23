(** An adversarial {!Eio.Fs.dir} wrapper for fault-injection testing.

    {!wrap} composes with any base directory capability, such as the in-memory
    backend from {!Eio_fs.Fs} or a real filesystem, and perturbs reads, writes,
    syncs, opens and stats according to a {!Schedule.t}. A schedule is either
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
  (** The family a fault belongs to. [`Swap] is scripted-only, so a seeded
      schedule never generates it. *)

  type t =
    | Short_read of int
        (** Deliver at most [n] bytes of the requested read ([n >= 1]), forcing
            the caller to loop. *)
    | Short_write of int
        (** Accept at most [n] bytes of the offered write ([n >= 1]), forcing
            the caller to loop. *)
    | Torn_write of int
        (** Commit [n] bytes to the base, then raise: bytes reach the store but
            the operation reports failure. *)
    | Write_error of code  (** Commit nothing, raise a write error (ENOSPC). *)
    | Sync_error of code  (** Fail a flush/sync. *)
    | Open_error of code  (** Fail an open/open_dir. *)
    | Stat_error of code  (** Fail a stat. *)
    | Swap_symlink of string
        (** Perform the operation, then replace its path with a symlink to the
            given target: the check-to-use window a TOCTOU exploits. On a stat,
            the stat sees the original file while the following open resolves to
            the symlink; on an open, the caller keeps its handle while the path
            afterwards answers about the target. *)

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
      an exact scenario: for example, a single {!Write_covering} rule carrying a
      {!constructor-Torn_write} fault reproduces a torn superblock flush at a
      chosen offset. *)

  val observing : (Op.t -> unit) -> t -> t
  (** [observing f t] is [t] with [f] called on every operation the wrapper
      consults the schedule about, before the schedule decides what to inject.
      It injects nothing of its own, so [observing f empty] is a pass-through
      wrapper that reports what the code under test asked the filesystem for.

      This is how a caller's cost is asserted in the unit the work is measured
      in rather than in seconds: how many reads a scan issues over a file is a
      property of the code, where the wall it takes to issue them is a property
      of the machine it ran on. *)

  val seeded :
    ?probability:float -> ?kinds:Fault.kind list -> seed:int -> unit -> t
  (** [seeded ~seed ()] injects deterministic pseudo-random faults. For each
      operation it fires a fault with the configured {!field-probability}
      (default [0.5]), choosing uniformly among the enabled {!field-kinds}
      (default: all) applicable to that operation. The same [seed] and operation
      sequence always yield the same fault sequence. *)

  val next : t -> Op.t -> Fault.t option
  (** [next t op] advances the schedule by one operation and is the fault to
      inject, if any. {!wrap} calls it; it is exported to test a schedule in
      isolation. *)

  val pp : t Fmt.t
  (** [pp] formats a schedule for logs (a seed or the count of pending rules).
  *)
end

val wrap : Schedule.t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [wrap schedule base] is [base] with operations perturbed by [schedule];
    opened files and subdirectories stay wrapped and share [schedule].

    The wrapper answers for the base it was given: it reports the base's
    {!Eio_fs_sync.Process_local}, and a file opened read-write carries the
    base's descriptor where there is one. That descriptor is not a way past the
    schedule, and the file also registers {!Eio_fs_sync.File_sync} so a barrier
    through {!Eio_fs_sync.sync_file} reaches the schedule's own sync. *)

val base_dir : _ Eio.Resource.t -> Eio.Fs.dir_ty Eio.Resource.t option
(** [base_dir dir] is the directory wrapped by {!wrap}, when [dir] is such a
    wrapper. Backend extensions use it only after the wrapper has observed the
    path operation that precedes their own operation. *)

(** A power-loss durability model over a base directory capability.

    Every write is tracked as un-synced until a successful {!Eio.File.sync}
    makes that file's prior writes durable. A crash rewrites the base to a
    post-crash image in which un-synced writes may be dropped or reordered,
    which demonstrates the classic missing-{!Eio.File.sync} hazard.

    {b Directory operations are journaled.} Creation, [mkdir], [unlink],
    [rmdir], [symlink] and [rename] enter a metadata journal in program order;
    {!sync_metadata} commits it whole, and a directory barrier commits only the
    entries of that directory. At a crash the policy decides how much of the
    uncommitted suffix survives, always a prefix of program order, and a dropped
    operation is rolled back whole. *)
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
  (** An un-synced change: [path] as opened, [kind], the byte [offset] and
      [length] of a {!Data} write (a truncation reports [offset] = new length,
      [length] = 0; directory operations report both as [0]), and the
      program-order [seq]. *)

  (** How the post-crash image reconstructs from the un-synced changes. *)
  type policy =
    | Drop_unsynced
        (** Every un-synced write is lost and every uncommitted directory
            operation is rolled back; only committed state survives. *)
    | Reorder_unsynced of int
        (** Seeded: each un-synced write survives on an even coin and the
            survivors are applied in a seeded-shuffled order, so a later write
            may land while an earlier one is lost. Uncommitted directory
            operations survive as a seeded-length prefix of program order:
            metadata never reorders. The [int] is the seed; the same seed and
            workload reproduce the same image. *)
    | Survive of (unsynced -> bool)
        (** Scripted: an un-synced write survives iff the predicate holds;
            survivors keep program order. An uncommitted directory operation
            survives iff the predicate holds for it {e and} every earlier
            uncommitted directory operation survived (the prefix rule). It pins
            an exact scenario, for example keeping a generation pointer but
            dropping the data block it references. *)

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
  (** [wrap t base] is [base] with durability tracked by [t]; files and
      subdirectories opened through it stay wrapped and share [t].

      The model's authority is the wrapper, not a carried descriptor: a write or
      barrier issued on the descriptor is not in the journal. A file's writes
      stay un-synced until {!Eio.File.sync} or {!Eio_fs_sync.sync_file} reaches
      it through the wrapper. A crash rebuilds the tree from the model, so a
      change made outside the wrapper is gone from the post-crash image. *)

  val crash : t -> unit
  (** [crash t] triggers a crash now: it reconstructs the post-crash image per
      {!policy} and rewrites the base to it. A second call does nothing. After a
      crash, reads, writes and syncs through the wrapper fail with an {!Eio.Io}
      exception ("power lost"); re-read through {!post_crash}. *)

  val is_crashed : t -> bool
  (** [is_crashed t] is [true] once a crash has fired. *)

  val post_crash : t -> base_dir
  (** [post_crash t] crashes [t] if it has not already crashed, and is the base
      directory over the crashed image: a fresh, unperturbed handle that a test
      re-mounts and re-reads through.

      @raise Invalid_argument if [t] was never {!wrap}ped. *)

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

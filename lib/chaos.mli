(** An adversarial {!Eio.Fs.dir} wrapper for fault-injection testing.

    {!wrap} composes with any base directory capability, such as the in-memory
    backend from {!Eio_fs.Fs} or a real filesystem, and perturbs reads, writes,
    syncs, opens, stats and directory reads according to a {!Schedule.t}. A
    schedule is either {e seeded} (deterministic pseudo-random faults, for
    fuzzing) or {e scripted} (an inspectable list of rules, for pinning an exact
    scenario such as a torn write during a superblock flush).

    All injected faults surface as ordinary {!Eio.Io} exceptions, so code under
    test cannot distinguish an injected failure from a real backend failure.

    {!Crash} is the companion model of power loss: it tracks which writes and
    directory operations a barrier has made durable, and rebuilds the base as a
    crash would leave it. *)

type code = [ `Enoent | `Eacces | `Enospc | `Eio ]
(** The type for the POSIX error codes an injected fault raises. An
    {!Fault.t.Open_error} or {!Fault.t.Stat_error} raises {!Eio.Fs.Not_found}
    for [`Enoent] and {!Eio.Fs.Permission_denied} for [`Eacces]; every other
    injected error is an {!Eio.Io} whose message names the code. *)

(** The fault kinds the wrapper can inject. Byte counts on the short-read,
    short-write and torn-write faults are relative to the current operation's
    buffer. *)
module Fault : sig
  type kind =
    [ `Short_read
    | `Read_error
    | `Short_write
    | `Torn_write
    | `Write_error
    | `Sync_error
    | `Open_error
    | `Stat_error
    | `Swap ]
  (** The type for fault families: one per constructor of {!t}, named after it,
      and [`Swap] for {!constructor-Swap_symlink}. A seeded schedule never
      generates [`Swap]: a swap needs a target it cannot draw. *)

  (** The type for injected faults. A fault fires on one operation; one that
      does not apply to that operation's kind (a write fault on a read, say)
      injects nothing. *)
  type t =
    | Short_read of int
        (** Deliver at most [max 1 n] bytes of the requested read, forcing the
            caller to loop. A read of an empty buffer is left whole. *)
    | Read_error of code
        (** Deliver nothing and raise [code], on a read or a directory read. *)
    | Short_write of int
        (** Accept at most [max 1 n] bytes of the offered write, forcing the
            caller to loop. *)
    | Torn_write of int
        (** Commit the first [n] bytes of the write (none if [n <= 0], all of
            them if [n] exceeds its length) to the base, then raise [`Enospc]:
            bytes reach the store but the operation reports failure. *)
    | Write_error of code  (** Commit nothing and raise [code]. *)
    | Sync_error of code  (** Fail a sync with [code]. *)
    | Open_error of code  (** Fail an open or a directory open with [code]. *)
    | Stat_error of code  (** Fail a stat with [code]. *)
    | Swap_symlink of string
        (** Perform an open or a stat, then replace its path with a symbolic
            link to the given target: the check-to-use window a TOCTOU exploits.
            On a stat, the stat sees the original file while the following open
            resolves to the link; on an open, the caller keeps its handle while
            the path afterwards answers about the target. *)

  val equal : t -> t -> bool
  (** [equal f0 f1] is [true] iff [f0] and [f1] are the same constructor with
      equal arguments. *)

  val pp : t Fmt.t
  (** [pp] formats a fault for logs and test failures. *)
end

(** An operation the schedule is consulted about before it is performed. *)
module Op : sig
  type kind =
    [ `Read  (** A read of a file's bytes, streaming or positioned. *)
    | `Read_dir
      (** A directory listing, by either enumeration entry point. It accepts
          {!Fault.t.Read_error}, never a byte-oriented short read. *)
    | `Write  (** A write of a file's bytes, streaming or positioned. *)
    | `Sync  (** A file's sync. *)
    | `Open  (** An open of a file or a subdirectory. *)
    | `Stat  (** A stat, of a path or of an open file. *) ]
  (** The type for operation kinds. *)

  type t = {
    kind : kind;  (** What the operation does. *)
    path : string;
        (** The path the operation names, as given to the wrapped directory it
            went through; for a file operation, the path the file was opened by.
        *)
    file_offset : int;
        (** The byte offset of a read or a write, and [0] otherwise. *)
    length : int;
        (** The bytes a read requests or a write offers, and [0] otherwise. *)
  }
  (** The type for operations. *)

  val pp : t Fmt.t
  (** [pp] formats an operation for logs and test failures. *)
end

(** The fault schedule: an inspectable value driving fault injection. *)
module Schedule : sig
  (** The type for the conditions under which a scripted rule fires. *)
  type trigger =
    | Nth_op of int
        (** Fire on the [n]th operation the schedule is consulted for (1-based,
            counting reads, directory reads, writes, syncs, opens and stats). *)
    | Op_on_path of { kind : Op.kind; path : string }
        (** Fire on the first [kind] operation on [path]. *)
    | Nth_op_on_path of { n : int; kind : Op.kind; path : string }
        (** Fire on the [n]th [kind] operation on [path] (1-based), counting
            from the schedule's first operation whether or not a rule fired on
            the earlier ones: the fault falls on a chosen later open or read of
            a file the code under test also opens before it. *)
    | Write_covering of { path : string; file_offset : int }
        (** Fire on the first write on [path] whose byte range covers
            [file_offset]. Best paired with a write that starts at a known
            offset, since the fault's byte count is operation-relative. *)

  type rule = {
    trigger : trigger;  (** When the rule fires. *)
    fault : Fault.t;  (** What it injects when it fires. *)
  }
  (** The type for scripted rules. *)

  type t
  (** The type for fault schedules. A schedule is stateful: it counts the
      operations it is consulted about, consumes the rules that fire and draws
      from its generator, so every wrapper given one schedule shares that state.
  *)

  val empty : t
  (** [empty] injects nothing: a pure pass-through wrapper. *)

  val scripted : rule list -> t
  (** [scripted rules] fires each rule the first time an operation matches its
      trigger. A fired rule is consumed; unmatched rules never fire. When
      several pending rules match one operation, the first in [rules] fires and
      the others stay pending. This pins an exact scenario: for example, a
      single {!Write_covering} rule carrying a {!Fault.t.Torn_write} fault
      reproduces a torn superblock flush at a chosen offset. *)

  val observing : (Op.t -> unit) -> t -> t
  (** [observing f t] is [t] with [f] called on every operation the wrapper
      consults the schedule about, before the schedule decides what to inject.
      It injects nothing of its own, so [observing f empty] is a pass-through
      wrapper that reports what the code under test asked the filesystem for.

      A test asserts a caller's cost with it in the unit the work is measured in
      rather than in seconds: how many reads a scan issues over a file is a
      property of the code, where the time it takes to issue them is a property
      of the machine it ran on. *)

  val seeded :
    ?probability:float -> ?kinds:Fault.kind list -> seed:int -> unit -> t
  (** [seeded ~probability ~kinds ~seed ()] injects deterministic pseudo-random
      faults. On each operation it fires a fault with probability [probability]
      (defaults to [0.5]; never at [0.] or below, always at [1.] or above),
      choosing uniformly among the kinds in [kinds] (defaults to every kind but
      [`Swap]) that apply to that operation, and firing nothing when none does.
      [`Swap] in [kinds] is ignored. The same [seed] and operation sequence
      always yield the same fault sequence. *)

  val pp : t Fmt.t
  (** [pp] formats a schedule for logs: its seed and probability, or how many of
      its rules are pending. *)
end

val wrap : Schedule.t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [wrap schedule base] is [base] with operations perturbed by [schedule];
    opened files and subdirectories stay wrapped and share [schedule]. The
    base's other directory capabilities remain available, including a native
    process working directory. Operations issued directly through those backend
    capabilities or by a child process do not consult [schedule]; faults cover
    the wrapped directory and file operations, not native descriptor syscalls.

    The wrapper answers for the base it was given: it reports the base's
    {!Eio_fs_sync.Process_local}, and a file opened read-write carries the
    base's descriptor where there is one. The file registers
    {!Eio_fs_sync.File_sync} so a barrier through {!Eio_fs_sync.sync_file}
    reaches the schedule's own sync. *)

(** A power-loss durability model over a base directory capability.

    Every write is tracked as un-synced until a successful {!Eio.File.sync}
    makes that file's prior writes durable. A crash rewrites the base to a
    post-crash image in which un-synced writes may be dropped or reordered,
    which demonstrates the classic missing-{!Eio.File.sync} hazard.

    {b Directory operations are journaled.} Creation, [mkdir], [unlink],
    [rmdir], [symlink] and [rename] enter a metadata journal in program order;
    {!Crash.sync_metadata} commits it whole, and a directory barrier commits
    only the entries of that directory. At a crash the policy decides how much
    of the uncommitted suffix survives, always a prefix of program order, and a
    dropped operation is rolled back whole. *)
module Crash : sig
  (** The type for the kinds of un-synced change a journal entry records. *)
  type op_kind =
    | Data  (** A write or truncation; offset and length locate it. *)
    | Create  (** The open that created the file. *)
    | Mkdir  (** A new directory. *)
    | Unlink  (** A removed file or symbolic link. *)
    | Rmdir  (** A removed directory. *)
    | Symlink of { target : string }  (** A new symbolic link to [target]. *)
    | Rename of { dest : string }  (** An entry moved to [dest]. *)

  type unsynced = {
    path : string;  (** The path the change was made at, as opened. *)
    kind : op_kind;  (** What the change is. *)
    offset : int;
        (** The byte offset of a {!Data} write, the new length of a truncation,
            and [0] for a directory operation. *)
    length : int;
        (** The byte length of a {!Data} write, and [0] for a truncation or a
            directory operation. *)
    seq : int;  (** The change's position in program order. *)
  }
  (** The type for un-synced changes. *)

  (** The type for how the post-crash image is rebuilt from the un-synced
      changes. *)
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

  (** The type for when a crash fires while the workload runs. *)
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
  (** The type for crash controllers. A controller tracks durability across the
      files opened through its wrapper and holds the crashed image after a
      crash. *)

  type base_dir = Eio.Fs.dir_ty Eio.Path.t
  (** The type for the backing store handed back after a crash. *)

  val v : ?policy:policy -> ?trigger:trigger -> unit -> t
  (** [v ~policy ~trigger ()] is a fresh controller that rebuilds the post-crash
      image by [policy] (defaults to {!Drop_unsynced}) and crashes on [trigger]
      (defaults to {!Manual}). *)

  val wrap : t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [wrap t base] is [base] with durability tracked by [t]; files and
      subdirectories opened through it stay wrapped and share [t]. The tree
      under [base] when [wrap] is called is [t]'s durable starting image.

      The model's authority is the wrapper, not a carried descriptor: a write or
      barrier issued on the descriptor is not in the journal. A file's writes
      stay un-synced until {!Eio.File.sync} or {!Eio_fs_sync.sync_file} reaches
      it through the wrapper. A crash rebuilds the tree from the model, so a
      change made outside the wrapper is gone from the post-crash image.

      @raise Invalid_argument
        if the tree under [base] holds an entry that is neither a regular file,
        a directory nor a symbolic link. *)

  val crash : t -> unit
  (** [crash t] triggers a crash now: it reconstructs the post-crash image per
      {!type-policy} and rewrites the base to it. A second call does nothing.
      After a crash, every operation through the wrapper (a read, a write, a
      sync, an open, a stat or a directory operation) fails with an {!Eio.Io}
      exception ("power lost") and leaves the image as it is; re-read through
      {!post_crash}.

      @raise Invalid_argument
        if [t] was never {!wrap}ped; [t] is left uncrashed. *)

  val is_crashed : t -> bool
  (** [is_crashed t] is [true] iff a crash has fired on [t]. *)

  val post_crash : t -> base_dir
  (** [post_crash t] crashes [t] if it has not already crashed, and is the base
      directory over the crashed image: a fresh, unperturbed handle that a test
      re-mounts and re-reads through.

      @raise Invalid_argument if [t] was never {!wrap}ped. *)

  val sync_metadata : t -> unit
  (** [sync_metadata t] commits the whole metadata journal: every pending
      directory operation becomes durable, as fsync(2) on every directory
      concerned would make it. A directory barrier ({!Eio_fs_sync.sync_dir} on a
      wrapped directory) commits only the entries of that directory.

      @raise Eio.Io ("power lost") if [t] has crashed. *)

  val unsynced : t -> unsynced list
  (** [unsynced t] are the changes not yet made durable, in program order. *)

  val pp_unsynced : unsynced Fmt.t
  (** [pp_unsynced] formats an un-synced change for logs and test failures. *)
end

(**/**)

(* For this package's tests and eio-fs.unix only. *)
module Private : sig
  val next : Schedule.t -> Op.t -> Fault.t option
  (* [next t op] advances [t] by one operation and is the fault to inject. *)

  val base_dir : _ Eio.Resource.t -> Eio.Fs.dir_ty Eio.Resource.t option
  (* [base_dir dir] is the directory {!wrap} wraps, when [dir] is a wrapper; a
     backend extension reads it only after the wrapper has observed the path
     operation preceding its own. *)
end

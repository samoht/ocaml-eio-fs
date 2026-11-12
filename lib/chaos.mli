(** Filesystem faults and power cuts, for tests of Eio programs.

    {!check} runs a workload under filesystem errors. {!Crash.check} cuts the
    power during a workload, and during its recovery, and checks what survives.
    Both run on fresh in-memory filesystems and choose their scenarios from what
    the workload does.

    {2 Exploration}

    A check runs the workload once with no fault and records its operations. Its
    scenarios are positions in that run, in two tiers:

    - the linear tier always runs whole. On each file it takes the first three
      reads and writes, the last, and the first of each length class, a length
      rounded down to a power of two, with the syncs that follow them, and every
      other operation: it grows with the structure of the workload, not with its
      bytes. There it tries each fault alone, and each power cut in its two
      simplest states;
    - the sampled tier spends the rest, at most half as much as the linear tier
      and at least 500 runs: the other positions, pairs of faults, the second at
      a position of the run the first one changed, and the other states a power
      cut may leave.

    A check never passes on a partial linear tier. One that needs more runs than
    its budget fails and says so: shrink the workload or raise [~budget].

    {2 Contracts}

    {b At most two faults per run, or one lasting condition.} A program that
    tries an operation three times gets past two faults, but no retry gets past
    a lasting condition: [run] must accept that any operation fails, and may
    demand only that the program handles it.

    {b A lasting condition reaches every operation.} A filesystem that turns
    read-only, full, or denies access does so for every operation from then on,
    the retries included. A run fails as a hang when one operation on one path,
    or one kind of operation in one directory, fails, while its clock stands
    still, 10000 times more than three tries of each time the first run made it:
    a program that retries at once spins so, under one name or a fresh one each
    time, and a batch that drops what fails does not.

    {b [recover] stands for a fresh process.} It runs after the cut on the
    directory as the disk holds it, knowing only what the workload acknowledged:
    it must not read state the workload left in memory.

    {b A check blocks its caller.} Each run has its own event loop, inside or
    outside another one; the caller's loop waits until the check returns.

    {2 Failures and replay}

    A check is deterministic. A failure raises [Failure] with the scenario, the
    tier that found it, the operations of the failing run, and the
    [~replay:"..."] argument that runs that scenario alone. When the workload's
    first run has changed, a replay finds its scenario again by its operations,
    and its report says so. A replay fails if the check no longer has that
    scenario.

    A check reads no environment variable and prints nothing. A test that wants
    a larger budget at night reads its own:
    {[
    let check run =
      let budget = Option.map int_of_string (Sys.getenv_opt "CHAOS_BUDGET") in
      Eio_fs.Chaos.check ?budget run
    ]}

    {2 Runs}

    Each run's clocks start at [0.] and move only when every fiber waits, so a
    sleep takes no time. A run in which every fiber waits and no timer is due
    fails as a hang, and so does one still running after an hour of its clock.
*)

type env =
  < fs : Eio.Fs.dir_ty Eio.Path.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; mono_clock : Eio.Time.Mono.ty Eio.Resource.t >
(** The type for what a workload runs with: a fresh in-memory directory and the
    run's clocks. [Eio_net_chaos.env] has the same shape, with [net] in place of
    [fs]. *)

(** {1:checks Checks} *)

val check :
  ?budget:int -> ?replay:string -> ?setup:(env -> unit) -> (env -> unit) -> unit
(** [check ~budget ~replay ~setup run] calls [run] many times, each on a fresh
    in-memory directory that holds what [setup] (defaults to nothing) made
    there, with no fault. The first run has no fault either.

    The linear tier tries each of these alone, at each structural operation:

    - the operation fails with each error Linux answers there: [EACCES] or
      [EROFS] on an open that may write or a directory change, and [ENOSPC] on
      one that makes a file or an entry, [EACCES] or [EIO] on an open for
      reading or a stat, [ENOSPC] or [EIO] on a write, [EIO] on a read or a
      sync;
    - a read or a write moves a single byte;
    - a write is torn: half its bytes land, then it fails with [ENOSPC];
    - from that operation on, the filesystem is read-only, full, or denies every
      access: each operation that can meet that error then fails with it, as
      above, and a write to a file already open is still allowed under the last.

    The sampled tier adds the other operations, and a second fault on the
    operations the first one caused, which is where a retry path lives.

    {!Fault.Swap_symlink} is not among them: it fails ordinary code that stats
    then opens a path in a directory no attacker can write. Security-sensitive
    code asks for it with {!wrap}.

    A run passes if [run] returns. [run] catches the errors its program may
    raise and checks the results itself: any exception that escapes it fails the
    check, an injected [Eio.Io] included. So does a hang, a first run that makes
    no filesystem operation, and two first runs that make different ones.

    [budget] (defaults to [20_000]) bounds the runs. [replay] runs only the
    scenario a failure named.

    @raise Failure with a report of the first failing run.
    @raise Invalid_argument if [budget] is not positive. *)

(** Power cuts.

    A write stays unsynced until {!Eio.File.sync} runs on its file. Directory
    changes enter a journal in program order, and a sync of a directory commits
    its entries. At a power cut, unsynced writes may be lost, as ext4 loses them
    with [data=writeback]. A file's length is one it had since its last sync, or
    the end of a 4096-byte page a write put bytes in and grew it across. A
    512-byte sector is written whole: it holds what the disk held, zeros where
    it held nothing, or what it held after one of the writes to it, so the
    writes to it land together. A truncation zeroes the rest of the 4096-byte
    page it cuts, sector by sector; at a length from before it, a page it frees
    holds no later write, as ext4 reuses no block before the truncation commits.
    A sector a write put bytes in, in a 4096-byte page the disk held nothing of,
    past the durable length or in a hole, may instead hold stale data: what the
    disk held there before, at the same place in a page a synced truncation of
    the file freed, or in a file the cut removed, and else the byte ['7']. Of
    the uncommitted directory changes, a prefix survives. A failed sync loses
    the file's unsynced writes, as Linux does, though some of their sectors may
    already be on disk; the page cache may keep them or evict them. *)
module Crash : sig
  val check :
    ?budget:int ->
    ?replay:string ->
    ?setup:(env -> unit) ->
    (ack:('a -> unit) -> env -> unit) ->
    recover:(acked:'a list -> env -> unit) ->
    unit
  (** [check ~budget ~replay ~setup run ~recover] cuts the power during [run],
      and during [recover], and checks that [recover] accepts what each cut
      leaves.

      [setup] (defaults to nothing) prepares the directory, and what it makes is
      durable. [run] calls [ack v] once its program has promised that [v] is
      durable, as a database acknowledges a commit. After a cut,
      [recover ~acked env] runs as a fresh process would, on the directory as
      the disk holds it, with [acked] the values acknowledged before the cut, in
      order. It checks the program's invariants, those of what [setup] made
      included, and raises if one fails.

      The linear tier cuts the power after each structural operation of [run]
      that changes the disk, and at its end. Each cut is tried in two states:
      every unsynced change lost, and every change kept, as when only the
      process is killed. Each structural sync is also made to fail, and each
      structural write to tear, alone, with a cut at the end. Then, from each
      cut, the power is cut again after each structural change [recover] makes,
      and at its end, and [recover] runs once more on what that cut left.

      The sampled tier tries the other cuts and faults, pairs of faults, and
      cuts during [recover] after a fault; then every state each cut allows
      while they are few, at most 64 and within the cut's share of the tier, and
      states drawn at random past that, saying so in its report.

      A run passes if [run] returns and [recover] accepts the state. A cut
      raises [Eio.Cancel.Cancelled] from the operation it falls before, and from
      every one after, as in a cancelled fiber, and once every fiber waits it
      cancels them all, as nothing runs after a power cut. Neither [run] nor
      [recover] may catch it: one that makes 1000 more operations after the cut
      fails as a hang. Any other exception that escapes [run] before the cut
      fails the check, an injected [Eio.Io] included.

      Two first runs that make different operations fail the check before any
      scenario runs. Once every scenario has passed, a [recover] that made no
      filesystem operation in any run fails it: it sees nothing a cut leaves.
      The check refuses no other [recover], as it cannot without failing correct
      ones: one that accepts the directory as [setup] left it, told the values
      the first run acknowledged, may hold them already there, and one that
      accepts an empty directory may read a missing file as empty. A [recover]
      that ignores [acked] passes, and misses what a cut loses.

      [budget] (defaults to [20_000]) bounds the runs. [replay] runs only the
      scenario a failure named.

      @raise Failure with a report of the first failing run.
      @raise Invalid_argument if [budget] is not positive. *)

  (** {1:blocks Building blocks}

      A test that drives one scenario by hand, or runs on a directory on disk,
      builds it from a controller. A particular state a cut may leave is pinned
      with [~replay] rather than built by hand. *)

  (** The type for the kinds of unsynced change. *)
  type op_kind =
    | Data  (** A write; its offset and length locate it. *)
    | Truncate  (** A truncation; its offset is the new length. *)
    | Create  (** The open that created the file. *)
    | Mkdir
    | Unlink
    | Rmdir
    | Symlink of { target : string }
    | Rename of { dest : string }
    | Chmod of { perm : int }
    | Chown of { uid : int64; gid : int64 }

  type unsynced = {
    path : string;
        (** The path of the change: for a write or a truncation, the file's path
            with its links resolved; for a directory change, the path as given.
        *)
    kind : op_kind;
    offset : int;
    length : int;
    seq : int;  (** The change's place in program order. *)
  }
  (** The type for changes no sync has made durable. *)

  val pp_unsynced : unsynced Fmt.t
  (** [pp_unsynced] formats a change for logs and reports. *)

  (** The type for how a cut rebuilds the disk. *)
  type policy =
    | Drop_unsynced
        (** Every unsynced write and every uncommitted directory change is lost:
            the state at the last syncs. *)
    | Survive of (unsynced -> bool)
        (** The writes for which the predicate holds survive, whole but for the
            sectors they overwrite, which hold every earlier overwrite of
            theirs; a directory change survives if it holds for it and every
            earlier one. [Survive (fun _ -> true)] is a process kill: the disk
            keeps what the program saw. *)

  type t
  (** The type for power-cut controllers. *)

  val v : policy -> t
  (** [v policy] is a controller whose cut rebuilds the disk by [policy]. *)

  val wrap : t -> _ Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
  (** [wrap t base] is [base] with its writes and directory changes tracked by
      [t]. What [base] holds when [wrap] is called is durable. [base] may be in
      memory or on disk.

      @raise Invalid_argument if [t] is already wrapped. *)

  val unsynced : t -> unsynced list
  (** [unsynced t] is the changes not yet durable, in program order. *)

  val cut : t -> Eio.Fs.dir_ty Eio.Path.t
  (** [cut t] cuts the power, if [t] has not cut it yet, and is the base
      directory, which now holds what the disk kept. Every operation through the
      wrapper fails after the cut. Called from a {!Chaos.wrap} hook over the
      wrapper, it cuts the power before that operation, in the middle of a run:
      {[
        let t = Crash.v Drop_unsynced in
        let hook (op : Op.t) =
          if op.index = 7 then ignore (Crash.cut t);
          None
        in
        let dir = Chaos.wrap hook (Crash.wrap t base) in
        ...
      ]}

      @raise Invalid_argument if [t] was never wrapped. *)
end

(** {1:blocks Building blocks} *)

(** The operations a wrapper sees. *)
module Op : sig
  type kind =
    [ `Open_read  (** An open for reading, of a file or a directory. *)
    | `Open_write  (** An open that may write or create the file. *)
    | `Stat
    | `Read  (** A read of a file's bytes. *)
    | `Read_dir
    | `Write  (** A write of a file's bytes. *)
    | `Sync  (** A sync of a file or of a directory. *)
    | `Change
      (** A change to a directory's entries ([mkdir], [symlink], [unlink],
          [rmdir], [rename]) or to an entry's mode or owner ([chmod], [chown]).
      *) ]
  (** The type for operation kinds. *)

  type t = {
    index : int;  (** The operation's place among all of them, from [1]. *)
    kind : kind;
    path : string;
        (** The path the operation names, as given to the wrapper: ["/f"] for a
            file [f] at the root of {!Eio_fs.Fs.v}. A file operation names the
            path the file was opened by. *)
    nth : int;
        (** The operation's place among those of its kind on its path, from [1].
        *)
    file_offset : int;  (** Where a read or a write starts, [0] otherwise. *)
    length : int;  (** The bytes a read asks for or a write offers. *)
  }
  (** The type for operations. *)

  val pp : t Fmt.t
  (** [pp] formats an operation for logs and reports. *)
end

(** The faults a wrapper injects. *)
module Fault : sig
  type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]
  (** The type for error codes. [`Enoent] raises {!Eio.Fs.Not_found} on an open,
      a stat or a change, [`Eacces] and [`Eperm] raise
      {!Eio.Fs.Permission_denied} there, and the others an {!Eio.Io} error that
      names them. *)

  (** The type for faults. One that does not apply to an operation does nothing.
  *)
  type t =
    | Fail of code
        (** The operation fails with that error. [ENOSPC] applies to a write,
            and to an open or a change that makes a file or an entry only. *)
    | Short of int  (** A read or a write moves at most [max 1 n] bytes. *)
    | Torn of int
        (** A write stores its first [n] bytes, then fails with [ENOSPC]. *)
    | Swap_symlink of string
        (** An open or a stat runs, then its path becomes a symbolic link to the
            target: the window a time-of-check to time-of-use attack uses. Code
            that must not follow a link an attacker plants tests it this way:
            {[
              let swap (op : Op.t) =
                if op.kind = `Stat && op.path = "/out" then
                  Some (Fault.Swap_symlink "secret/key")
                else None
              in
              let dir = Chaos.wrap swap base in
              ...
            ]} *)

  val pp : t Fmt.t
  (** [pp] formats a fault for logs and reports. *)
end

val wrap : (Op.t -> Fault.t option) -> _ Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
(** [wrap f base] is [base] with [f] asked about each operation before it runs.
    The operation meets the fault [f] answers. [f] may also act: count, log,
    change [base], or cut the power with {!Crash.cut}. That is how a test
    measures the reads a scan makes, or forces an interleaving at one operation.
    Files and directories opened through the wrapper stay wrapped.

    Injected errors are ordinary {!Eio.Io} exceptions: the code under test
    cannot tell them from those of a disk. *)

(**/**)

module Private = Chaos_model

(**/**)

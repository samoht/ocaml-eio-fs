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
    - the sampled tier spends a fortieth of the budget, 500 runs by default, or
      half as much as the linear tier if that is more: the other positions,
      pairs of faults, and the other states a power cut may leave. A first fault
      that changes what the run writes gets its second faults first, on the
      operations that follow it, those that change the disk first: that is where
      a retry or a recovery path lives. A larger budget tries more.

    A check never passes on a partial linear tier. One that needs more runs than
    its budget fails and says so: shrink the workload or raise [~budget]. The
    first run stops as soon as the tier needs more, so a workload that polls for
    ever costs no more than the budget allows, and the report shows its trace up
    to there.

    {2 Contracts}

    {b At most two faults per run, or one lasting condition.} A program that
    tries an operation three times gets past two faults, but no retry gets past
    a lasting condition: [run] must accept that any operation fails, and may
    demand only that the program handles it.

    {b A lasting condition reaches every operation.} A filesystem that turns
    read-only, full, or denies access does so for every operation from then on,
    the retries included. A run fails as a hang when, while its clock stands
    still but for the time operations take, and with no success there in
    between, one operation on one path fails 10000 times more than three times
    the first run made it, or one kind of operation in one directory 10000 times
    more than 1000 times. A program that retries at once spins so, under one
    name or a fresh one each time. A batch that tries each item up to three
    times and drops what fails does not, nor a pool that tries up to 1000 names
    for each name its first run made. No check can tell a spin from a loop that
    ends later, so one that tries more often than that is taken for a spin.

    {b [recover] stands for a fresh process.} It runs after the cut on the
    directory as the disk holds it, knowing only what the workload acknowledged:
    it must not read state the workload left in memory.

    {b A check blocks its caller.} Each run has its own event loop, inside or
    outside another one; the caller's loop waits until the check returns.

    {2 Failures and replay}

    A check is deterministic. A failure raises [Failure] with the scenario, the
    tier that found it, the operations of the failing run, and the
    [~replay:"..."] argument that runs that scenario alone. A power cut's report
    also says after which operation each value was acknowledged, and, for a
    state drawn among those the cut allows, what each file holds: its length,
    and each run of sectors that holds a write, zeros or stale data, with where
    the stale data comes from. A report numbers an operation by its place in the
    trace, wherever it names one: the cut, the faults, the changes a cut lost
    and the acknowledgements. A block of up to eight operations repeated three
    times or more, in the same directories each time, as a retry makes them,
    shows its first and last instances, and a trace longer than that keeps its
    first and last hundred lines and those around its first fault, so a report's
    size is bounded by the workload's structure rather than by how long it spun.
    A run keeps only those lines as it goes, and so does its memory. When the
    workload's first run has changed, a replay finds its scenario again by its
    operations, and its report says so. A replay fails if the check no longer
    has that scenario, and if it names a state drawn at random of a cut that
    allows two states or fewer, which the check never draws. A state drawn at
    random under a larger budget needs that budget again, which the refusal
    names. The report of a check run inside another is text in the outer one.

    A check reads no environment variable and prints nothing. A test that wants
    a larger budget at night reads its own:
    {[
    let check run =
      let budget = Option.map int_of_string (Sys.getenv_opt "CHAOS_BUDGET") in
      Eio_fs.Chaos.check ?budget run
    ]}

    {2 Runs}

    Each run's clocks start at [0.] and move to the next timer when every fiber
    waits, so a sleep takes no time. Each turn, a fiber resumed by a yield,
    another fiber or a timer, also moves them by 1 us, as CPU time passes
    between two. Five guards stop a run that does not end:

    - a run in which every fiber waits and no timer is due fails as a hang;
    - a retry that never yields meets the spin guard above;
    - a run that makes 100000 filesystem operations fails as a busy loop, or as
      too large for a check, so a run's operations stay bounded: a poll that
      never yields stops there;
    - a run stops at 10 million turns. If no fiber waited 1 ms or more for a
      timer in the last 10 million, it fails as a busy loop: a poll by yields,
      or by sleeps shorter than 1 ms, that never ends. Otherwise it fails as too
      large for a check: a check repeats the run hundreds of times, so it
      refuses a longer one, as it refuses one past its budget. Fibers on timers
      take turns too: a thousand on a heartbeat of 100 ms reach 10 million in
      1000 s of clock;
    - a run still going after an hour of its clock fails as a hang: a poll that
      sleeps 1 ms or more between its tries and never ends.

    An operation takes time, and other fibers run meanwhile: a sync of a file or
    a directory 1 ms, any other operation 1 us, and one that fails none. Its
    effect lands at the end of that time. A sync covers what was written before
    it: a write another fiber makes while it runs stays unsynced, as Linux's
    fsync(2) promises nothing about it. *)

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
      reading or a stat, [ENOSPC] or [EIO] on a write, [EIO] on a read, a
      truncation or a sync;
    - a sync fails with [EIO] and the page cache drops the pages it could not
      write: a later read of them sees what the disk holds, as Linux may show
      after a failed fsync(2);
    - a read or a write moves a single byte;
    - a write is torn: half its bytes land, then it fails with [ENOSPC];
    - from that operation on, the filesystem is read-only, full, or denies every
      access: each operation that can meet that error then fails with it, as
      above, and a write to a file already open is still allowed under the last.

    The sampled tier adds the other operations, and a second fault on the path
    the first one sent the run down, which is where a retry path lives.

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
    the end of a 4096-byte page a write put bytes in, below a length it had
    since. A 512-byte sector is written whole: it holds what the disk held,
    zeros where it held nothing, or what it held after one of the writes to it,
    so the writes to it land together. A truncation zeroes the rest of the
    4096-byte page it cuts, sector by sector; at a length from before it, a page
    it frees holds no later write, as ext4 reuses no block before the truncation
    commits. A 4096-byte page the disk held nothing of, past the durable length
    or in a hole, that a write put bytes in is written whole, as ext4 allocates
    it: each of its sectors within the length may instead hold stale data. That
    is what a freed block held: a page a synced truncation freed, or one of a
    file the workload removed, its unsynced bytes included, as writeback may
    have written them before the removal. ext4 may give a freed block to any
    offset of any file: the check draws the page at the same place first, then
    the others, up to eight distinct ones; and else the byte ['7']. Only a freed
    block holds stale data: a file the cut lost and the workload never removed
    frees none, and a removed file still open at the cut none either, as ext4
    frees its blocks at its last close. Of the uncommitted directory changes, a
    prefix survives. A failed sync loses the file's unsynced writes, as Linux
    does, though some of their sectors may already be on disk; the page cache
    may keep them or evict them. Before the cut, a read after a failed sync sees
    them as kept, or, in a scenario of its own, as the disk holds them. *)
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
      process is killed. Each structural sync is also made to fail, with the
      pages it lost kept in the page cache and evicted from it, and each
      structural write to tear, alone, with a cut at the end. Then, from each
      cut, the power is cut again after each structural change [recover] makes,
      and at its end, and [recover] runs once more on what that cut left.

      The sampled tier tries the other cuts and faults, pairs of faults, and
      cuts during [recover] after a fault; then every state each cut allows
      while they fit in the cut's share of the tier, and states drawn at random
      past that, saying so in its report. A fourth of those draws are uniform;
      the others have a shape writeback leaves: a prefix of the sectors and
      changes landed and the rest not, all of them but one, or only one. What a
      cut does not use goes to the cuts that allow more states.

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
      memory or on disk; a hole in a file on disk counts as written zeros.

      A change that reaches [base] other than through the wrapper, by name or
      from a process the workload started, is kept by a cut as durable, as a
      process that syncs its own writes leaves it: the wrapper sees no other
      process's syncs, so what a test checks is the wrapper's writes, with the
      others' kept. At a path the wrapper changed, the policy decides, starting
      from what [base] held there when the wrapper first changed it, and a
      barrier on a directory another process made commits what the wrapper put
      in it. Stop those processes before calling {!cut}.

      @raise Invalid_argument if [t] is already wrapped. *)

  val unsynced : t -> unsynced list
  (** [unsynced t] is the changes not yet durable, in program order. *)

  val cut : t -> Eio.Fs.dir_ty Eio.Path.t
  (** [cut t] cuts the power, if [t] has not cut it yet, and is the base
      directory, which now holds what the disk kept. An entry the cut spared
      keeps its inode, as on a disk. On an in-memory base, each entry has the
      times of its version the disk kept, as ext4 journals them with the inode:
      a directory's from the last change to it the cut kept, a file's from that
      or its last sync, whichever came later. An unsynced write that survives
      does not move them. On a disk, an entry the cut spared keeps its times.
      Every operation through the wrapper fails after the cut. Called from a
      {!Chaos.wrap} hook over the wrapper, it cuts the power before that
      operation, in the middle of a run:
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
    | `Truncate  (** A truncation of a file; [file_offset] is its new length. *)
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
    file_offset : int;
        (** Where a read or a write starts, a truncation's new length, [0]
            otherwise. *)
    length : int;  (** The bytes a read asks for or a write offers. *)
    dest : string option;
        (** A rename's destination, as given to the directory it lands in. *)
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
    | Evict of code
        (** A sync fails with that error, and the page cache drops the pages it
            could not write: under {!Crash.wrap}, a later read of them sees the
            bytes the disk holds. Elsewhere it is {!constructor-Fail}. *)
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

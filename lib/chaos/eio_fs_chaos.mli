(** Filesystem faults and power cuts, for tests of Eio programs and of Eio
    filesystems.

    [Eio_fs_chaos] has two checks, one function each:

    - {!run} checks that an Eio program copes with a filesystem that fails. It
      runs the program many times on in-memory filesystems that fail, and each
      time starts it again on what the fault left, as a machine restarts a
      service.
    - {!val-conformance} checks that an Eio filesystem answers as Linux or macOS
      does.

    Both raise [Failure] with a report when the check fails, as an assertion
    does, so they are the body of a test in any test runner. {!run} is
    {!Eio_chaos.run} on one device, {!disk}: a program that also needs a network
    is tested with [Eio_chaos.run [ disk (); Eio_net_chaos.network () ]]. A test
    that wants one fault at one place builds it from the
    {{!blocks}building blocks}: {!wrap} for errors, {!Power} for a power cut by
    hand, {!ack} for a file the program has promised to keep.

    {!run}, {!val-conformance} and {!Power.v} take [?os], as {!Eio_fs_mem.v}
    does: [`Linux], the default, [`Macos] or [`Windows]. It picks the
    filesystem's answers and what a power cut keeps of each kind of sync. The
    choice comes from the argument, never from the host, so a test checks the
    same thing on every machine.

    {!val-conformance} replays {e scenarios}, each a short sequence of
    operations recorded on each kernel. A report of {!run} names its runs as
    scenarios too: a fault at a place of the first run.

    How a check picks its {{!exploration}faults}, the {{!contracts}contracts} a
    program meets, its {{!replay}reports and replay}, its {{!runs}runs} and the
    {{!model}disk model} a power cut follows come after. *)

type env = Eio_chaos.env
(** The type for what a program or a filesystem under test runs with: the
    capabilities of [Eio.Stdenv], each simulated, so code written against the
    accessors of [Eio.Stdenv] takes it unchanged. {!Eio_chaos.type-env} says
    what each is.

    - [fs] is the root of a fresh in-memory filesystem, the one that fails, and
      [cwd] the same directory, confined to itself as eio_posix confines a
      process's (an absolute path or link and a [".."] above it are refused with
      [Permission_denied]). Both run as {!Eio_fs_mem.root}, and
      {!Eio_fs_mem.as_user} runs them as another user. [net] refuses each
      operation, unless another device gives it, as the network of eio-net does;
    - [clock] and [mono_clock] start at [0.], and a wait takes no time
      ({{!runs}runs});
    - [secure_random] and the default [Random] state draw from a fixed seed,
      whatever [~seed]: the first run and each run with a fault draw the same,
      and a restart draws others, as a new process does;
    - [stdin] is empty, and [stdout], [stderr] and [Eio.traceln] are kept in
      memory: a report shows the last 2 KiB the program printed;
    - [domain_mgr] runs its work in the calling fiber, on the run's own
      scheduler;
    - [process_mgr] starts nothing: [spawn] and [pipe] raise [Eio.Io] with
      [Not_available];
    - [debug] is Eio's, [backend_id] is ["chaos"], and [chaos] is the run's
      {!Eio_chaos.t}, for a device the program builds over others.

    A program that reaches the system other than through [env], by [Unix], the
    standard library's channels, [Random.self_init], [Domain.spawn] or
    [Sys.command], escapes the check. Built with
    {{:https://github.com/samoht/ocaml-hermetic}hermetic} ([open! Hermetic]),
    such a call does not compile, so what differs between two runs can only be
    state the program kept in memory from an earlier one. Without it, a direct
    call escapes, and when it makes two runs with no fault differ in the kinds
    of operation they make, the check fails with a report that names the first
    difference and its likely causes. *)

(** {1:program Testing a program} *)

val run :
  ?os:[< Eio_chaos.os ] ->
  ?budget:int ->
  ?seed:int ->
  ?replay:string ->
  ?setup:(env -> unit) ->
  ?inspect:(env -> unit) ->
  (env -> unit) ->
  unit
(** [run ~os ~budget ~seed ~replay ~setup ~inspect app] runs [app] once with no
    fault on an in-memory filesystem that answers as [os] (defaults to [`Linux])
    does and holds what [setup] (defaults to doing nothing) made there. It then
    runs [app] again many times, each time from that state, with one fault:

    - an open, a read, a write, a truncation, a sync or a directory change
      fails, with each error Linux gives there: [EACCES] or [EROFS] on an open
      that may write or a directory change, and [ENOSPC] on one that makes a
      file or an entry, [EACCES] or [EIO] on an open for reading or a stat,
      [ENOSPC] or [EIO] on a write, [EIO] on a read, a truncation or a sync;
    - a sync fails with [EIO] and the page cache drops the pages it could not
      write: a later read of them sees what the disk holds;
    - a read or a write moves a single byte;
    - a write is torn: half its bytes land, then it fails with [ENOSPC];
    - from one operation on, the filesystem is read-only, full, or denies every
      access;
    - the power is cut after an operation that changes the disk. The disk keeps
      what was synced and may lose the rest, as {!section-model} says.

    After each of those runs, [run] runs [app] again with no fault, on what the
    filesystem then holds, as a machine restarts a service. After a power cut,
    that is what the disk kept.

    {b How the faults are picked.} [run] tries every fault above at every
    operation of its first run, but of the many reads and writes of one file it
    takes the first three, the last and one of each size. Then it draws more
    runs from [seed] (defaults to [0]) within [budget] (defaults to [20_000]),
    as {!section-exploration} says.

    {!Fault.Swap_symlink} is not among the faults: it fails ordinary code that
    stats then opens a path in a directory no attacker can write.
    Security-sensitive code asks for it with {!wrap}.

    {b When it passes.} [run] passes iff:

    - the first run returns;
    - no run hangs or raises an exception other than [Eio.Io], an assertion of
      [app] included. A power cut ends the run where it falls;
    - after each fault, each file the program {{!ack}acknowledged} and has not
      changed since holds what it held then, and [inspect] (defaults to doing
      nothing), called on the filesystem as a fresh process, returns;
    - each restart returns.

    An [Eio.Io] error that escapes a run with a fault, from one fiber or
    several, neither passes nor fails it: the disk did fail, so what decides is
    the state the run left, which the last two rules check. A restart alone
    cannot see lost data, as the program writes it again: {!ack} is how the
    program says what it promised, and [inspect] checks what only the test
    knows, such as the records a log must still hold.

    The filesystems live in memory and their clocks jump to the next timer, so a
    check of a small program takes milliseconds.

    @raise Failure
      with a report of the first failing run: the fault, the operations of the
      run, the error, what the program printed, and the line to paste that runs
      that case again, as
      [Eio_fs_chaos.run ~replay:"fs/7a2e6ba7/none|end|drop" app]. [replay]
      reruns that one case, and its restart, whatever the [seed] that found it.
      To find it, a replay may first run [app] a few times, so the failing run
      is the last one.
    @raise Invalid_argument
      if [budget] is not positive, or [replay] is not a replay string of [run].
*)

val disk : unit -> Eio_chaos.device
(** [disk ()] is the disk of {!run} as a device of {!Eio_chaos.run}, to test a
    program on it and other simulated devices at once: an in-memory filesystem
    that answers as the [~os] of the check does, with the faults and power cuts
    above. [run app] is [Eio_chaos.run [ disk () ] app], but for the name its
    reports give. *)

(** {1:conformance Testing a filesystem} *)

val conformance :
  ?os:[< Eio_chaos.os ] ->
  ?user:Eio_fs_mem.user ->
  ?under:Eio_chaos.device list ->
  ?budget:int ->
  ?seed:int ->
  ?replay:string ->
  (sw:Eio.Switch.t -> env -> _ Eio.Path.t) ->
  unit
(** [conformance ~os ~user dir] runs each scenario recorded on the kernel of
    [os] (defaults to [`Linux]) in a directory of mode [0o755] it makes in a
    fresh, empty directory [dir ~sw env], opened as a sandbox, as the recorder
    ran it below its current directory, and compares each answer with the
    kernel's. [sw] ends with the scenario, so what [dir] opens under it is
    closed there.

    Each scenario runs in an event loop of its own, as a run of {!run} does,
    given an {!type-env} whose [fs] is a fresh in-memory filesystem that answers
    as [os] does. A filesystem built on other capabilities, such as a wrapper
    over a directory, takes them from that env, so its runs are as deterministic
    as a program's. A directory on the host works too: its calls run one at a
    time in the scenario's loop.

    {[
    let check_mem () =
      Eio_fs_chaos.conformance (fun ~sw:_ _env -> Eio_fs_mem.v ())
    ]}

    A scenario is a short sequence of {!Eio.Path} operations: names that differ
    by case or Unicode form, names and paths at their length limits, opens of
    each kind, symbolic links and their chains, renames, removals, and the
    permission rules a user meets. An answer is a result (the bytes read, the
    entries listed, a file's kind, size or mode) or an error, which compares by
    the class Eio gives it ([Not_found], [Already_exists], [Permission_denied]
    and so on), else by its errno.

    A report writes each operation as the traces do, a verb and its arguments,
    paths relative to the scenario's directory:

    - [write p d] saves [d] in [p], creating or truncating it, and [read p]
      loads [p]; [readd p] is one read of up to 16 bytes, and [pwa p d off]
      writes [d] at [off] in [p] opened for appending;
    - [open p flags] opens [p] for reading with [r], else for writing, with [x]
      exclusive, [t] truncating, [c] creating if missing and [a] appending, and
      not following a final link with [n];
    - [opendir p], [lsd p] (lists [p] opened as a directory) and [ls p];
    - [mkdir p [mode]], [unlink p], [rmdir p], [rename a b], [symlink t p],
      [readlink p], [chmod p mode], [lchmod p mode] and [chgrp p], which gives
      [p] the user's supplementary group;
    - [stat p] and [lstat p] answer the kind and size, [mode p] the mode, and
      [group p] whether the group is the user's own, the supplementary one or
      another;
    - [chain n] makes [n] links [l1] to [ln], each to the one before, the first
      to [f];
    - [hold p] and [holddir p] keep [p] open as a file or a directory until
      [close p] or the scenario's end, [cd p] opens [p] as a directory that
      confines the operations after it, and [mkdirs p] is {!Eio.Path.mkdirs};
    - [p{s}n] is [p] followed by [s] repeated [n] times, [\NNN] the byte of
      decimal code [NNN], and [@p] is [p] under the scenario's directory by its
      absolute path.

    The answers were recorded as a user other than root, with the umask [0o022]
    and a supplementary group. [user] is the {!Eio_fs_mem.type-user} [dir]'s
    operations run as: with it, every scenario runs, and a scenario that changes
    a file's group changes it to the first of [user]'s supplementary groups.
    Without it, the scenarios that need a user other than root, or a
    supplementary group, are left out. {!Eio_fs_mem.v} passes for the [os] it is
    given.

    A scenario that names a path by its absolute spelling, as a link's target,
    takes it from {!Eio.Path.native} of its directory; where that has none, as
    on {!Eio_fs_mem} but under [`Windows], the scenario is left out.

    On Windows only eio_windows was recorded, as OCaml's [Unix] reaches the C
    runtime there, not the kernel: the answers are eio_windows 1.7's on NTFS, as
    an administrator without [user], and as a basic user, who may not make a
    symbolic link, with it.

    Where Eio defines an answer the kernel cannot give, as for a path Eio
    resolves one component at a time, the expected answer is the one eio_posix
    gave on that kernel. eio_posix itself does not pass: it resolves a trailing
    slash, ["."] and [".."] itself, where the kernel answers otherwise, and
    differs on 13 of the 115 scenarios on Linux and 21 on macOS. A filesystem
    that wraps eio_posix differs there too.

    [conformance] runs every scenario, whatever the earlier ones answered. In a
    scenario it stops at the first answer that differs, as the later ones depend
    on it. It blocks its caller, as {!run} does.

    {b Over other devices.} A filesystem built over other devices, as a remote
    filesystem over a network, takes them from its {!type-env}: [under]
    (defaults to none) are the devices it holds, with a quiet {!disk} as its
    [fs] unless one of them is a disk. Its scenarios first run as above, each on
    healthy devices. Then, if [under] is not empty, [conformance] is
    {!Eio_chaos.run} of a program that runs every scenario in turn, in one event
    loop, with [budget], [seed] and [replay] as {!run} takes them, and the
    faults the devices of [under] allow, but no power cut. It passes iff each
    run does:

    - until a device fails, every answer is the kernel's;
    - once one failed ({!Eio_chaos.faulted}), an answer is the kernel's, or an
      error eio-fs injects, which the fault below may explain: a missing file, a
      refused access, a read-only or full filesystem, or an input/output error.
      At the first such error the scenario ends, as the later answers depend on
      whether the operation took effect, and so does one whose directory cannot
      be made;
    - the restart, with no fault, runs every scenario again, each in a new
      directory, and every answer is the kernel's.

    So a hang fails, an error of another device that leaks, such as
    [Connection_reset] or [End_of_file], fails, and so does a success that
    differs from the kernel's, as a short read given as the whole file. A device
    that turns a harmless fault below into an error of its own passes. A report
    is that of {!run}, named after [conformance], and its replay line runs that
    case alone.

    @raise Failure
      if an answer differs, with a report ({!Eio_chaos.fail}): a table with a
      row for each such scenario, its name, its operations up to that one, the
      answer expected and the one given, as [case lookup], [write a x; stat A],
      [file 1] and [ENOENT]. What was given is marked [*] where eio_posix fails
      that scenario too, and what was expected is marked with a note that says
      why when it is eio_posix's answer rather than the kernel's. Then the
      notes, and how many scenarios passed, differed and were left out, and why.
    @raise Invalid_argument
      if [user] is root, its umask is not [0o022], or it has no supplementary
      group. *)

(** {1:blocks Building blocks} *)

type Eio.Exn.Backend.t +=
  | Injected_fault of string
        (** The backend code carried by the errors a wrapper raises, the
            [Not_found] and [Permission_denied] of a refused access included:
            the errno's name and the path, as ["EIO /f"]. So an injected error
            is an {!Eio.Io} error of the shape a disk's error has. *)

(** The operations a wrapper sees. *)
module Op : sig
  type kind =
    [ `Open_read  (** An open for reading, of a file or a directory. *)
    | `Open_write  (** An open that may write or create the file. *)
    | `Stat  (** A stat, of a path or of an open file. *)
    | `Read  (** A read of a file's bytes. *)
    | `Read_dir  (** A listing of a directory's entries. *)
    | `Write  (** A write of a file's bytes. *)
    | `Truncate  (** A truncation of a file; [offset] is its new length. *)
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
            file [f] at the root of {!Eio_fs_mem.v}. A file operation names the
            path the file was opened by. *)
    nth : int;
        (** The operation's place among those of its kind on its path, from [1].
        *)
    rank : int;
        (** The operation's place among those of its kind on any path, from [1].
            A check aims its faults by [kind] and [rank]. *)
    offset : int;
        (** Where a read or a write starts, a truncation's new length, [0]
            otherwise. *)
    length : int;  (** The bytes a read asks for or a write offers. *)
    dest : string option;
        (** A rename's destination, as given to the directory it lands in. *)
  }
  (** The type for operations. *)

  val pp : t Fmt.t
  (** [pp] formats an operation for logs and reports, as [#4 write /f@0+5]: its
      {!index}, its kind and its {!path}, then, for a read or a write, [@] its
      {!offset} and [+] its {!length}. A truncation ends with [to] its new
      length, and a rename with [to] its {!dest}. *)
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
            could not write: under {!Power.wrap}, a later read of them sees the
            bytes the disk holds. Elsewhere it is {!constructor-Fail}. *)
    | Short of int  (** A read or a write moves at most [max 1 n] bytes. *)
    | Torn of int
        (** A write stores its first [n] bytes, then fails with [ENOSPC]. *)
    | Swap_symlink of string
        (** An open or a stat runs, then its path becomes a symbolic link to the
            target: the window a time-of-check to time-of-use attack uses. Code
            that must not follow a link an attacker plants tests it this way:
            {[
            let swapped base =
              let swap (op : Op.t) =
                if op.kind = `Stat && op.path = "/out" then
                  Some (Fault.Swap_symlink "secret/key")
                else None
              in
              Eio_fs_chaos.wrap swap base
            ]} *)
    | Cancel of int
        (** A read or a write of a file is cancelled [n] turns of the event loop
            after it starts, or before it starts if [n <= 0]. If the cancel
            stopped it, it runs again, so the program sees it complete as with
            no fault. The wrapper checks the cancel against [base] and raises
            [Failure] on a breach.

            A read that returned [k] bytes wrote none of its buffer past them. A
            read the cancel stopped wrote none of it, then, in the turns that
            follow, or by the file's close. A write the cancel stopped is made
            again from the caller's buffers, after the copy it was given is
            overwritten with noise. A file that holds noise or a byte twice was
            written by a write that raised [Eio.Cancel.Cancelled].

            {!run} does not try it. *)

  val pp : t Fmt.t
  (** [pp] formats a fault for logs and reports. *)
end

val wrap : (Op.t -> Fault.t option) -> _ Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
(** [wrap f base] is [base] with [f] asked about each operation before it runs.
    The operation meets the fault [f] answers. [f] may also act: count, log,
    change [base], or cut the power with {!Power.cut}. That is how a test
    measures the reads a scan makes, or forces an interleaving at one operation.
    [base] may be in memory or on disk. Files and directories opened through the
    wrapper stay wrapped.

    Injected errors are ordinary {!Eio.Io} exceptions: the code under test
    cannot tell them from those of a disk. *)

val ack : env -> _ Eio.Path.t -> unit
(** [ack env file] records that the program has promised that [file] is durable
    as it is now, as a database acknowledges a commit. [file] is a path of
    [env#fs] or [env#cwd]. After each fault, before the restart, {!run} compares
    each acknowledged file with what it held at its last [ack]. A file the run
    opened for writing, wrote, truncated, renamed or removed since, or whose
    directory it renamed or removed, is not compared: [inspect] judges it. An
    [ack] after a power cut does nothing.

    {[
    let app env =
      let config = Eio.Path.(env#fs / "config") in
      Eio.Path.save ~create:(`Or_truncate 0o644) config "new";
      Eio_fs_chaos.ack env config
    ]}

    This [app] syncs neither [config] nor its directory, so it fails the check:
    a power cut loses the write, and [config] no longer holds ["new"].

    @raise Invalid_argument
      if [env] is not that of a run of {!run}, or [file] is not a regular file.
*)

val acked : env -> string list
(** [acked env] is the paths {!ack} recorded, in the order of their first
    {!val-ack}, each from the root, as ["/config"]. Called from [inspect], it is
    those of the run with a fault that came before.

    @raise Invalid_argument if [env] is not that of a run of {!run}. *)

(** A power cut at a place the test chooses.

    {[
    let cut_at_seventh base =
      let power = Power.v Lose_unsynced in
      let hook (op : Op.t) =
        if op.index = 7 then ignore (Power.cut power);
        None
      in
      Eio_fs_chaos.wrap hook (Power.wrap power base)
    ]}

    A cut keeps what the program made durable, and may lose the rest:

    - a write is durable once {!Eio.File.sync} runs on its file;
    - a sync of a directory makes its own changes durable, a rename into or out
      of it whole, and the earlier changes to each directory they touch;
    - of the rest, a cut may keep some writes and a prefix of the directory
      changes, in program order;
    - a failed sync loses the file's unsynced writes, as Linux does.

    This is ext4 with [data=writeback]. The {{!model}disk model} at the end says
    which states a cut may leave, sector by sector.

    {b macOS.} Under [~os:`Macos] the disk is APFS's, on a drive with a cache:

    - {!Eio.File.sync} and the portable barriers of [Eio_fs.Sync] are fsync(2),
      which stops in that cache: alone, they make nothing durable;
    - the barriers of [eio-fs.unix] ([Eio_fs_unix.sync_file],
      [Eio_fs_unix.sync_dir], [Eio_fs_unix.barrier]) are [F_FULLFSYNC]: each
      makes its file or directory durable, and every fsync(2) before it, of any
      file;
    - a failed fsync(2) loses only the writes no earlier one flushed;
    - the rest is as on Linux, as no source rules it out on APFS, so a cut may
      leave a state APFS would not.

    {b Windows.} Under [~os:`Windows] a cut keeps what it keeps on Linux:
    {!Eio.File.sync} is FlushFileBuffers, and a directory barrier counts as
    Linux's, though eio_windows syncs no directory. What NTFS keeps of a
    directory on a power cut is not modelled: only a power-cut test on real
    Windows could tell it. *)
module Power : sig
  type change
  (** The type for a write or a directory change that no sync has made durable.
  *)

  val pp_change : change Fmt.t
  (** [pp_change] formats a change for logs and reports. *)

  (** The type for what a cut does with the changes not yet synced. *)
  type policy =
    | Lose_unsynced
        (** The disk holds what the last syncs made durable, with the
            truncations made before a directory change they made durable. *)
    | Keep of (change -> bool)
        (** The disk also holds the changes for which the predicate holds, as
            far as the {{!model}disk model} allows. [Keep (fun _ -> true)] is a
            killed process: the disk keeps what the program saw. *)

  type t
  (** The type for power switches. *)

  val v : ?os:[< Eio_chaos.os ] -> policy -> t
  (** [v ~os policy] is a power switch whose cut leaves what [policy] says, as
      the disk of [os] (defaults to [`Linux]) allows. *)

  val wrap : t -> _ Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
  (** [wrap t base] is [base] with its writes, syncs and directory changes
      tracked by [t]. What [base] holds when [wrap] is called is durable. [base]
      may be in memory or on disk; a hole in a file on disk counts as written
      zeros.

      A change that reaches [base] other than through the wrapper, by name or
      from a process the program started, is kept by a cut as durable: the
      wrapper sees no other process's syncs. At or below a path the wrapper
      changed, the policy decides, starting from what [base] held there when the
      wrapper first changed it. Stop those processes before calling {!cut}.

      @raise Invalid_argument if [t] already wraps a directory. *)

  val unsynced : t -> change list
  (** [unsynced t] is the changes not yet durable, in program order. *)

  val cut : t -> Eio.Fs.dir_ty Eio.Path.t
  (** [cut t] cuts the power, if [t] has not cut it yet, and is the directory
      given to {!wrap}, which now holds what the disk kept. An entry the cut
      spared keeps its inode and, on an in-memory base, the times of its version
      the disk kept. Every later operation through the wrapper fails. Called
      from the function given to {!Eio_fs_chaos.wrap}, it cuts the power before
      that operation, in the middle of a run.

      @raise Invalid_argument if [t] wraps no directory. *)
end

(** {1:exploration Exploration}

    A check runs the program once with no fault and records its operations. Its
    scenarios are positions in that run, in two tiers:

    - the linear tier always runs whole. On each file it takes the first three
      reads and writes, the last, and the first of each length class, a length
      rounded down to a power of two, with the syncs that follow them, and every
      other operation: it grows with the structure of the program, not with its
      bytes. There it tries each fault alone, and each power cut in its two
      simplest states: everything unsynced lost, and everything kept;
    - the sampled tier spends a fortieth of the budget, 500 runs by default, or
      half as much as the linear tier if that is more: the other positions,
      pairs of faults, power cuts during a restart, and the other states a power
      cut may leave. A first fault that changes what the run writes gets its
      second faults first, on the operations that follow it, those that change
      the disk first: that is where a retry or a recovery path lives. When it
      has fewer runs than scenarios of a kind, it splits them, in order, into as
      many groups of neighbours as it has runs, and runs one scenario of each
      group, drawn from a seed. Each draw may land on any scenario of its group,
      so a pattern that repeats along the scenarios, as the faults tried at each
      operation do, hides no kind of fault from it, where taking every k-th
      scenario may meet only one. A larger budget tries more only once a
      fortieth of it passes half the linear tier, so raising it a little may
      change nothing.

    A check never passes on a partial linear tier. One that needs more runs than
    its budget fails and says so: shrink the program or raise [~budget]. The
    first run stops as soon as the tier needs more, so a program that polls for
    ever costs no more than the budget allows, and the report shows its trace up
    to there. *)

(** {1:contracts Contracts}

    {b At most two faults per run, or one lasting condition.} A program that
    tries an operation three times gets past two faults, but no retry gets past
    a lasting condition: [app] must accept that any operation fails, and may
    demand only that the program handles it.

    {b A lasting condition reaches every operation.} A filesystem that turns
    read-only, full, or denies access does so for every operation from then on,
    the retries included. A program that retries at once spins, and a run fails
    as a hang when, with no success in between and its clock still but for the
    time operations take:

    - one operation on one path fails 10,000 times more than three times the
      first run made it;
    - or one kind of operation in one directory fails 10,000 times more than
      1000 times the first run made it.

    A batch that tries each item up to three times and drops what fails passes,
    and so does a pool that tries up to 1000 names for each name its first run
    made. No check can tell a spin from a loop that ends later, so one that
    tries more often than that is taken for a spin.

    {b A restart is a fresh process.} It runs on the directory as the fault or
    the disk left it, and so does [inspect]: neither may read state an earlier
    run left in memory.

    {b Each run makes the same kinds of operation.} A check aims a fault at an
    operation by its kind and its rank among the operations of that kind, so the
    names a program gives its files may change from run to run, and a report
    says where two runs with no fault named different files. Two runs with no
    fault that differ in the kinds of operation they make, or in their order,
    fail the check: {!type-env} says what can make them differ.

    {b Do not catch [Eio.Cancel.Cancelled].} A power cut raises it from the
    operation it falls before, and from every one after, as in a cancelled
    fiber, and once every fiber waits it cancels them all, as nothing runs after
    a power cut. A run that makes 1000 more operations after the cut fails as a
    hang. *)

(** {1:replay Reports and replay}

    A check is deterministic. A failing run raises [Failure] with:

    - the scenario;
    - the operations of the failing run, each numbered by its place in the
      trace, which the cut, the faults, the changes a cut lost and the
      acknowledgements name it by;
    - for a power cut, after which operation each file was acknowledged, and,
      for a state drawn among those the cut allows, what each file holds: its
      length, and each run of sectors that holds a write, zeros or stale data,
      with where the stale data comes from;
    - what the program printed, its last lines;
    - the [Eio_fs_chaos.run ~replay:"..." app] call that runs that scenario
      again, which passes [~os:`Macos] for a check under macOS.

    A report's size is bounded by the program's structure, not by how long it
    spun:

    - a block of up to eight operations repeated three times or more, in the
      same directories each time, as a retry makes them, shows its first and
      last instances;
    - a longer trace keeps its first and last hundred lines and those around its
      first fault;
    - a run keeps only those lines as it goes, and so does its memory.

    A replay:

    - finds its scenario again by the kind and rank of the operations it names
      when the program's first run has changed, and its report says so;
    - runs the program a few times first when the scenario is a second fault or
      a cut during a restart, as its runs say where those fall: the failing run
      is the last one;
    - fails if the check no longer has that scenario, or if it names a state
      drawn at random of a cut that allows two states or fewer, which the check
      never draws;
    - of a state drawn at random under a larger budget needs that budget again,
      which the refusal names.

    The report of a check run inside another is text in the outer one.

    The sampled tier draws its scenarios from a seed, the same one on every
    check unless [~seed] gives another, so a check meets the same faults each
    time. A replay names its scenario, not the draw that found it, so it needs
    no seed.

    A check reads no environment variable and prints nothing. A test that wants
    a larger budget at night, or other faults in the sampled tier, reads its
    own:
    {[
    let check app =
      let int name = Option.map int_of_string (Sys.getenv_opt name) in
      Eio_fs_chaos.run ?budget:(int "CHAOS_BUDGET") ?seed:(int "CHAOS_SEED") app
    ]} *)

(** {1:runs Runs}

    Each run, with its restart, has its own event loop, inside or outside
    another one. A check runs to its end before it returns, so it blocks the
    event loop it is called from: the other fibers of that loop wait for it.

    Each run's clocks start at [0.] and move to the next timer when every fiber
    waits, so a wait takes no time. Each turn, a fiber resumed by a yield,
    another fiber or a timer, also moves them by 1 us, as CPU time passes
    between two. Five guards stop a run that does not end:

    - a run in which every fiber waits and no timer is due fails as a hang;
    - a retry that never yields meets the spin guard above;
    - a run that makes 100,000 filesystem operations fails as a busy loop, or as
      too large for a check, so a run's operations stay bounded: a poll that
      never yields stops there;
    - a run stops at 10,000,000 turns. If no fiber waited 1 ms or more for a
      timer in the last 10,000,000, it fails as a busy loop: a poll by yields,
      or by sleeps shorter than 1 ms, that never ends. Otherwise it fails as a
      busy loop or as too large for a check: a check repeats the run hundreds of
      times, so it refuses a longer one, as it refuses one past its budget.
      Fibers on timers take turns too: a thousand on a heartbeat of 100 ms reach
      10,000,000 in 1000 s of clock;
    - a run still going after an hour of its clock fails as a hang: a poll that
      sleeps 1 ms or more between its tries and never ends.

    An operation takes time, and other fibers run meanwhile: a sync of a file or
    a directory 1 ms, any other operation 1 us, and one that fails none. Its
    effect lands at the end of that time. A sync covers the changes made before
    it: a write, a change of mode or owner, or an entry another fiber makes
    while it runs stays unsynced: Linux's fsync(2) promises nothing of them. *)

(** {1:model The disk model}

    A cut leaves a state ext4 with [data=writeback] may leave. Under
    [~os:`Macos] the same rules hold, but for the syncs {!Power} lists.

    {b Lengths.}

    - A file's length is one it had since its last sync, or the end of a
      4096-byte page a write put bytes in, below a length it had since.
    - A length a write set survives only with every directory change made before
      it: ext4 journals it as it writes the pages back.
    - A length a truncation set survives only with every directory change made
      before it, and whenever a later one survives: ext4 journals it in the
      syscall's own handle.

    {b Pages and sectors.}

    - A 512-byte sector is written whole. It holds what the disk held, zeros
      where it held nothing, or what it held after one of the writes to it, so
      the writes to it land together.
    - A 4096-byte page the disk held nothing of holds bytes only with every
      directory change made before the first write into it, as ext4 allocates
      the page with the length.
    - A truncation zeroes the rest of the page it cuts, sector by sector. At a
      length from before it, a page it frees holds no later write, as ext4
      reuses no block before the truncation commits.

    {b Stale data.}

    - A page the disk held nothing of, past the durable length or in a hole,
      that a write put bytes in is written whole, as ext4 allocates it. Each of
      its sectors within the length may instead hold stale data.
    - Stale data is what a freed block held: a page a synced truncation freed,
      or one of a file the program removed, its unsynced bytes included, as
      writeback may have written them before the removal.
    - ext4 may give a freed block to any offset of any file. The check tries the
      page at the same place first, then the others, up to eight distinct ones,
      and the byte ['7'] when no block was freed.
    - A file the cut lost and the program never removed frees no block, nor does
      a removed file still open at the cut, as ext4 frees its blocks at its last
      close.

    {b Failed syncs.} A failed sync loses the file's unsynced writes, though
    some of their sectors may already be on disk. The page cache may keep them
    or evict them: before the cut, a read after a failed sync sees them as kept,
    or, in a scenario of its own, as the disk holds them. *)

(**/**)

(* For this package's tests: the check's two halves alone, the second with
   a recovery and acknowledged values of its own, the crash model's own
   policies and controls, the law of the wrapper, and counters. *)
module Private : sig
  val scenarios : string list
  (** [scenarios] is the name of each scenario {!val-conformance} runs, in
      order. *)

  val eio_posix_differs : [< Eio_chaos.os ] -> string list
  (** [eio_posix_differs os] is the scenarios whose answers eio_posix gave on
      [os] differ from those {!val-conformance} expects. *)

  val conformance_differing :
    ?os:[< Eio_chaos.os ] ->
    ?user:Eio_fs_mem.user ->
    (sw:Eio.Switch.t -> env -> _ Eio.Path.t) ->
    (string * string) list
  (** [conformance_differing ~os ~user dir] is the scenarios {!val-conformance}
      reports, in order, each with what [dir] got, followed by [" *"] where
      eio_posix differs there too. *)

  val expected :
    ?trace:string ->
    ?basic:bool ->
    [< Eio_chaos.os ] ->
    string ->
    string list option
  (** [expected ~trace ~basic os name] is the answers {!val-conformance} expects
      of the scenario [name] on [os], as a basic user on Windows if [basic],
      read from the CSV text [trace] (defaults to the trace the library holds),
      or [None] if it was not recorded there. A column is found by its name. *)

  val check_faults :
    ?os:[< Eio_chaos.os ] ->
    ?budget:int ->
    ?seed:int ->
    ?replay:string ->
    ?setup:(env -> unit) ->
    (env -> unit) ->
    unit
  (** [check_faults] is {!run} with its errors alone: no power cut. *)

  val check_cuts :
    ?os:[< Eio_chaos.os ] ->
    ?budget:int ->
    ?seed:int ->
    ?replay:string ->
    ?setup:(env -> unit) ->
    (ack:('a -> unit) -> env -> unit) ->
    recover:(acked:'a list -> env -> unit) ->
    unit
  (** [check_cuts run ~recover] is {!run} with its power cuts alone: [run]
      acknowledges values, and [recover], which restarts it, is told those
      acknowledged before the cut. *)

  module Crash = Crash

  (* It hands out nothing a schedule's holder could not reach by running an
     operation through a wrapper. *)
  module Law = Law
  (** What {!wrap} does with each operation of each interface it binds. *)

  module Internal : sig
    val descriptors : _ Eio.Path.t -> int option
    (** [descriptors path] is how many descriptors are open on the in-memory
        filesystem of [path], or [None] if [path] is not in one. *)

    val replay_copied : unit -> int
    (** [replay_copied ()] is how many bytes {!Crash} has copied into the images
        it replays a file's changes over, since the program started. *)
  end
end

(**/**)

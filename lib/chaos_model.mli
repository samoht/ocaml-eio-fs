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

type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]
(** The type for the POSIX error codes an injected fault raises. An
    {!Fault.t.Open_error}, a {!Fault.t.Change_error}, or a {!Fault.t.Stat_error}
    on a stat of a path, raises {!Eio.Fs.Not_found} for [`Enoent] and
    {!Eio.Fs.Permission_denied} for [`Eacces] and [`Eperm]; every other injected
    error, a stat of an open file included, is an {!Eio.Io} whose message names
    the code. [`Eacces], [`Eperm] and [`Erofs] (a read-only filesystem) are for
    code that assumes it can always write: a directory it cannot create in, a
    read-only mount, a file another user owns. *)

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
    | `Change_error
    | `Swap ]
  (** The type for fault families: one per constructor of {!t}, named after it,
      and [`Swap] for {!constructor-Swap_symlink}. A seeded schedule never
      generates [`Swap]: a swap needs a target it cannot draw. *)

  (** The type for injected faults. A fault fires on one operation; one that
      does not apply to that operation's kind injects nothing. *)
  type t =
    | Short_read of int
        (** Deliver at most [max 1 n] bytes of the requested read, forcing the
            caller to loop. A read of an empty buffer is left whole. *)
    | Read_error of code
        (** Deliver nothing and raise {!type-code}, on a read or a directory
            read. *)
    | Short_write of int
        (** Accept at most [max 1 n] bytes of the offered write, forcing the
            caller to loop. *)
    | Torn_write of int
        (** Commit the first [n] bytes of the write (none if [n <= 0], all of
            them if [n] exceeds its length) to the base, then raise [`Enospc]:
            bytes reach the store but the operation reports failure. *)
    | Write_error of code  (** Commit nothing and raise {!type-code}. *)
    | Sync_error of code  (** Fail a sync with {!type-code}. *)
    | Open_error of code
        (** Fail an open or a directory open with {!type-code}. *)
    | Stat_error of code  (** Fail a stat with {!type-code}. *)
    | Change_error of code
        (** Fail a change to a directory's entries (a [mkdir], [symlink],
            [unlink], [rmdir] or [rename]) or to an entry's mode or owner (a
            [chmod] or [chown]) with {!type-code}, before it is made. A seeded
            schedule draws [`Eacces], [`Eperm] or [`Erofs]. *)
    | Swap_symlink of string
        (** Perform an open or a stat, then replace its path with a symbolic
            link to the given target: the check-to-use window a TOCTOU exploits.
            On a stat, the stat sees the original file while the following open
            resolves to the link; on an open, the caller keeps its handle while
            the path afterwards answers about the target. *)

  val equal : t -> t -> bool
  (** [equal f0 f1] is [true] iff [f0] and [f1] are the same constructor with
      equal arguments. *)

  val compare : t -> t -> int
  (** [compare] is a total order on faults compatible with {!equal}: by
      constructor in declaration order, then by argument. *)

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
    | `Truncate
      (** A truncation of a file; its [file_offset] is the new length. It
          accepts {!Fault.t.Write_error}, which no seeded schedule injects. *)
    | `Sync
      (** A sync: a file's, or a directory's through [Eio_fs_sync.sync_dir],
          whose path is the directory. *)
    | `Open_read  (** An open for reading, of a file or a subdirectory. *)
    | `Open_write  (** An open that may write or create the file. *)
    | `Stat  (** A stat, of a path or of an open file. *)
    | `Change
      (** A change to a directory's entries ([mkdir], [symlink], [unlink],
          [rmdir], [rename]) or to an entry's mode or owner ([chmod], [chown]).
          Its path is the entry made, removed or changed, a rename's source. *)
    ]
  (** The type for operation kinds. *)

  type t = {
    kind : kind;  (** What the operation does. *)
    path : string;
        (** The path the operation names, as given to the wrapped directory it
            went through; for a file operation, the path the file was opened by.
            A scripted rule matches it exactly as spelt: through the root
            {!Fs.v} returns, whose path is ["/"], a file is ["/f"]. *)
    file_offset : int;
        (** The byte offset of a read or a write, and [0] otherwise. *)
    length : int;
        (** The bytes a read requests or a write offers, and [0] otherwise. *)
    space : bool;
        (** Whether the operation may fail for want of room: a write, an open
            that makes its file, a [mkdir], a [symlink] or a [rename]. An
            [`Enospc] fault on another open or change injects nothing, as Linux
            never answers it there. *)
    dest : string option;
        (** A rename's destination, as given to the directory it lands in. *)
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
            counting reads, directory reads, writes, syncs, opens, stats and
            directory changes). *)
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

  val hook : (Op.t -> Fault.t option) -> t
  (** [hook f] injects the fault [f] answers for each operation. *)

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
  (** [pp] formats a schedule for logs. A seeded schedule prints as the OCaml
      expression that rebuilds it; a scripted one prints how many of its rules
      are pending. *)
end

val wrap : Schedule.t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [wrap schedule base] is [base] with operations perturbed by [schedule];
    opened files and subdirectories stay wrapped and share [schedule].

    The wrapper and every file and subdirectory it opens hold [base] out of
    reach: each is a value of its own bound to the wrapper's interfaces only, so
    code handed the wrapper cannot rebuild a handler that reaches [base] and
    steps around [schedule]. It answers {!Eio_fs_sync.Process_local} and
    {!Eio_fs_sync.Modeless} as [base] does, and every opened file, a read-only
    one included, registers {!Eio_fs_sync.File_sync} so a barrier through it
    reaches the schedule's own sync. A backend's own interfaces are not carried
    over: a wrapper over a host directory is no native working directory for a
    child process, and its files hand out no host descriptor, except to
    [eio-fs.unix], which takes a lock on the descriptor beneath. A rename from
    an in-memory directory of {!Fs} into a wrapper over the same tree is the
    in-memory rename, outside [schedule]; one from a host directory of another
    backend into a wrapper is refused by that backend, which finds no directory
    of its own there. *)

(** A power-loss durability model over a base directory capability.

    Every write is tracked as un-synced until a successful {!Eio.File.sync}
    makes that file's prior writes durable. A file is known by its inode, not
    its path: a sync through any name of it, through a symbolic link, [.] or
    [..], or through a read-only handle, is a sync of that file, and its pending
    writes follow it through a rename of itself or of a directory above it.

    {b A failed sync loses the file's pending writes.} Linux marks the pages
    whose writeback failed clean without writing them, so a later sync that
    succeeds does not make them durable. A writeback may fail only in part, and
    the pages that succeeded are on disk: {!Crash.Reorder_unsynced} keeps any
    subset of the blocks failed syncs lost, each as one of those failed
    writebacks left it, and the other policies keep none. A truncation is not
    lost: ext4 journals it as metadata, so it stays pending and a later sync
    makes it durable. The page cache may still hold their bytes, and a later
    change that dirties one of those blocks then writes it whole, lost bytes
    included; or it may have evicted the page, and the later change lands over
    the bytes on disk. {!Crash.Reorder_unsynced} draws either for each file,
    whether a later sync ran or not; the other policies keep the page cache's.
    Before the failure the lost writes are pending like any write; after it, no
    crash shows them but through such a block. This holds for a sync failure
    {!Chaos.wrap} injects above the model and for one the base raises beneath
    it. The only safe recovery is to write the data again from the application's
    own copy. A crash rewrites the base to a post-crash image in which un-synced
    writes may be dropped or reordered, which demonstrates the classic
    missing-{!Eio.File.sync} hazard.

    {b Directory operations are journaled.} Creation, [mkdir], [unlink],
    [rmdir], [symlink], [rename], [chmod] and [chown] enter a metadata journal
    in program order; {!Crash.sync_metadata} commits it whole, and a directory
    barrier (a sync of the directory, opened as a file or not) commits only the
    entries of that directory and a [chmod] or [chown] of the directory itself,
    not one of an entry's. That is a deliberate weakening below ext4, whose
    barrier commits whole transactions in order: a child's [chmod] can be lost
    behind a sibling entry the barrier committed, which ext4 would not do and
    POSIX allows. A setuid clear, by contrast, is carried by any barrier that
    commits a later entry. A sync of a file also commits a [chmod] or [chown] of
    it. A write or a truncation that clears setuid or setgid enters the journal
    at once, at its place in program order, as ext4 journals the change in the
    running transaction: a sync of the file commits it, as does
    {!Crash.sync_metadata} and any barrier that commits a later entry, and at a
    crash it survives with the prefix it is part of. A length the write, or a
    later write or truncation of the file, sets lands only with or after the
    clear, as ext4 journals both in program order; data that overwrites within
    the length may land without it, as ext4 allows. A barrier on a directory
    that is not itself durable yet holds from the moment it is, or from the
    moment another directory the entry changes is: with [d/f] durable,
    [mkdir e; rename d/f e/f; fsync_dir e] makes the rename durable when the
    barrier returns, so a crash before [e] itself is durable loses the file from
    both directories; so does [fsync_dir d] in place of [fsync_dir e]. ext4
    would keep [e/f], as committing the rename commits the [mkdir]; eio-fs
    deliberately weakens this, so that a missing barrier on [e]'s parent shows.
    The model knows files and directories by inode, as ext4 does: a barrier
    holds for its directory under whatever name it has later, and never for
    another directory of the same name. A barrier through a directory opened
    before it was removed or replaced commits that directory's own entries, and
    nothing of the one now at its name. Committed entries take effect in program
    order, whatever order their barriers ran in, and a barrier also commits the
    pending entries an entry it commits needs: the earlier ones in a directory
    it changes, as one directory's entries stay in order, the ones that emptied
    a directory it removes or a rename replaces, and, for a rename, the renames
    that moved a directory above where it lands, so a directory never lands
    inside itself. The entry that made a directory is not among them: only a
    barrier on its parent makes a new directory durable. Each entry takes effect
    whole or not at all: a creation whose directory the image does not hold
    makes nothing, and an rmdir of a directory the image still fills, or a
    rename over one, changes nothing. A rename moves the node the image holds,
    with what the image holds below it; a node made by a creation that made
    nothing still exists for a committed rename of it, which puts it where it
    moved, as its inode exists on disk; moved into a directory the image does
    not hold, a node leaves the image until a later committed rename brings it
    back. At a crash the policy decides how much of the uncommitted suffix
    survives, always a prefix of program order, and a dropped operation is
    rolled back whole; an entry a deferred barrier covers survives, with the
    entries it needs, whenever it would hold. *)
module Crash : sig
  (** The type for the kinds of un-synced change a journal entry records. *)
  type op_kind =
    | Data  (** A write; its offset and length locate it. *)
    | Truncate  (** A truncation; its offset is the new length. *)
    | Create  (** The open that created the file. *)
    | Mkdir  (** A new directory. *)
    | Unlink  (** A removed file or symbolic link. *)
    | Rmdir  (** A removed directory. *)
    | Symlink of { target : string }  (** A new symbolic link to [target]. *)
    | Rename of { dest : string }  (** An entry moved to [dest]. *)
    | Chmod of { perm : int }  (** A new mode. *)
    | Chown of { uid : int64; gid : int64 }  (** A new owner. *)

  type unsynced = {
    path : string;
        (** The path the change was made at: for a write or a truncation, the
            file's path with its symbolic links resolved; for a directory
            operation, the path as the caller gave it. *)
    kind : op_kind;  (** What the change is. *)
    offset : int;
        (** The byte offset of a {!Data} write, the new length of a {!Truncate},
            and [0] for a directory operation. *)
    length : int;
        (** The byte length of a {!Data} write, and [0] for a {!Truncate} or a
            directory operation. *)
    seq : int;  (** The change's position in program order. *)
  }
  (** The type for un-synced changes. *)

  (** The type for how the post-crash image is rebuilt from the un-synced
      changes. *)
  type policy =
    | Drop_unsynced
        (** The state at the last barriers: every un-synced write is lost and
            every uncommitted directory operation is rolled back. It is one
            crash state among many. It hides the bugs that need a later change
            to land, such as a rename that reaches the disk before the data it
            names; {!Reorder_unsynced} over many seeds finds those. *)
    | Reorder_unsynced of int
        (** Seeded: writes go back to the disk in blocks, as a page cache writes
            them. A write dirties the blocks it covers and, past the end of the
            file, those of the zeroed hole it leaves; a truncation that shrinks
            dirties only the block it cuts mid-way, none when it cuts on a
            boundary, and one that grows dirties the blocks it adds. Each dirty
            block comes back as it was durable or as it stood after any one of
            the changes that dirtied it, drawn independently of the other
            blocks. The length is drawn independently of the blocks: one a
            change left, or a block boundary a write grew the file across. A
            block a shrink did not dirty keeps its old bytes, which show again
            when a later length grows over them, as data=writeback may leave
            them. A shrink the length does not hold has freed no page: at that
            length the pages past the shrink hold no later change, as ext4
            reuses no block before the truncation that freed it commits. So a
            multi-block write may keep any subset of its blocks, while a block
            is never torn and never holds a later write without the earlier
            ones. Uncommitted directory operations survive as a prefix of
            program order whose length is drawn uniformly: metadata never
            reorders. The [int] is the seed; the same seed and workload
            reproduce the same image. *)
    | Survive of (unsynced -> bool)
        (** Scripted: an un-synced write survives iff the predicate holds;
            survivors keep program order. A disk writes a 512-byte sector whole,
            so where a surviving write overwrote bytes the file held, its sector
            holds what the page cache held after it, the earlier overwrites of
            that sector included. Otherwise writes are kept or dropped whole,
            whatever block they share. An uncommitted directory operation
            survives iff the predicate holds for it {e and} every earlier
            uncommitted directory operation survived (the prefix rule). It pins
            an exact scenario, for example keeping a generation pointer but
            dropping the data block it references. *)
    | Choose of (int -> int)
        (** As {!Reorder_unsynced}, with each draw [f n], in [\[0, n)], made by
            the function, so a caller can enumerate the states, and with lengths
            as ext4 leaves them: one a change left, or the end of a 4096-byte
            page a write put bytes in and grew the file across. A truncation
            also dirties the rest of the 4096-byte page it cuts, which ext4
            zeroes. A block a write put bytes in, in a 4096-byte page the disk
            did not hold, past the durable length or in a hole, may also hold
            stale data, as ext4 may commit the page before its bytes land: the
            page at the same place that a durable truncation of the file freed,
            or that a file the crash removed held, and else every byte ['7'].
            With 512-byte blocks, a disk's sectors, that is ext4 with
            [data=writeback]. *)

  (** The type for when a crash fires while the workload runs. *)
  type trigger =
    | Manual  (** Only an explicit {!crash} (or {!post_crash}) fires it. *)
    | After_write of { path : string; file_offset : int }
        (** Crash right after the write covering that offset in that file
            commits, before any following sync. *)
    | Nth_op of int
        (** Crash on the [n]th durability op (1-based): a write or a truncation,
            a file's sync, or a directory operation, a change of mode or owner
            included. A directory barrier and {!sync_metadata} are not counted.
        *)
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

  val v : ?policy:policy -> ?trigger:trigger -> ?block_size:int -> unit -> t
  (** [v ~policy ~trigger ~block_size ()] is a fresh controller that rebuilds
      the post-crash image by [policy] (defaults to {!Drop_unsynced}, a single
      state) and crashes on [trigger] (defaults to {!Manual}). [block_size]
      (defaults to [4096]) is the byte size of the blocks {!Reorder_unsynced}
      writes back.

      @raise Stdlib.Invalid_argument if [block_size] is not positive. *)

  val wrap : t -> _ Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  (** [wrap t base] is [base] with durability tracked by [t]; files and
      subdirectories opened through it stay wrapped and share [t]. The tree
      under [base] when [wrap] is called is [t]'s durable starting image.

      As for {!Chaos.wrap}, the wrapper and what it opens hold [base] out of
      reach, so every write that reaches [base] through them enters the journal.
      A file's writes stay un-synced until {!Eio.File.sync} or
      {!Eio_fs_sync.sync_file} reaches it through the wrapper. A crash rebuilds
      the tree from the model, so a change made outside the wrapper, by name or
      on the host descriptor [eio-fs.unix] locks, is gone from the post-crash
      image.

      The model reads and rebuilds the tree with root's rights, whatever user
      [base] runs as: on an in-memory directory of {!Eio_fs.Fs} it does so
      through the same directory as {!Eio_fs.root}. A host directory gives the
      model only the rights of the process, which must be able to read and write
      every entry of the tree, as its owner or root can.

      A controller has one wrapper, and its operations run as the user of
      [base]. {!Eio_fs.Fs.as_user} refuses a wrapper, so a test cannot drive one
      controller as several users: it runs each user on a controller of its own.

      @raise Stdlib.Invalid_argument
        if [t] is already wrapped, or if the tree under [base] holds an entry
        that is neither a regular file, a directory nor a symbolic link. *)

  val crash : t -> unit
  (** [crash t] triggers a crash now: it reconstructs the post-crash image per
      {!type-policy} and rewrites the base to it. A second call does nothing.
      After a crash, every operation through the wrapper (a read, a write, a
      sync, an open, a stat or a directory operation) fails with an {!Eio.Io}
      exception ("power lost") and leaves the image as it is; re-read through
      {!post_crash}.

      @raise Stdlib.Invalid_argument
        if [t] was never {!wrap}ped; [t] is left uncrashed. *)

  val is_crashed : t -> bool
  (** [is_crashed t] is [true] iff a crash has fired on [t]. *)

  val post_crash : t -> base_dir
  (** [post_crash t] crashes [t] if it has not already crashed, and is the base
      directory given to {!wrap}, which now holds the crashed image. It is that
      handle itself, not a new one, and no wrapper is in front of it, so a test
      re-reads the image through it.

      @raise Stdlib.Invalid_argument if [t] was never {!wrap}ped. *)

  val sync_metadata : t -> unit
  (** [sync_metadata t] commits the whole metadata journal: every pending
      directory operation becomes durable, as fsync(2) on every directory
      concerned would make it, a setuid or setgid clear included. A directory
      barrier ({!Eio_fs_sync.sync_dir} on a wrapped directory) commits only the
      entries of that directory.

      @raise Eio.Io ("power lost") if [t] has crashed. *)

  val left : t -> string list
  (** [left t] is, file by file, what [t]'s crash left of the unsynced writes
      under a {!Choose} policy: the length, and each run of sectors that holds
      something else than the durable bytes, with what it holds. *)

  val seq : t -> int
  (** [seq t] is the place in program order of the last change [t] tracked. *)

  val stamp : t -> int
  (** [stamp t] grows with each change [t] tracks, each commit, barrier or
      failure of a sync, and each file or mode an open meets: a sync or an open
      that leaves it as it was changed nothing a cut leaves. *)

  val unsynced : t -> unsynced list
  (** [unsynced t] are the changes not yet made durable, in program order: every
      write no sync made durable, and every journal entry not committed. That
      includes an entry a sync or a barrier reached that does not hold yet: a
      [chmod] or [chown] whose node is not durable, as one a file sync reached
      while the file's creation is pending or one a barrier on a new directory
      reached for that directory, and an entry whose directory is not durable.
      Such an entry survives a crash whenever it would hold, whatever a
      {!Survive} predicate says of it. *)

  val pp_unsynced : unsynced Fmt.t
  (** [pp_unsynced] formats an un-synced change for logs and test failures. *)
end

(**/**)

(* For this package's tests. It hands out nothing a schedule's holder could
   not reach by running an operation through a wrapper. *)
module Internal : sig
  val next : Schedule.t -> Op.t -> Fault.t option
  (** [next t op] advances [t] by one operation and is the fault to inject. *)

  val crash_image :
    Crash.t ->
    (string * int64 * [ `File of string | `Dir | `Link of string ]) list
  (** [crash_image t] is the image [t]'s crash rebuilt: each entry's path below
      the wrapped root, the inode it had before the crash, and what it holds. *)

  val replay_copied : unit -> int
  (** [replay_copied ()] is how many bytes {!Crash} has copied into the images
      it replays a file's changes over, since the program started. *)
end

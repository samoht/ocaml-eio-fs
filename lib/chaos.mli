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
    | `Stat_error ]
  (** The family a fault belongs to, used to enable/disable kinds in a seeded
      schedule. *)

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
    subdirectories stay wrapped and share [schedule]. *)

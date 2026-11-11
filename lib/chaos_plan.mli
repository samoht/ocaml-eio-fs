(** The scenarios of a check, as pure functions of a recorded run. *)

(** The operations a wrapper sees. *)
module Op : sig
  type kind =
    [ `Open_read
    | `Open_write
    | `Stat
    | `Read
    | `Read_dir
    | `Write
    | `Sync
    | `Change ]

  type t = {
    index : int;
    kind : kind;
    path : string;
    nth : int;
    file_offset : int;
    length : int;
  }

  val kind_name : kind -> string
  (** [kind_name k] names [k] in reports and keys. *)

  val pp : t Fmt.t
  (** [pp] formats an operation. *)
end

(** The faults a wrapper injects. *)
module Fault : sig
  type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]
  type t = Fail of code | Short of int | Torn of int | Swap_symlink of string

  val pp : t Fmt.t
  (** [pp] formats a fault. *)

  val key : t -> string
  (** [key f] names [f] in a replay. *)
end

type pos = { kind : Op.kind; path : string; nth : int }
(** The type for the place of an operation in a run, by its kind, path and rank
    among those, so it is found again in another run. *)

val at : pos -> Op.t -> bool
(** [at p op] is [true] iff [op] is at [p]. *)

val pos_text : pos -> string
(** [pos_text p] says where [p] is, for a report. *)

type trace = Op.t list
(** The type for a run's operations, in order. *)

val structural : trace -> trace
(** [structural t] is the operations of [t] that make its structure: on each
    file, the first three reads and writes, the last, and the first of each
    length class, a length rounded down to a power of two; the syncs that follow
    them; and every other operation. *)

val spread : int -> 'a list -> 'a list
(** [spread n l] is [n] elements of [l], evenly spread, or [l] if it has [n] or
    fewer. *)

val sampled_share : budget:int -> linear:int -> classes:int -> int
(** [sampled_share ~budget ~linear ~classes] is the runs of the sampled tier for
    each of its [classes]: in all at most half of [linear], the runs the linear
    tier took, and at least 500, within [budget]. *)

(** {1 check} *)

(** The type for the conditions that last from an operation on. *)
type lasting = Read_only | Full | Denied

type check = { faults : (pos * Fault.t) list; lasting : (lasting * pos) option }
(** The type for a scenario of [check]: faults that fire once each, in order, or
    a lasting condition. *)

val none : check
(** [none] is the scenario with no fault. *)

val lasting_text : lasting -> string
(** [lasting_text l] says what the filesystem is under [l]. *)

val lasting_code : lasting -> Op.t -> Fault.code option
(** [lasting_code l op] is the error [op] meets under [l], if any. *)

val check_linear : trace -> check list
(** [check_linear first] is each fault alone at each structural operation of
    [first], and each lasting condition from each. *)

val check_others : trace -> check list
(** [check_others first] is the same at the other operations. *)

val check_seconds : pos * Fault.t -> after:trace -> check list
(** [check_seconds f ~after] is the pairs of faults whose first is [f] and whose
    second lands after it in [after], the run [f] alone made. *)

val faults_text : (pos * Fault.t) list -> string
(** [faults_text fs] says what [fs] do, for a report. *)

val check_key : check -> string
(** [check_key s] names [s] in a replay. *)

val check_text : check -> string
(** [check_text s] says what [s] does, for a report. *)

(** {1 Crash.check} *)

(** The type for the state a cut leaves: every unsynced change lost, every one
    kept, one of the states the cut allows by the draws that build it, or one
    drawn at random from a seed. *)
type state = Drop | Keep | Enum of int list | Seed of int

type crash = {
  cfaults : (pos * Fault.t) list;
  cut : pos option;  (** Before that operation; [None] at the end. *)
  state : state;
  rcut : (pos option * state) option;  (** A cut during recover. *)
}
(** The type for a scenario of [Crash.check]. *)

val base : crash
(** [base] is the cut at the end that loses every unsynced change. *)

val tears : sizes:(string * int) list -> trace -> Op.t -> int list
(** [tears ~sizes first op] is where the write [op] of [first] may be torn,
    given the size of each file before [first]: at the 512-byte sectors of the
    bytes it overwrites, and anywhere in those it adds. *)

val crash_cuts : trace -> crash list
(** [crash_cuts first] is a cut after each structural operation of [first] that
    changes the disk, and at its end, in both simplest states. *)

val crash_faults : tears:(Op.t -> int list) -> trace -> crash list
(** [crash_faults ~tears first] is each structural sync failed and each
    structural write torn, alone, with a cut at the end. *)

val crash_others : tears:(Op.t -> int list) -> trace -> crash list
(** [crash_others ~tears first] is the same at the other operations. *)

val rcuts : crash -> recover:trace -> crash list
(** [rcuts s ~recover] is [s] with a cut after each structural change of
    [recover], the trace of its recovery, and at its end. *)

val crash_seconds :
  tears:(Op.t -> int list) -> pos * Fault.t -> after:trace -> crash list
(** [crash_seconds ~tears f ~after] is the pairs of faults whose first is [f]
    and whose second lands after it in [after]. *)

val next_choices : (int * int) list -> int list option
(** [next_choices trail] is the draws of the next state a cut allows, after the
    one whose draws, last first, are [trail], each with the number of choices it
    had; [None] after the last. *)

(** The draws the states of a cut make. *)
module Draws : sig
  type t
  (** The type for the tree of the draws met so far. *)

  val empty : unit -> t
  (** [empty ()] is a tree with no draw. *)

  val add : t -> (int * int) list -> unit
  (** [add t trail] adds the draws, last first, of one state. *)

  val lower_bound : t -> int
  (** [lower_bound t] is the number of states the draws met so far allow at
      least: a choice not yet taken counts as one state. *)
end

val state_key : state -> string
(** [state_key st] names [st] in a replay. *)

val crash_key : crash -> string
(** [crash_key s] names [s] in a replay. *)

val split_state_key : string -> (string * state) option
(** [split_state_key k] is the key of the scenario [k] names with its state
    replaced by [Drop], and that state. *)

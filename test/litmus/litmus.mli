(** Crash-consistency litmus tests over {!Eio_fs.Chaos.Crash}.

    A litmus test is a setup, which runs on the bare in-memory tree and is
    durable before the body starts, and a body, which runs under the crash model
    and may lose power after any of its operations. Its outcome is what the
    observed names hold after the crash. The format follows Ferrite (Bornholt et
    al., "Specifying and Checking File System Crash-Consistency Models", ASPLOS
    2016; https://github.com/uwplse/ferrite). *)

(** {1 Programs} *)

(** The type for litmus operations. A file descriptor is named by the path it
    was created at, and keeps naming that file after a rename. *)
type op =
  | Creat of string  (** [creat(2)]: create the file, open it for writing. *)
  | Write of string * string  (** Append to the descriptor. *)
  | Pwrite of string * int * string  (** Overwrite at an offset. *)
  | Rename of string * string
  | Fsync of string  (** A barrier on the descriptor. *)
  | Mark
      (** An externally visible event: the outcome records whether the crash
          came after it. *)
  | Mkdir of string
  | Unlink of string
  | Symlink of { target : string; name : string }
  | Truncate of string * int  (** [ftruncate(2)] on the descriptor. *)
  | Fsync_dir of string  (** A barrier on a directory, [""] for the root. *)
  | Fsync_fails of string
      (** A barrier on the descriptor that fails with [EIO], injected by
          {!Eio_fs.Chaos.wrap} above the crash model. *)

type t = {
  name : string;
  setup : op list;
  body : op list;
  observe : string list;  (** The names an outcome records. *)
}
(** The type for litmus tests. *)

(** {1 Outcomes} *)

type entry = Absent | File of string | Dir | Link of string

type outcome = { marked : bool; entries : (string * entry) list }
(** The type for post-crash states: [marked] iff the crash came after the body's
    {!Mark}. *)

module Outcome : sig
  type t = outcome

  val compare : t -> t -> int
  val pp : t Fmt.t

  val file : ?marked:bool -> (string * string option) list -> t
  (** [file l] is the outcome holding each regular file of [l], [None] for
      absent. *)
end

module Set : Set.S with type elt = outcome

val pp_set : Set.t Fmt.t
val testable : Set.t Alcotest.testable

(** {1 Running under eio-fs} *)

type exploration = {
  outcomes : Set.t;
  runs : int;
  last_new : int;
      (** The highest seed (or subset index) that found an outcome no earlier
          one had: the saturation point. *)
}

val reorder : ?block_size:int -> seeds:int -> t -> exploration
(** [reorder ~seeds t] runs [t] under [Reorder_unsynced s] for [s] in
    [0 .. seeds - 1] and under [Drop_unsynced], crashing after each body
    operation in turn and at the end, and collects the distinct outcomes. *)

val survive : ?block_size:int -> t -> exploration
(** [survive t] runs [t] under a [Survive] policy for every subset of the
    changes pending at each crash point. *)

(** {1 Verdicts} *)

val seeds : unit -> int
(** [seeds ()] is [$LITMUS_SEEDS], or [500]. *)

val report : bool
(** [report] is [true] iff [$LITMUS_REPORT] is set: verdicts print their outcome
    sets to stderr. *)

val check :
  name:string ->
  eio:Set.t ->
  envelope:Set.t ->
  ext4:Set.t ->
  extra:outcome list ->
  missing:outcome list ->
  impossible:outcome list ->
  unit
(** [check] asserts that the eio-fs outcomes outside [envelope] are exactly
    [impossible] (known unsoundness), that those outside [ext4] but inside
    [envelope] are exactly [extra] (deliberately weaker), and that the [ext4]
    outcomes eio-fs cannot produce are exactly [missing]. *)

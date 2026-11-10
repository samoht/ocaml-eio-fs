(** An independent reference crash model.

    The reference sees only the live trees before and after each operation and
    turns each operation into events on inodes. Events touching the same
    directory stay ordered, as do the attribute and data events of one inode; a
    directory barrier waits for the earlier changes to that directory and a file
    barrier for its file's data and attributes. A crash state is any
    downward-closed set of events that holds every completed barrier. *)

(** {1 Operations} *)

type op =
  | Mkdir of string
  | Creat of string * char
  | Append of string * char
  | Rename of string * string
  | Unlink of string
  | Rmdir of string
  | Fsync of string
  | Fsync_dir of string  (** [""] is the root. *)
  | Chmod of string * int
  | Trunc of string * int

val pp_op : op Fmt.t
val pp_ops : op list Fmt.t

val exec : Eio.Fs.dir_ty Eio.Path.t -> op -> unit
(** [exec fs op] runs [op] below [fs]. *)

(** {1 Live trees} *)

module IM : Map.S with type key = int
module SM : Map.S with type key = string

type node = { kind : [ `F | `D ]; perm : int; data : string; ents : int SM.t }

type tree = int * node IM.t
(** The type for trees: the root's id and each node by id. *)

type ids
(** The type for small ids given to inodes in the order they are met. *)

val ids : unit -> ids
val id : ids -> int64 -> int
val id_opt : ids -> int64 -> int option

val snapshot : ids -> Eio.Fs.dir_ty Eio.Path.t -> tree
(** [snapshot ids root] is the tree below [root]. *)

(** {1 Events} *)

type event =
  | Meta of {
      changes : (int * string * int option) list;
      touches : int list;
      links : int list;
      moved : int list;
      under : int list;
    }
  | Init of int
  | Attrs of int * int
  | Data of int * string
  | Bdir of int
  | Bfile of int

val pp_events : event list Fmt.t

val events : before:tree -> after:tree -> barrier:event list -> event list
(** [events ~before ~after ~barrier] are the events of the operation that took
    [before] to [after], then [barrier]. *)

val bases : tree list -> node IM.t
(** [bases trees] is each inode's attributes as first met in [trees]. *)

(** {1 Crash states} *)

type entry = {
  path : string;
  k : [ `F | `D ];
  id : int;
  perm : int;
  content : string;
}

type state = entry list

val pp_state : state Fmt.t

module States : Set.S with type elt = state

val without_data : state -> state
val without_modes : state -> state

type states = {
  all : States.t;  (** Every state the reference allows. *)
  prefix : States.t;
      (** Those whose journal events are the barriers' closure and a
          program-order prefix of the rest. *)
  ancestors : States.t;
      (** Those of [prefix] that also hold, with a rename, the renames that
          moved a directory above where it lands. *)
}

val states :
  root:int -> init:node IM.t -> base:node IM.t -> event list -> states
(** [states ~root ~init ~base evs] are the crash states after [evs] from the
    tree [init]. *)

(** Ferrite's operational crash-consistency models, as outcome enumerators.

    Ported from the Rosette models of Ferrite
    (https://github.com/uwplse/ferrite, [rosette/seqfs.rkt],
    [rosette/advfs.rkt], [rosette/ext4.rkt], [rosette/lang.rkt]; MIT, Copyright
    (c) 2015 James Bornholt, Antoine Kaufmann, Jialin Li, Arvind Krishnamurthy,
    Emina Torlak and Xi Wang). Where Rosette leaves a choice to the SMT solver,
    this port enumerates it: every prefix of every valid reordering of the
    cracked program is a crash state. *)

type ext4 = {
  block_size : int;
  nodelalloc : bool;  (** ext4 mounted [nodelalloc]. *)
  writeback : bool;
      (** Not Ferrite's: [data=writeback]. An append to a new block is a block
          write and a size update that reorder freely, so a size can land before
          its data (Definition 7 without rule 4). *)
  local_fsync : bool;
      (** Not Ferrite's: the barrier rule of the paper's Definition 3, under
          which an [fsync(f)] waits for earlier updates of [f] only, instead of
          the Rosette model's barrier on every operation (which its own source
          marks "too strong"). A file's creation is a directory update, so
          [fsync(f)] does not wait for it. *)
}

type model =
  | Seqfs  (** Sequential crash consistency: no reordering. *)
  | Advfs
      (** Data reaches the disk only on [fsync]; operations on different files
          reorder. *)
  | Ext4 of ext4

val ext4 : ?block_size:int -> ?nodelalloc:bool -> unit -> model
(** [ext4 ()] is Ferrite's ext4 ([data=ordered]) model. [block_size] defaults to
    [4096]. *)

val envelope : ?block_size:int -> unit -> model
(** [envelope ()] is the weakest model eio-fs claims to stay inside: ext4 with
    [writeback] and [local_fsync]. *)

val pp_model : model Fmt.t

val outcomes : model -> Litmus.t -> Litmus.Set.t
(** [outcomes m t] is every post-crash outcome [m] allows for [t].

    @raise Invalid_argument
      if [t] uses an operation the Ferrite language lacks, other than
      {!Litmus.Pwrite} over existing bytes and {!Litmus.Mark}. *)

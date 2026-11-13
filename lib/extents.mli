(** Disjoint byte ranges of a file, in order: those a write covered. *)

type t
(** The type for sets of half-open ranges [\[a, b)], none empty, no two
    overlapping or touching. *)

val empty : t
(** [empty] holds no range. *)

val add : int -> int -> t -> t
(** [add a b t] is [t] with [\[a, b)] merged with every range it overlaps or
    touches, or [t] if [b <= a]. It costs [O((1 + k) log n)] for [k] ranges
    merged. *)

val cut : int -> t -> t
(** [cut n t] is the ranges of [t] below [n], the one across [n] ending there.
    It costs [O(log n + k)] for [k] ranges dropped. *)

val overlaps : int -> int -> t -> bool
(** [overlaps a b t] is [true] iff a range [\[x, y)] of [t] has [x < b] and
    [y > a]. *)

val pages : t -> int
(** [pages t] is how many 4096-byte pages the ranges of [t] put bytes in. *)

val fold : ?from:int -> (int -> int -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold ~from f t acc] folds [f a b] over the ranges [\[a, b)] of [t] with
    [b > from], in order. [from] defaults to [min_int]. It costs [O(log n + k)]
    for [k] ranges folded. *)

(** The faults a wrapper injects. {!Eio_fs_chaos.Fault} documents them. *)

type code = [ `Enoent | `Eacces | `Eperm | `Erofs | `Enospc | `Eio ]

type t =
  | Fail of code
  | Evict of code
  | Short of int
  | Torn of int
  | Swap_symlink of string
  | Cancel of int

val code_name : code -> string
(** [code_name c] is [c]'s errno, as ["EIO"]. *)

val pp : t Fmt.t
(** [pp] formats a fault. *)

val key : t -> string
(** [key f] names [f] in a replay key: ["eio"], ["torn3"]. *)

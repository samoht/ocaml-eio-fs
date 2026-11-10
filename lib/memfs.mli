(** The in-memory filesystem's directories, behind [Eio_fs.Fs]. *)

val fs :
  ?user:Low_level.User.t ->
  Low_level.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t
(** [fs ~user t] is full access to the filesystem [t], as [user] (defaults to
    root). *)

val cwd :
  ?user:Low_level.User.t ->
  Low_level.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Resource.t
(** [cwd ~user t] is access to [t] confined to its current directory. *)

val v : ?user:Low_level.User.t -> unit -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [v ~user ()] is the root of a fresh, empty filesystem. *)

val as_user :
  Low_level.User.t -> 'a Eio.Path.t -> [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [as_user user path] is [path], an in-memory directory, as [user].

    @raise Invalid_argument if [path] is not an in-memory directory. *)

val load :
  ?filter:('a Eio.Path.t -> bool) ->
  'a Eio.Path.t ->
  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
(** [load ~filter path] is a new filesystem holding a copy of the tree at
    [path]. *)

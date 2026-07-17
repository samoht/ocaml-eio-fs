external full_fsync : Unix.file_descr -> unit = "caml_eio_fs_full_fsync"

(* A plain POSIX directory registers no barrier of its own, so open it
   natively -- Eio directory resources intentionally do not expose their
   descriptor -- and issue the platform sync. *)
let posix_sync_dir p =
  let fd = Unix.openfile (Eio.Path.native_exn p) [ Unix.O_RDONLY ] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> full_fsync fd)

let sync_dir ((res, _) as p : _ Eio.Path.t) =
  match Eio_fs_sync.find res with
  | Some _ -> Eio_fs_sync.sync_dir p
  | None -> posix_sync_dir p

let save_durable ~create path content =
  Eio_fs_sync.save_durable_with ~sync_dir ~create path content

(* {2 The barrier-granting wrapper}

   A delegating directory resource that adds exactly one operation: the
   registered [Eio_fs_sync.Dir_sync] barrier, implemented natively. What a
   POSIX main wraps its directory with before handing it to a pure-Eio store,
   granting the durability capability the plain backend cannot express. *)

type boxed = Boxed : (module Eio.Fs.Pi.DIR with type t = 'a) * 'a -> boxed
type dir = { base : Eio.Fs.dir_ty Eio.Std.r; boxed : boxed }

(* Unwrap for rename targets, so a wrapped destination reaches the base
   backend. *)
type (_, _, _) Eio.Resource.pi +=
  | Barrier_base :
      ('t, 't -> Eio.Fs.dir_ty Eio.Std.r, [> `Barrier_base ]) Eio.Resource.pi

let wrap_hook :
    (base:Eio.Fs.dir_ty Eio.Std.r -> [ `Close | Eio.Fs.dir_ty ] Eio.Std.r) ref =
  ref (fun ~base:_ -> assert false)

module Dir_impl = struct
  type t = dir

  let open_in t ~sw path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.open_in bv ~sw path

  let open_out t ~sw ~append ~create path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.open_out bv ~sw ~append ~create path

  let open_subtree t ~sw path =
    let (Boxed ((module B), bv)) = t.boxed in
    let sub = B.open_subtree bv ~sw path in
    !wrap_hook ~base:(sub :> Eio.Fs.dir_ty Eio.Std.r)

  let mkdir t ~perm path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.mkdir bv ~perm path

  let read_dir t path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.read_dir bv path

  let with_dir_entries t path fn =
    let (Boxed ((module B), bv)) = t.boxed in
    B.with_dir_entries bv path fn

  let stat t ~follow path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.stat bv ~follow path

  let unlink t path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.unlink bv path

  let rmdir t path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.rmdir bv path

  let rename t old_path new_dir new_path =
    let (Eio.Resource.T (nv, nops)) = new_dir in
    let target =
      match Eio.Resource.get_opt nops Barrier_base with
      | Some unwrap -> unwrap nv
      | None -> (new_dir :> Eio.Fs.dir_ty Eio.Std.r)
    in
    let (Boxed ((module B), bv)) = t.boxed in
    B.rename bv old_path target new_path

  let read_link t path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.read_link bv path

  let symlink ~link_to t path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.symlink ~link_to bv path

  let chmod t ~follow ~perm path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.chmod bv ~follow ~perm path

  let chown ~follow ?uid ?gid t path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.chown ~follow ?uid ?gid bv path

  let native t path =
    let (Boxed ((module B), bv)) = t.boxed in
    B.native bv path

  let pp ppf t =
    let (Boxed ((module B), bv)) = t.boxed in
    Fmt.pf ppf "barrier(%a)" B.pp bv
end

let handler :
    (dir, [ `Dir | `Close | `Barrier_base | `Dir_sync ]) Eio.Resource.handler =
  Eio.Resource.handler
    [
      H (Eio.Fs.Pi.Dir, (module Dir_impl));
      H
        ( Eio.Resource.Close,
          fun t ->
            let (Eio.Resource.T (v, ops)) = t.base in
            match Eio.Resource.get_opt ops Eio.Resource.Close with
            | Some close -> close v
            | None -> () );
      H (Barrier_base, fun t -> t.base);
      H
        ( Eio_fs_sync.Dir_sync,
          fun t ~path ->
            match Eio_fs_sync.find t.base with
            | Some barrier -> barrier ~path
            | None -> posix_sync_dir (t.base, path) );
    ]

let wrap_dir ~base : [ `Close | Eio.Fs.dir_ty ] Eio.Std.r =
  let (Eio.Resource.T (bv, bops)) = base in
  let boxed = Boxed (Eio.Resource.get bops Eio.Fs.Pi.Dir, bv) in
  (Eio.Resource.T ({ base; boxed }, handler)
    :> [ `Close | Eio.Fs.dir_ty ] Eio.Std.r)

let () = wrap_hook := fun ~base -> wrap_dir ~base
let wrap (dir, path) = (wrap_dir ~base:(dir :> Eio.Fs.dir_ty Eio.Std.r), path)

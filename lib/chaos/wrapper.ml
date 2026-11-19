open! Hermetic
open Eio.Std

(* Faults surface as ordinary [Eio.Io] exceptions so consumers cannot tell a
   real backend failure from an injected one. *)
type Eio.Exn.Backend.t += Injected_fault of string

let () =
  Eio.Exn.Backend.register_pp (fun ppf -> function
    | Injected_fault s ->
        Fmt.pf ppf "chaos:%s" s;
        true
    | _ -> false)

let backend code ctx =
  Fmt.kstr (fun s -> Injected_fault s) "%s %s" (Fault.code_name code) ctx

let raise_open code path =
  match code with
  | `Enoent -> raise (Eio.Fs.err (Eio.Fs.Not_found (backend `Enoent path)))
  | (`Eacces | `Eperm) as c ->
      raise (Eio.Fs.err (Eio.Fs.Permission_denied (backend c path)))
  | (`Erofs | `Enospc | `Eio) as c ->
      raise (Eio.Exn.create (Eio.Exn.X (backend c path)))

let raise_io code ctx = raise (Eio.Exn.create (Eio.Exn.X (backend code ctx)))

type hook = space:bool -> Op.t -> Fault.t option

(* The hook and what numbers the operations it is asked about: every
   wrapper given one schedule shares the numbering. *)
type sched = {
  hook : hook;
  mutable index : int;
  nths : (Op.kind * string option, int) Hashtbl.t; (* on a path, and on any *)
}

let schedule hook = { hook; index = 0; nths = Hashtbl.create 16 }

(* An [ENOSPC] on an operation that needs no room is none: Linux never
   answers it there. *)
let applies ~space (op : Op.t) : Fault.t -> bool = function
  | Fail `Enospc when (not space) && (op.kind = `Change || op.kind = `Open_write)
    ->
      false
  | Fail _ -> true
  | Evict _ -> op.kind = `Sync
  | Short _ -> op.kind = `Read || op.kind = `Write
  | Torn _ -> op.kind = `Write
  | Swap_symlink _ -> (
      match op.kind with `Open_read | `Open_write | `Stat -> true | _ -> false)

(* The fault [t]'s hook answers for the next operation, if it applies. *)
let next ?(space = false) ?(offset = 0) ?(len = 0) ?dest t kind path =
  t.index <- t.index + 1;
  let count key =
    let n = 1 + Option.value ~default:0 (Hashtbl.find_opt t.nths key) in
    Hashtbl.replace t.nths key n;
    n
  in
  let nth = count (kind, Some path) in
  let rank = count (kind, None) in
  let op : Op.t =
    { index = t.index; kind; path; nth; rank; offset; length = len; dest }
  in
  match t.hook ~space op with
  | Some f when applies ~space op f -> Some f
  | Some _ | None -> None

(* Take at most [n] bytes from the front of [bufs], sharing the underlying
   storage so a read still lands in the caller's buffers. *)
let take_v n bufs =
  let rec go acc remaining = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | b :: bs ->
        let len = Cstruct.length b in
        if len <= remaining then go (b :: acc) (remaining - len) bs
        else List.rev (Cstruct.sub b 0 remaining :: acc)
  in
  if n <= 0 then [] else go [] n bufs

(* [n] within [[lo, hi]], and [hi] when the range is empty: a fault that
   shortens an operation never asks for more bytes than it offers, so a short
   read of an empty buffer reads it whole. *)
let clamp lo hi n = min hi (max lo n)

let close_resource r =
  let (Eio.Resource.T (v, ops)) = r in
  match Eio.Resource.get_opt ops Eio.Resource.Close with
  | Some close -> close v
  | None -> ()

(* {2 What a wrapped file carries over}

   A wrapped file is its own value: the wrapper's state, bound to the
   wrapper's read, write, sync and close and nothing of the base's. A handler
   rebuilt from its bindings so reaches no operation the wrapper does not
   implement, and so no operation that steps around the schedule or the
   journal. It registers [Eio_fs_sync.File_sync], bound to its own sync, so a
   barrier a publication issues reaches the wrapper.

   The base file stays reachable through [Eio_fs_internal.Base_file] alone,
   which no library outside this package can name: eio-fs.unix takes the
   host descriptor a record lock is taken on from it. *)
let rw_boxed (type s) ?(extra = [])
    (module X : Eio.File.Pi.WRITE with type t = s) ~barrier (state : s)
    ~(base : Eio.File.rw_ty r) : Eio.File.rw_ty r =
  Eio.Resource.T
    ( state,
      Eio.Resource.handler
        (Eio.Resource.bindings (Eio.File.Pi.rw (module X))
        @ [
            Eio.Resource.H (Eio_fs_sync.File_sync, barrier);
            Eio.Resource.H
              (Eio_fs_internal.Base_file, fun _ -> (base :> Eio.File.ro_ty r));
          ]
        @ extra) )

type (_, _, _) Eio.Resource.pi += Sync_failed = Eio_fs_internal.Sync_failed

let sync_failed ~evict (Eio.Resource.T (v, ops)) =
  match Eio.Resource.get_opt ops Sync_failed with
  | Some f -> f v ~evict
  | None -> ()

(* {2 File wrappers} *)

(* Reads are shared between the read-only and read-write wrappers; a read-write
   file coerces to read-only for these. [pos] is the shared streaming cursor. *)
let do_single_read sched path ~pos (base : Eio.File.ro_ty r) buf =
  let len = Cstruct.length buf in
  let buf =
    match next sched ~offset:!pos ~len `Read path with
    | Some (Fail c) -> raise_io c path
    | Some (Short n) -> Cstruct.sub buf 0 (clamp 1 len n)
    | _ -> buf
  in
  let got = Eio.Flow.single_read base buf in
  pos := !pos + got;
  got

let do_pread sched path (base : Eio.File.ro_ty r) ~file_offset bufs =
  let len = Cstruct.lenv bufs in
  let offset = Optint.Int63.to_int file_offset in
  let bufs =
    match next sched ~offset ~len `Read path with
    | Some (Fail c) -> raise_io c path
    | Some (Short n) -> take_v (clamp 1 len n) bufs
    | _ -> bufs
  in
  Eio.File.pread base ~file_offset bufs

(* Where a streaming write lands: at the cursor, or for a file opened to
   append at its end, wherever the cursor was. *)
let write_offset ~append ~pos base =
  if append then Optint.Int63.to_int (Eio.File.stat base).Eio.File.Stat.size
  else !pos

(* A seek moves the cursor the wrapper reports offsets from. *)
let do_seek ~pos base off cmd =
  let at = Eio.File.seek base off cmd in
  pos := Optint.Int63.to_int at;
  at

let do_single_write sched path ~append ~pos (base : Eio.File.rw_ty r) bufs =
  let len = Cstruct.lenv bufs in
  let off = write_offset ~append ~pos base in
  match next sched ~space:true ~offset:off ~len `Write path with
  | Some (Short n) ->
      let got = Eio.Flow.single_write base (take_v (clamp 1 len n) bufs) in
      pos := off + got;
      got
  | Some (Torn n) ->
      let n = clamp 0 len n in
      if n > 0 then Eio.Flow.write base (take_v n bufs);
      pos := off + n;
      Fmt.kstr (raise_io `Enospc) "%s torn@%d" path !pos
  | Some (Fail c) -> raise_io c path
  | _ ->
      let got = Eio.Flow.single_write base bufs in
      pos := off + got;
      got

let do_pwrite sched path ~append (base : Eio.File.rw_ty r) ~file_offset bufs =
  let len = Cstruct.lenv bufs in
  let off =
    if append then write_offset ~append ~pos:(ref 0) base
    else Optint.Int63.to_int file_offset
  in
  match next sched ~space:true ~offset:off ~len `Write path with
  | Some (Short n) ->
      Eio.File.pwrite_single base ~file_offset (take_v (clamp 1 len n) bufs)
  | Some (Torn n) ->
      let n = clamp 0 len n in
      if n > 0 then Eio.File.pwrite_all base ~file_offset (take_v n bufs);
      Fmt.kstr (raise_io `Enospc) "%s torn@%d" path (off + n)
  | Some (Fail c) -> raise_io c path
  | _ -> Eio.File.pwrite_single base ~file_offset bufs

(* A barrier passed on to the file beneath, through the cache of the drive
   when [full]: through the base's own barrier, or its sync. *)
let barrier_base ~full (base : Eio.File.rw_ty r) =
  match Eio_fs_sync.file_barrier base with
  | Some barrier -> barrier ~full
  | None -> Eio.File.sync base

let do_sync ?(full = false) sched path (base : Eio.File.rw_ty r) =
  match next sched `Sync path with
  | Some (Fail c) ->
      sync_failed ~evict:false base;
      raise_io c path
  | Some (Evict c) ->
      sync_failed ~evict:true base;
      raise_io c path
  | _ -> barrier_base ~full base

let do_stat sched path (base : Eio.File.ro_ty r) =
  match next sched `Stat path with
  | Some (Fail c) -> raise_io c path
  | _ -> Eio.File.stat base

(* One record backs both wrappers. [ro] is always present (reads work on either
   kind); [rw] is present exactly when the file was opened read-write, and is
   the same underlying resource as [ro]. *)
type file = {
  sched : sched;
  path : string;
  append : bool;
  pos : int ref;
  ro : Eio.File.ro_ty r;
  rw : Eio.File.rw_ty r option;
}

(* Read-write is invariant: the read-write handler is only ever attached to a
   file constructed with [rw = Some _]. *)
let writable t = match t.rw with Some rw -> rw | None -> assert false

module Ro_impl = struct
  type t = file

  let read_methods = []
  let single_read t buf = do_single_read t.sched t.path ~pos:t.pos t.ro buf
  let pread t ~file_offset bufs = do_pread t.sched t.path t.ro ~file_offset bufs
  let stat t = do_stat t.sched t.path t.ro
  let seek t off cmd = do_seek ~pos:t.pos t.ro off cmd
  let close t = close_resource t.ro
end

(* fsync(2) flushes the file whichever descriptor names it, so a file opened
   read-only answers [Eio_fs_sync.File_sync] too, scheduled as the read-write
   sync is: through the base's own barrier, or the sync its handle exposes. *)
let sync_base ~full (Eio.Resource.T (v, ops) as base) =
  match Eio_fs_sync.file_barrier base with
  | Some barrier -> barrier ~full
  | None -> (
      match Eio.Resource.get_opt ops Eio.File.Pi.Write with
      | Some (module W) -> W.sync v
      | None ->
          invalid_arg
            "Eio_fs_chaos.wrap: base opens read-only files that cannot sync, \
             register Eio_fs_sync.File_sync on them")

let sync_ro t ~full =
  match next t.sched `Sync t.path with
  | Some (Fail c) ->
      sync_failed ~evict:false t.ro;
      raise_io c t.path
  | Some (Evict c) ->
      sync_failed ~evict:true t.ro;
      raise_io c t.path
  | _ -> sync_base ~full t.ro

(* A failure a wrapper above injected is told on to the file beneath, so
   stacked wrappers reach the power-loss model at the bottom. *)
let ro_handler =
  Eio.Resource.handler
    (Eio.Resource.bindings (Eio.File.Pi.ro (module Ro_impl))
    @ [
        Eio.Resource.H (Eio_fs_sync.File_sync, sync_ro);
        Eio.Resource.H (Sync_failed, fun t ~evict -> sync_failed ~evict t.ro);
        Eio.Resource.H (Eio_fs_internal.Base_file, fun t -> t.ro);
      ])

let wrap_ro sched ~path base : Eio.File.ro_ty r =
  Eio.Resource.T
    ( { sched; path; append = false; pos = ref 0; ro = base; rw = None },
      ro_handler )

module Rw_impl = struct
  include Ro_impl

  let single_write t bufs =
    do_single_write t.sched t.path ~append:t.append ~pos:t.pos (writable t) bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src

  let pwrite t ~file_offset bufs =
    do_pwrite t.sched t.path ~append:t.append (writable t) ~file_offset bufs

  let sync t = do_sync t.sched t.path (writable t)

  let truncate t len =
    match next t.sched ~offset:(Optint.Int63.to_int len) `Truncate t.path with
    | Some (Fail c) -> raise_io c t.path
    | _ -> Eio.File.truncate (writable t) len
end

let wrap_rw sched ~path ~append base : Eio.File.rw_ty r =
  let state =
    {
      sched;
      path;
      append;
      pos = ref 0;
      ro = (base :> Eio.File.ro_ty r);
      rw = Some base;
    }
  in
  rw_boxed
    ~extra:
      [ Eio.Resource.H (Sync_failed, fun t ~evict -> sync_failed ~evict t.ro) ]
    (module Rw_impl)
    ~barrier:(fun t ~full -> do_sync ~full t.sched t.path (writable t))
    state ~base

(* {2 Directory wrapper} *)

(* The base directory provider and value, unpacked once and boxed so the wrapper
   methods can call through without re-resolving the interface. *)
type base_dir =
  | Base_dir : (module Eio.Fs.Pi.DIR with type t = 'a) * 'a -> base_dir

type dir = { base : Eio.Fs.dir_ty r; boxed : base_dir; sched : sched }

(* A rename target that is itself a chaos directory is unwrapped before it
   reaches the base backend. *)
let base_dir = Eio_fs_internal.base_dir

let wrap_dir_hook :
    (base:Eio.Fs.dir_ty r -> sched:sched -> [ `Close | Eio.Fs.dir_ty ] r) ref =
  ref (fun ~base:_ ~sched:_ -> assert false)

(* Free the name at [path], whatever holds it. A directory is emptied first and
   removed with [rmdir]; anything else is unlinked. Both wrappers that free a
   name go through here. *)
let rec remove_at (Base_dir ((module B), bv) as owner) path =
  match B.stat bv ~follow:false path with
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()
  | { kind = `Directory; _ } ->
      let rec clear () =
        match B.read_dir bv path with
        | [] -> ()
        | names ->
            List.iter
              (fun name ->
                let child = Eio_utils.Posix_path.join path name in
                remove_at owner child)
              names;
            clear ()
      in
      clear ();
      if not (String.equal path "") then B.rmdir bv path
  | _ -> B.unlink bv path

(* A swap fires after the scheduled operation: the name becomes a symbolic
   link to the target, so whatever resolves the path afterwards reaches the
   target, as in the window a TOCTOU attack uses. *)
let swap_after boxed fault path =
  match fault with
  | Some (Fault.Swap_symlink target) ->
      let (Base_dir ((module B), bv)) = boxed in
      remove_at boxed path;
      B.symlink ~link_to:target bv path
  | Some _ | None -> ()

module Dir_impl = struct
  type t = dir

  let open_in t ~sw ~follow path =
    let fault = next t.sched `Open_read path in
    (match fault with Some (Fail c) -> raise_open c path | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let file = B.open_in bv ~sw ~follow path in
    swap_after t.boxed fault path;
    wrap_ro t.sched ~path file

  let open_out t ~sw ~follow ~append ~create path =
    let (Base_dir ((module B), bv)) = t.boxed in
    (* Only an open that makes its file needs room. *)
    let space =
      create <> `Never
      &&
      match B.stat bv ~follow:true path with
      | _ -> false
      | exception Eio.Io _ -> true
    in
    let fault = next t.sched ~space `Open_write path in
    (match fault with Some (Fail c) -> raise_open c path | _ -> ());
    let file = B.open_out bv ~sw ~follow ~append ~create path in
    swap_after t.boxed fault path;
    wrap_rw t.sched ~path ~append file

  (* A file of the base the caller holds, at [path]: the hook answers for it
     as for an open of [path] that makes nothing, and it is wrapped as one
     opened through the wrapper is. *)
  let adopt t ~sw ~path file =
    let fault = next t.sched `Open_write path in
    (match fault with Some (Fail c) -> raise_open c path | _ -> ());
    let file = Eio_fs_sync.adopt ~sw (t.base, path) file in
    swap_after t.boxed fault path;
    wrap_rw t.sched ~path ~append:false file

  let open_subtree t ~sw path =
    let fault = next t.sched `Open_read path in
    (match fault with Some (Fail c) -> raise_open c path | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let sub = B.open_subtree bv ~sw path in
    swap_after t.boxed fault path;
    !wrap_dir_hook ~base:(sub :> Eio.Fs.dir_ty r) ~sched:t.sched

  let stat t ~follow path =
    let fault = next t.sched `Stat path in
    (match fault with Some (Fail c) -> raise_open c path | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    let result = B.stat bv ~follow path in
    swap_after t.boxed fault path;
    result

  (* A change to a directory's entries, named by the entry it makes or
     removes; only one that makes an entry needs room. *)
  let change ?(space = false) ?dest t path =
    match next t.sched ?dest ~space `Change path with
    | Some (Fail c) -> raise_open c path
    | _ -> ()

  let mkdir t ~perm path =
    change ~space:true t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.mkdir bv ~perm path

  let read_dir t path =
    (match next t.sched `Read_dir path with
    | Some (Fail c) -> raise_io c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    B.read_dir bv path

  let with_dir_entries t path fn =
    (match next t.sched `Read_dir path with
    | Some (Fail c) -> raise_io c path
    | _ -> ());
    let (Base_dir ((module B), bv)) = t.boxed in
    B.with_dir_entries bv path fn

  let unlink t path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.unlink bv path

  let rmdir t path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.rmdir bv path

  let rename t old_path new_dir new_path =
    change ~space:true ~dest:new_path t old_path;
    let target =
      match base_dir new_dir with
      | Some base -> base
      | None -> (new_dir :> Eio.Fs.dir_ty r)
    in
    let (Base_dir ((module B), bv)) = t.boxed in
    B.rename bv old_path target new_path

  let rename_exclusive t backend ~source ~target:(new_dir, new_path) =
    change ~space:true ~dest:new_path t source;
    let target =
      match base_dir new_dir with Some base -> base | None -> new_dir
    in
    Eio_fs_sync.Rename.exclusive backend (t.base, source) (target, new_path)

  let read_link t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.read_link bv path

  let symlink ~link_to t path =
    change ~space:true t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.symlink ~link_to bv path

  let chmod t ~follow ~perm path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.chmod bv ~follow ~perm path

  let chown ~follow ?uid ?gid t path =
    change t path;
    let (Base_dir ((module B), bv)) = t.boxed in
    B.chown ~follow ?uid ?gid bv path

  let native t path =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.native bv path

  let barrier ~full t path =
    (match next t.sched `Sync path with
    | Some (Fail c | Evict c) -> raise_io c path
    | _ -> ());
    match Eio_fs_sync.dir_barrier t.base with
    | Some barrier -> barrier ~path ~full
    | None ->
        let (Base_dir ((module B), bv)) = t.boxed in
        B.sync_dir bv path

  let sync_dir t path = barrier ~full:false t path

  let pp ppf t =
    let (Base_dir ((module B), bv)) = t.boxed in
    B.pp ppf bv
end

(* Path syntax is a property of the backend: a wrapper spells its paths as
   its base does, POSIX's or Windows'. *)
let dir_impl (module P : Eio.Fs.Pi.PATH) :
    (module Eio.Fs.Pi.DIR with type t = dir) =
  (module struct
    include Dir_impl

    let split = P.split
    let join = P.join
  end)

(* The wrapper is its own value, bound to its own interfaces: a handler
   rebuilt from its bindings reaches nothing of the base's, so no directory
   operation steps around the schedule. The base stays reachable through
   [Eio_fs_internal.Base_dir] and [Eio_fs_internal.Admin] alone, which no
   library outside this package can name. A backend's own interfaces, such as the native directory a child
   process is started in, are not carried over. *)
let dir_handler syntax =
  Eio.Resource.handler
    [
      Eio.Resource.H (Eio.Fs.Pi.Dir, dir_impl syntax);
      Eio.Resource.H (Eio.Resource.Close, fun t -> close_resource t.base);
      Eio.Resource.H (Eio_fs_internal.Base_dir, fun t -> t.base);
      (* The base's root rights, under the same schedule. *)
      Eio.Resource.H
        ( Eio_fs_internal.Admin,
          fun t ->
            let base =
              Option.value ~default:t.base (Eio_fs_internal.admin t.base)
            in
            (!wrap_dir_hook ~base ~sched:t.sched :> Eio.Fs.dir_ty r) );
      Eio.Resource.H
        ( Eio_fs_internal.Rebase,
          fun t f ->
            !wrap_dir_hook ~base:(f t.base :> Eio.Fs.dir_ty r) ~sched:t.sched );
      Eio.Resource.H (Eio_fs_sync.Rename.Exclusive, Dir_impl.rename_exclusive);
      Eio.Resource.H
        (Eio_fs_sync.Process_local, fun t -> Eio_fs_sync.is_process_local t.base);
      Eio.Resource.H
        (Eio_fs_sync.Modeless, fun t -> Eio_fs_sync.is_modeless t.base);
      Eio.Resource.H
        ( Eio_fs_sync.Dir_sync,
          fun t ~path ~full -> Dir_impl.barrier ~full t path );
      Eio.Resource.H (Eio_fs_sync.Adopt, Dir_impl.adopt);
    ]

let wrap_dir ~base ~sched : [ `Close | Eio.Fs.dir_ty ] r =
  let (Eio.Resource.T (bv, bops)) = base in
  let module B = (val Eio.Resource.get bops Eio.Fs.Pi.Dir) in
  let state = { base; boxed = Base_dir ((module B), bv); sched } in
  let handler = dir_handler (module B : Eio.Fs.Pi.PATH) in
  (Eio.Resource.T (state, handler) :> [ `Close | Eio.Fs.dir_ty ] r)

let () = wrap_dir_hook := wrap_dir

let wrap sched (base, path) =
  (wrap_dir ~base:(base :> Eio.Fs.dir_ty r) ~sched, path)

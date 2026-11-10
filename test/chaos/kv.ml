(* A key-value store: a write-ahead log, compacted into a snapshot by
   rename every [every] puts, and on open. Each [bug] is one mistake. *)

module P = Eio.Path

let parse s =
  let len = String.length s in
  let rec go off acc =
    if off + 8 > len then List.rev acc
    else
      let n = Int32.to_int (String.get_int32_be s off) in
      let k = Int32.to_int (String.get_int32_be s (off + 4)) land 0xffffffff in
      if n < 0 || off + 9 + n > len then List.rev acc
      else
        let p = String.sub s (off + 8) n in
        if App.Log.sum p = k then go (off + 9 + n) (p :: acc) else List.rev acc
  in
  go 0 []

let load_opt p =
  match P.load p with
  | s -> s
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ""

let exists p = P.kind ~follow:false p <> `Not_found

let bindings p =
  String.split_on_char '\n' p
  |> List.filter (( <> ) "")
  |> List.map (fun l ->
      let i = String.index l '=' in
      (String.sub l 0 i, String.sub l (i + 1) (String.length l - i - 1)))

let payload b = String.concat "" (List.map (fun (k, v) -> k ^ "=" ^ v ^ "\n") b)
let apply m (k, v) = (k, v) :: List.remove_assoc k m

type t = {
  dir : Eio.Fs.dir_ty P.t;
  sw : Eio.Switch.t;
  mutable wal : Eio.File.rw_ty Eio.Resource.t;
  mutable mem : (string * string) list;
  mutable n : int;
  bug : [ `None | `Ignore_snap_sync_error | `Wal_first | `Recovery_no_sync ];
}

let open_wal ~sw dir =
  P.open_out ~sw ~append:true ~create:(`If_missing 0o644) P.(dir / "wal")

let read_state dir =
  let snap =
    match parse (load_opt P.(dir / "snap")) with [ p ] -> bindings p | _ -> []
  in
  let wal = List.concat_map bindings (parse (load_opt P.(dir / "wal"))) in
  List.fold_left apply snap wal

let reset_wal dir =
  P.save ~create:(`Or_truncate 0o644) P.(dir / "wal.tmp") "";
  P.rename P.(dir / "wal.tmp") P.(dir / "wal");
  Eio_fs_sync.sync_dir dir

let write_snap ~sync ~ignore_sync_error dir mem =
  P.with_open_out ~create:(`Or_truncate 0o644)
    P.(dir / "snap.tmp")
    (fun f ->
      Eio.Flow.copy_string (App.Log.encode (payload mem)) f;
      if sync then
        if ignore_sync_error then try Eio.File.sync f with Eio.Io _ -> ()
        else Eio.File.sync f);
  P.rename P.(dir / "snap.tmp") P.(dir / "snap");
  Eio_fs_sync.sync_dir dir

let compact ?(on_open = false) t =
  let sync = not (on_open && t.bug = `Recovery_no_sync) in
  let ignore_sync_error = t.bug = `Ignore_snap_sync_error in
  if t.bug = `Wal_first then (
    reset_wal t.dir;
    write_snap ~sync ~ignore_sync_error t.dir t.mem)
  else (
    write_snap ~sync ~ignore_sync_error t.dir t.mem;
    reset_wal t.dir);
  t.wal <- open_wal ~sw:t.sw t.dir

let open_ ?(bug = `None) ~sw dir =
  let dir = (dir :> Eio.Fs.dir_ty P.t) in
  let mem = read_state dir in
  let wal = open_wal ~sw dir in
  Eio_fs_sync.sync_dir dir;
  let t = { dir; sw; wal; mem; n = 0; bug } in
  if load_opt P.(dir / "wal") <> "" || exists P.(dir / "snap.tmp") then
    compact ~on_open:true t;
  t

let every = ref 4

let put t k v =
  Eio.Flow.copy_string (App.Log.encode (payload [ (k, v) ])) t.wal;
  Eio.File.sync t.wal;
  t.mem <- apply t.mem (k, v);
  t.n <- t.n + 1;
  if t.n mod !every = 0 then compact t

let get t k = List.assoc_opt k t.mem

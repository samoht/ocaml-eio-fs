(* Workloads for the exhaustive check, and one run of a workload under
   Chaos.Crash. The fake barrier and the fake sync do nothing: they let a
   test check that the invariants catch a barrier that was never issued. *)

module C = Eio_fs.Chaos.Private.Crash
module Ch = Eio_fs.Chaos.Private
module P = Eio.Path
module I63 = Optint.Int63
module IM = Map.Make (Int64)

let user = { Eio_fs.uid = 1000; gid = 1000; groups = []; umask = 0o022 }

type who = Root | User

type op =
  | Write of who * string * int * int
  | Append of string * int
  | Trunc of who * string * int
  | Fsync of string
  | Fsync_fail of string
  | Mkdir of string
  | Rmdir of string
  | Unlink of string
  | Rename of string * string
  | Symlink of string * string (* link, target *)
  | Sync_dir of string
  | Sync_meta
  | Chmod of who * string * int
  | Chown of string * int
  | Open_h of string
  | Write_h of int * int
  | Sync_h
  | Open_dh of string
  | Sync_dh
  | Open_in_d of string
  | Sync_in_d
  | Fake_barrier of string
  | Fsync_fail_u of string
  | Fsync_u of string
  | Fake_fsync of string
  | Fsync_fail_s of string (* through a second, faultless wrapper *)
  | Fsync_fail_b of string (* failing beneath Chaos.Crash *)
  | Create of who * string * int

let who = function Root -> "" | User -> "u:"

let pp_op ppf = function
  | Write (w, p, o, l) -> Fmt.pf ppf "%swrite %s@%d+%d" (who w) p o l
  | Append (p, l) -> Fmt.pf ppf "append %s+%d" p l
  | Trunc (w, p, n) -> Fmt.pf ppf "%strunc %s %d" (who w) p n
  | Fsync p -> Fmt.pf ppf "fsync %s" p
  | Fsync_fail p -> Fmt.pf ppf "fsync! %s" p
  | Mkdir p -> Fmt.pf ppf "mkdir %s" p
  | Rmdir p -> Fmt.pf ppf "rmdir %s" p
  | Unlink p -> Fmt.pf ppf "unlink %s" p
  | Rename (a, b) -> Fmt.pf ppf "rename %s %s" a b
  | Symlink (l, t) -> Fmt.pf ppf "symlink %s->%s" l t
  | Sync_dir p -> Fmt.pf ppf "syncdir %s" p
  | Sync_meta -> Fmt.pf ppf "sync_metadata"
  | Chmod (w, p, m) -> Fmt.pf ppf "%schmod %s %o" (who w) p m
  | Chown (p, u) -> Fmt.pf ppf "chown %s %d" p u
  | Open_h p -> Fmt.pf ppf "h=open %s" p
  | Write_h (o, l) -> Fmt.pf ppf "write h@%d+%d" o l
  | Sync_h -> Fmt.pf ppf "fsync h"
  | Open_dh p -> Fmt.pf ppf "dh=open_dir %s" p
  | Sync_dh -> Fmt.pf ppf "syncdir dh"
  | Open_in_d p -> Fmt.pf ppf "dfh=open_in %s" p
  | Sync_in_d -> Fmt.pf ppf "fsync dfh"
  | Fake_barrier p -> Fmt.pf ppf "FAKE-syncdir %s" p
  | Fsync_fail_u p -> Fmt.pf ppf "u:fsync! %s" p
  | Fsync_u p -> Fmt.pf ppf "u:fsync %s" p
  | Fake_fsync p -> Fmt.pf ppf "FAKE-fsync %s" p
  | Fsync_fail_s p -> Fmt.pf ppf "fsync!s %s" p
  | Fsync_fail_b p -> Fmt.pf ppf "fsync!b %s" p
  | Create (w, p, m) -> Fmt.pf ppf "%screate %s %o" (who w) p m

let pp_wl = Fmt.(list ~sep:(any "; ") pp_op)

type init =
  | IDir of string * int
  | IDir_g of string * int * int (* path, perm, gid; owned by root *)
  | IFile of string * string * int * int (* path, contents, perm, uid *)
  | ILink of string * string

type kind = F of string | D | L of string
type node = { kind : kind; perm : int; uid : int64; gid : int64 }

module TS = Set.Make (struct
  type t = int64 * string * int64

  let compare = compare
end)

type snap = { triples : TS.t; nodes : node IM.t; paths : (string * int64) list }

let snapshot base =
  let root = (P.stat ~follow:false base).ino in
  let triples = ref TS.empty and nodes = ref IM.empty and paths = ref [] in
  let st0 = P.stat ~follow:false base in
  nodes :=
    IM.add root
      { kind = D; perm = st0.perm; uid = st0.uid; gid = st0.gid }
      !nodes;
  let rec walk prefix p ino =
    List.iter
      (fun name ->
        let q = P.(p / name) in
        let st = P.stat ~follow:false q in
        let kind =
          match st.kind with
          | `Directory -> D
          | `Symbolic_link -> L (P.read_link q)
          | `Regular_file -> F (P.load q)
          | _ -> failwith "kind"
        in
        let path = if prefix = "" then name else prefix ^ "/" ^ name in
        triples := TS.add (ino, name, st.ino) !triples;
        paths := (path, st.ino) :: !paths;
        nodes :=
          IM.add st.ino
            { kind; perm = st.perm; uid = st.uid; gid = st.gid }
            !nodes;
        if kind = D then walk path q st.ino)
      (List.sort compare (P.read_dir p))
  in
  walk "" base root;
  (root, { triples = !triples; nodes = !nodes; paths = !paths })

let setup base init =
  List.iter
    (function
      | IDir (p, perm) ->
          P.mkdir ~perm P.(base / p);
          P.chmod ~follow:false ~perm P.(base / p)
      | IDir_g (p, perm, gid) ->
          P.mkdir ~perm P.(base / p);
          P.chown ~follow:false ~uid:0L ~gid:(Int64.of_int gid) P.(base / p);
          P.chmod ~follow:false ~perm P.(base / p)
      | IFile (p, s, perm, uid) ->
          P.save ~create:(`Exclusive perm) P.(base / p) s;
          P.chmod ~follow:false ~perm P.(base / p);
          if uid <> 0 then
            P.chown ~follow:false ~uid:(Int64.of_int uid)
              ~gid:(Int64.of_int uid)
              P.(base / p)
      | ILink (p, t) -> P.symlink ~link_to:t P.(base / p))
    init

exception Invalid of int * string
exception Anomaly of string

type handles = {
  mutable fh : Eio.File.rw_ty Eio.Resource.t option;
  mutable dh : Eio.Fs.dir_ty P.t option;
  mutable dfh : Eio.File.ro_ty Eio.Resource.t option;
}

let fail_sched () =
  Ch.Schedule.seeded ~probability:1.0 ~kinds:[ `Sync_error ] ~seed:0 ()

let must_fail f =
  match f () with
  | () -> raise (Anomaly "failing sync did not fail")
  | exception Eio.Io _ -> ()

let exec ~sw ~w ~wu ~dry hs i op =
  let ch = Char.chr (Char.code 'a' + i) in
  let buf n = [ Cstruct.of_string (String.make n ch) ] in
  let d = function Root -> w | User -> wu in
  match op with
  | Write (u, p, off, len) ->
      P.with_open_out ~create:(`If_missing 0o666)
        P.(d u / p)
        (fun h -> Eio.File.pwrite_all h ~file_offset:(I63.of_int off) (buf len))
  | Append (p, len) ->
      P.with_open_out ~append:true ~create:(`If_missing 0o666)
        P.(w / p)
        (fun h -> Eio.Flow.write h (buf len))
  | Trunc (u, p, n) ->
      P.with_open_out ~create:`Never
        P.(d u / p)
        (fun h -> Eio.File.truncate h (I63.of_int n))
  | Fsync p -> P.with_open_out ~create:`Never P.(w / p) Eio.File.sync
  | Fsync_fail p ->
      let wf = Ch.wrap (fail_sched ()) w in
      must_fail (fun () ->
          P.with_open_out ~create:`Never P.(wf / p) Eio.File.sync)
  | Fsync_fail_s p ->
      let wf = Ch.wrap (fail_sched ()) (Ch.wrap Ch.Schedule.empty w) in
      must_fail (fun () ->
          P.with_open_out ~create:`Never P.(wf / p) Eio.File.sync)
  | Fsync_fail_b p ->
      let sync () = P.with_open_out ~create:`Never P.(w / p) Eio.File.sync in
      if dry then sync () else must_fail sync
  | Fsync_u p -> P.with_open_out ~create:`Never P.(wu / p) Eio.File.sync
  | Fsync_fail_u p ->
      let wf = Ch.wrap (fail_sched ()) wu in
      must_fail (fun () ->
          P.with_open_out ~create:`Never P.(wf / p) Eio.File.sync)
  | Create (u, p, perm) ->
      P.with_open_out ~create:(`Exclusive perm) P.(d u / p) ignore
  | Mkdir p -> P.mkdir ~perm:0o755 P.(w / p)
  | Rmdir p -> P.rmdir P.(w / p)
  | Unlink p -> P.unlink P.(w / p)
  | Rename (a, b) -> P.rename P.(w / a) P.(w / b)
  | Symlink (l, t) -> P.symlink ~link_to:t P.(w / l)
  | Sync_dir p -> Eio_fs_sync.sync_dir P.(w / p)
  | Sync_meta | Fake_barrier _ | Fake_fsync _ -> ()
  | Chmod (u, p, m) -> P.chmod ~follow:false ~perm:m P.(d u / p)
  | Chown (p, u) ->
      P.chown ~follow:false ~uid:(Int64.of_int u) ~gid:(Int64.of_int u)
        P.(w / p)
  | Open_h p ->
      if hs.fh <> None then raise Exit;
      hs.fh <-
        Some
          (P.open_out ~sw ~create:(`If_missing 0o666) P.(w / p)
            :> Eio.File.rw_ty Eio.Resource.t)
  | Write_h (off, len) -> (
      match hs.fh with
      | None -> raise Exit
      | Some h -> Eio.File.pwrite_all h ~file_offset:(I63.of_int off) (buf len))
  | Sync_h -> (
      match hs.fh with None -> raise Exit | Some h -> Eio.File.sync h)
  | Open_dh p ->
      if hs.dh <> None then raise Exit;
      hs.dh <- Some (P.open_dir ~sw P.(w / p) :> Eio.Fs.dir_ty P.t)
  | Sync_dh -> (
      match hs.dh with None -> raise Exit | Some d -> Eio_fs_sync.sync_dir d)
  | Open_in_d p ->
      if hs.dfh <> None then raise Exit;
      if P.kind ~follow:true P.(w / p) <> `Directory then raise Exit;
      hs.dfh <- Some (P.open_in ~sw P.(w / p) :> Eio.File.ro_ty Eio.Resource.t)
  | Sync_in_d -> (
      match hs.dfh with
      | None -> raise Exit
      | Some h -> (
          match Eio_fs_sync.file_barrier h with
          | Some b -> b ()
          | None -> raise (Anomaly "no file barrier on dir handle")))

(* Fresh fs, durable init, ops through Chaos.Crash over a Chaos wrapper with
   [sched]. [observe] is called with the raw base after each op, [at] with
   the index of the op about to run. *)
let exec_all ~sched ~dry ~observe ~at ~init ~policy ops k =
  Eio.Switch.run @@ fun sw ->
  let base = Eio_fs.Fs.v () in
  setup base init;
  observe 0 base;
  let t = C.v ~policy ~block_size:2 () in
  (* One controller, one wrapper: a workload with a user's operation runs
     all of them as that user. *)
  let has_user =
    List.exists
      (function
        | Write (User, _, _, _)
        | Trunc (User, _, _)
        | Chmod (User, _, _)
        | Create (User, _, _)
        | Fsync_u _ | Fsync_fail_u _ ->
            true
        | _ -> false)
      ops
  in
  let inner = if has_user then Eio_fs.Fs.as_user user base else base in
  let w = C.wrap t (Ch.wrap sched inner) in
  let wu = w in
  let hs = { fh = None; dh = None; dfh = None } in
  List.iteri
    (fun i op ->
      at i;
      (try exec ~sw ~w ~wu ~dry hs i op with
      | Anomaly _ as e -> raise e
      | e -> raise (Invalid (i, Printexc.to_string e)));
      if op = Sync_meta then C.sync_metadata t;
      observe (i + 1) base)
    ops;
  k t base

(* The rules that fail each [Fsync_fail_b] beneath Chaos.Crash: the first
   sync it sends down, by its rank among the syncs on its path, found by a
   run that lets it succeed. *)
let beneath_rules ~init ops =
  if not (List.exists (function Fsync_fail_b _ -> true | _ -> false) ops) then
    []
  else begin
    let cur = ref (-1) and counts = Hashtbl.create 4 and rules = ref [] in
    let failing i =
      match List.nth ops i with Fsync_fail_b _ -> true | _ -> false
    in
    let f (op : Ch.Op.t) =
      if op.kind = `Sync then begin
        let n = 1 + Option.value ~default:0 (Hashtbl.find_opt counts op.path) in
        Hashtbl.replace counts op.path n;
        if failing !cur && not (List.mem_assoc !cur !rules) then
          rules :=
            ( !cur,
              {
                Ch.Schedule.trigger =
                  Nth_op_on_path { n; kind = `Sync; path = op.path };
                fault = Sync_error `Eio;
              } )
            :: !rules
      end
    in
    exec_all
      ~sched:(Ch.Schedule.observing f Ch.Schedule.empty)
      ~dry:true
      ~observe:(fun _ _ -> ())
      ~at:(fun i -> cur := i)
      ~init ~policy:C.Drop_unsynced ops
      (fun _ _ -> ());
    List.rev_map snd !rules
  end

(* One run: fresh fs, durable init, ops, crash by [policy]. [observe] is
   called with the raw base after each op (oracle run only). *)
let run ?(observe = fun _ _ -> ()) ?(before_crash = fun _ -> ()) ~init ~policy
    ops =
  let sched = Ch.Schedule.scripted (beneath_rules ~init ops) in
  exec_all ~sched ~dry:false ~observe ~at:ignore ~init ~policy ops
  @@ fun t base ->
  before_crash t;
  let _pc = C.post_crash t in
  let pc = base in
  let img = Ch.Internal.crash_image t in
  let img =
    List.map
      (fun (p, ino, c) ->
        let st = P.stat ~follow:false P.(pc / p) in
        (p, ino, c, st.perm, st.uid, st.gid))
      img
  in
  List.sort compare img

type image =
  (string
  * int64
  * [ `File of string | `Dir | `Link of string ]
  * int
  * int64
  * int64)
  list

let pp_image ppf (img : image) =
  List.iter
    (fun (p, ino, c, perm, uid, _) ->
      Fmt.pf ppf "%s#%Ld%s:%o:%Ld " p ino
        (match c with
        | `File s -> "=" ^ String.escaped s
        | `Dir -> "/"
        | `Link l -> "->" ^ l)
        perm uid)
    img

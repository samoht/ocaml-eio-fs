(* An exhaustive small-scope check of Chaos.Crash.

   Every workload up to a bound is enumerated; for each, every crash state
   eio-fs produces (Drop, every Survive cut x data subset, Reorder at block
   size 1 over seeds until no new image appears) is checked against a few
   application-level invariants derived from chaos.mli, not against a model:

   (a) bytes durable at the last good sync (or initially) are present, or
       replaced by a later write;
   (b/f) a name's binding is one it had at or after the last barrier on its
       directory (so a covered creation exists and a covered removal is gone);
   (c) a file durably linked and still linked at the end is somewhere;
   (d) no inode appears twice;
   (e) every byte is one that inode had at that offset, or zero;
   (g) modes/owners are ones the inode had. *)

module Crash = Eio_fs.Chaos.Crash
module Chaos = Eio_fs.Chaos

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

type op =
  | Creat of string
  | Write of string * int * char
  | Trunc of string * int
  | Rename of string * string
  | Unlink of string
  | Mkdir of string
  | Rmdir of string
  | Fsync of string
  | Fsync_fail of string
  | Fsyncdir of string
  | Chmod of string * int
  | Sync_meta

let pp_op ppf = function
  | Creat p -> Fmt.pf ppf "creat %s" p
  | Write (p, o, c) -> Fmt.pf ppf "write %s@%d %C" p o c
  | Trunc (p, n) -> Fmt.pf ppf "truncate %s %d" p n
  | Rename (a, b) -> Fmt.pf ppf "rename %s %s" a b
  | Unlink p -> Fmt.pf ppf "unlink %s" p
  | Mkdir p -> Fmt.pf ppf "mkdir %s" p
  | Rmdir p -> Fmt.pf ppf "rmdir %s" p
  | Fsync p -> Fmt.pf ppf "fsync %s" p
  | Fsync_fail p -> Fmt.pf ppf "fsync %s FAILS" p
  | Fsyncdir p -> Fmt.pf ppf "fsyncdir %S" p
  | Chmod (p, m) -> Fmt.pf ppf "chmod %s %o" p m
  | Sync_meta -> Fmt.pf ppf "sync_metadata"

type init = {
  idirs : string list;
  ifiles : (string * string) list;
  imodes : (string * int) list;
}

let pp_init ppf i =
  Fmt.pf ppf "[%a]"
    Fmt.(list ~sep:(any "; ") string)
    (List.map (fun d -> d ^ "/") i.idirs
    @ List.map (fun (p, c) -> Fmt.str "%s=%S" p c) i.ifiles)

(* {1 Tracker: the live namespace plus what the invariants need} *)

type bst = Durable | Pending | Lost

type inode = {
  id : int;
  is_dir : bool;
  mutable data : (char * bst) array;
  allowed : (int, char list) Hashtbl.t; (* (a): byte -> allowed values *)
  mutable dlen : int; (* (a): minimum length *)
  ever : (int, char list) Hashtbl.t; (* (e) *)
  mutable modes : (int * int * int) list; (* (g) *)
}

type tr = {
  inodes : (int, inode) Hashtbl.t;
  mutable next : int;
  ns : (int * string, int) Hashtbl.t; (* live (dir id, name) -> inode *)
  hist : (int * string, (int * int option) list) Hashtbl.t;
      (* key -> (time, binding) newest first *)
  bar : (int, int) Hashtbl.t; (* dir id -> last barrier time *)
  mutable time : int;
}

let add_l tbl k v =
  let l = Option.value ~default:[] (Hashtbl.find_opt tbl k) in
  if not (List.mem v l) then Hashtbl.replace tbl k (v :: l)

let new_inode t ~is_dir =
  let id = t.next in
  t.next <- id + 1;
  let n =
    {
      id;
      is_dir;
      data = [||];
      allowed = Hashtbl.create 4;
      dlen = 0;
      ever = Hashtbl.create 4;
      modes = [];
    }
  in
  Hashtbl.replace t.inodes id n;
  n

let split p =
  match String.rindex_opt p '/' with
  | None -> ("", p)
  | Some i -> (String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1))

let rec resolve t p =
  if p = "" then Some 0
  else
    let d, n = split p in
    match resolve t d with
    | None -> None
    | Some did -> Hashtbl.find_opt t.ns (did, n)

let node t id = Hashtbl.find t.inodes id

let is_dir_p t p =
  match resolve t p with Some i -> (node t i).is_dir | None -> false

let is_file_p t p =
  match resolve t p with Some i -> not (node t i).is_dir | None -> false

let bind t key b =
  (match b with
  | Some i -> Hashtbl.replace t.ns key i
  | None -> Hashtbl.remove t.ns key);
  let h = Option.value ~default:[] (Hashtbl.find_opt t.hist key) in
  Hashtbl.replace t.hist key ((t.time, b) :: h)

let key_of t p =
  let d, n = split p in
  match resolve t d with Some did -> Some (did, n) | None -> None

let tr_init init =
  let t =
    {
      inodes = Hashtbl.create 8;
      next = 0;
      ns = Hashtbl.create 8;
      hist = Hashtbl.create 8;
      bar = Hashtbl.create 4;
      time = -1;
    }
  in
  let _root = new_inode t ~is_dir:true in
  List.iter
    (fun d ->
      let n = new_inode t ~is_dir:true in
      bind t (Option.get (key_of t d)) (Some n.id))
    init.idirs;
  List.iter
    (fun (p, c) ->
      let n = new_inode t ~is_dir:false in
      n.data <- Array.init (String.length c) (fun i -> (c.[i], Durable));
      String.iteri
        (fun i ch ->
          Hashtbl.replace n.allowed i [ ch ];
          add_l n.ever i ch)
        c;
      n.dlen <- String.length c;
      bind t (Option.get (key_of t p)) (Some n.id))
    init.ifiles;
  t

let valid t = function
  | Creat p -> (
      match key_of t p with Some k -> not (Hashtbl.mem t.ns k) | None -> false)
  | Write (p, _, _) | Trunc (p, _) | Unlink p | Fsync p | Fsync_fail p ->
      is_file_p t p
  | Rename (a, b) when is_dir_p t a -> (
      a <> b
      && (not (String.starts_with ~prefix:(a ^ "/") b))
      &&
      match key_of t b with
      | None -> false
      | Some k -> (
          match Hashtbl.find_opt t.ns k with
          | None -> true
          | Some i ->
              (node t i).is_dir
              && not
                   (Hashtbl.fold (fun (d, _) _ acc -> acc || d = i) t.ns false))
      )
  | Rename (a, b) -> (
      a <> b && is_file_p t a
      &&
      match key_of t b with
      | None -> false
      | Some k -> (
          match Hashtbl.find_opt t.ns k with
          | None -> true
          | Some i -> not (node t i).is_dir))
  | Mkdir p -> (
      match key_of t p with Some k -> not (Hashtbl.mem t.ns k) | None -> false)
  | Rmdir p ->
      is_dir_p t p
      &&
      let id = Option.get (resolve t p) in
      not (Hashtbl.fold (fun (d, _) _ acc -> acc || d = id) t.ns false)
  | Fsyncdir p -> is_dir_p t p
  | Chmod (p, _) -> resolve t p <> None
  | Sync_meta -> true

let write_bytes n off s =
  let len = Array.length n.data in
  let nl = max len (off + String.length s) in
  let d = Array.make nl ('\000', Pending) in
  Array.blit n.data 0 d 0 len;
  for i = len to off - 1 do
    d.(i) <- ('\000', Pending);
    add_l n.ever i '\000';
    match Hashtbl.find_opt n.allowed i with
    | Some l -> Hashtbl.replace n.allowed i ('\000' :: l)
    | None -> ()
  done;
  String.iteri
    (fun j c ->
      let i = off + j in
      d.(i) <- (c, Pending);
      add_l n.ever i c;
      match Hashtbl.find_opt n.allowed i with
      | Some l -> Hashtbl.replace n.allowed i (c :: l)
      | None -> ())
    s;
  n.data <- d

let truncate n m =
  let len = Array.length n.data in
  if m < len then begin
    n.data <- Array.sub n.data 0 m;
    n.dlen <- min n.dlen m;
    for i = m to len - 1 do
      match Hashtbl.find_opt n.allowed i with
      | Some l -> Hashtbl.replace n.allowed i ('\000' :: l)
      | None -> ()
    done
  end
  else if m > len then write_bytes n len (String.make (m - len) '\000')

let good_sync n =
  let last = ref 0 in
  Array.iteri
    (fun i (c, st) ->
      match st with
      | Durable | Pending ->
          Hashtbl.replace n.allowed i [ c ];
          n.data.(i) <- (c, Durable);
          last := i + 1
      | Lost -> ())
    n.data;
  (* bytes past the live length: no longer constrained *)
  Hashtbl.filter_map_inplace
    (fun i l -> if i >= Array.length n.data then None else Some l)
    n.allowed;
  n.dlen <- !last

let failed_sync n =
  Array.iteri
    (fun i (c, st) -> if st = Pending then n.data.(i) <- (c, Lost))
    n.data

(* Apply [op] at time [t.time]; the op is known valid. *)
let tr_apply t op =
  t.time <- t.time + 1;
  let file p = node t (Option.get (resolve t p)) in
  match op with
  | Creat p ->
      let n = new_inode t ~is_dir:false in
      bind t (Option.get (key_of t p)) (Some n.id)
  | Mkdir p ->
      let n = new_inode t ~is_dir:true in
      bind t (Option.get (key_of t p)) (Some n.id)
  | Write (p, off, c) -> write_bytes (file p) off (String.make 1 c)
  | Trunc (p, m) -> truncate (file p) m
  | Rename (a, b) ->
      let ka = Option.get (key_of t a) and kb = Option.get (key_of t b) in
      let x = Hashtbl.find t.ns ka in
      bind t ka None;
      bind t kb (Some x)
  | Unlink p | Rmdir p -> bind t (Option.get (key_of t p)) None
  | Fsync p -> good_sync (file p)
  | Fsync_fail p -> failed_sync (file p)
  | Fsyncdir p -> Hashtbl.replace t.bar (Option.get (resolve t p)) t.time
  | Chmod _ -> ()
  | Sync_meta ->
      Hashtbl.iter
        (fun _ n -> if n.is_dir then Hashtbl.replace t.bar n.id t.time)
        t.inodes

(* Allowed bindings of a key: in effect at its directory's last barrier, or
   set after it. *)
let allowed_bindings t (did, name) =
  let tb = Option.value ~default:(-1) (Hashtbl.find_opt t.bar did) in
  let h = Option.value ~default:[] (Hashtbl.find_opt t.hist (did, name)) in
  let after = List.filter (fun (tm, _) -> tm > tb) h |> List.map snd in
  let at =
    match List.find_opt (fun (tm, _) -> tm <= tb) h with
    | Some (_, b) -> b
    | None -> None
  in
  List.sort_uniq compare (at :: after)

(* {1 Running eio-fs} *)

type entry = {
  path : string;
  ino : int; (* tracker id, -1 if unknown *)
  kind : [ `F of string | `D ];
  mode : int * int * int;
}

let pp_entry ppf e =
  let m, u, g = e.mode in
  match e.kind with
  | `D -> Fmt.pf ppf "%s/ #%d %o %d:%d" e.path e.ino m u g
  | `F c -> Fmt.pf ppf "%s=%S #%d %o %d:%d" e.path c e.ino m u g

type user_kind = Root | Plain

let eio_user = function
  | Root -> Eio_fs.root
  | Plain -> { Eio_fs.uid = 1000; gid = 1000; groups = []; umask = 0o022 }

let exec ~ctl cw op =
  let p x = cw / x in
  match op with
  | Creat x -> Eio.Path.with_open_out ~create:(`Exclusive 0o644) (p x) ignore
  | Write (x, off, c) ->
      Eio.Path.with_open_out ~create:`Never (p x) (fun f ->
          Eio.File.pwrite_all f ~file_offset:(i63 off)
            [ Cstruct.of_string (String.make 1 c) ])
  | Trunc (x, n) ->
      Eio.Path.with_open_out ~create:`Never (p x) (fun f ->
          Eio.File.truncate f (i63 n))
  | Rename (a, b) -> Eio.Path.rename (p a) (p b)
  | Unlink x -> Eio.Path.unlink (p x)
  | Mkdir x -> Eio.Path.mkdir ~perm:0o755 (p x)
  | Rmdir x -> Eio.Path.rmdir (p x)
  | Fsync x -> Eio.Path.with_open_out ~create:`Never (p x) Eio.File.sync
  | Fsync_fail x -> (
      let sched =
        Chaos.Schedule.scripted
          [
            {
              trigger = Op_on_path { kind = `Sync; path = snd (cw / x) };
              fault = Sync_error `Eio;
            };
          ]
      in
      let xw = Chaos.wrap sched cw in
      match Eio.Path.with_open_out ~create:`Never (xw / x) Eio.File.sync with
      | () -> failwith "injected sync did not fail"
      | exception Eio.Io _ -> ())
  | Fsyncdir x -> Eio_fs_sync.sync_dir (p x)
  | Chmod (x, perm) -> Eio.Path.chmod ~follow:true ~perm (p x)
  | Sync_meta -> Crash.sync_metadata ctl

let stat_mode path =
  let st = Eio.Path.stat ~follow:false path in
  (st.perm, Int64.to_int st.uid, Int64.to_int st.gid, st.ino)

exception Live_mismatch of string

(* Run [ops] from [init]; [modes] records each inode's modes live, [inos]
   maps eio inode -> tracker id; returns the image and the unsynced list. *)
let run ~user ~init ~ops ~policy ~bs =
  Eio_mock.Backend.run @@ fun () ->
  let base = Eio_fs.Fs.v ~user:(eio_user user) () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (base / d)) init.idirs;
  List.iter
    (fun (p, c) -> Eio.Path.save ~create:(`Exclusive 0o644) (base / p) c)
    init.ifiles;
  List.iter
    (fun (p, m) -> Eio.Path.chmod ~follow:true ~perm:m (base / p))
    init.imodes;
  let ctl = Crash.v ~policy ~block_size:bs () in
  let cw = Crash.wrap ctl base in
  ops
  |> List.iter (fun op ->
      match exec ~ctl cw op with
      | () -> ()
      | exception e ->
          raise
            (Live_mismatch (Fmt.str "%a: %s" pp_op op (Printexc.to_string e))));
  let uns = Crash.unsynced ctl in
  Crash.crash ctl;
  let post = Eio_fs.Fs.as_user Eio_fs.root (Crash.post_crash ctl) in
  let img =
    List.map
      (fun (path, ino, kind) ->
        let m, u, g, _ = stat_mode (post / path) in
        let kind =
          match kind with
          | `File c -> `F c
          | `Dir -> `D
          | `Link _ -> failwith "link"
        in
        (path, ino, kind, (m, u, g)))
      (Eio_fs.Chaos.Private.crash_image ctl)
  in
  (img, uns)

(* The live run, once per workload: inode map and modes seen. *)
let live ~user ~init ~ops (t : tr) =
  Eio_mock.Backend.run @@ fun () ->
  let base = Eio_fs.Fs.v ~user:(eio_user user) () in
  List.iter (fun d -> Eio.Path.mkdir ~perm:0o755 (base / d)) init.idirs;
  List.iter
    (fun (p, c) -> Eio.Path.save ~create:(`Exclusive 0o644) (base / p) c)
    init.ifiles;
  List.iter
    (fun (p, m) -> Eio.Path.chmod ~follow:true ~perm:m (base / p))
    init.imodes;
  let ctl = Crash.v () in
  let cw = Crash.wrap ctl base in
  let inos = Hashtbl.create 8 in
  (* replay the tracker alongside *)
  let tt = tr_init init in
  let note () =
    let rec go rel did =
      Hashtbl.iter
        (fun (d, name) id ->
          if d = did then begin
            let r = if rel = "" then name else rel ^ "/" ^ name in
            let m, u, g, ino = stat_mode (cw / r) in
            Hashtbl.replace inos (Int64.to_int ino) id;
            let n = node t id in
            if not (List.mem (m, u, g) n.modes) then
              n.modes <- (m, u, g) :: n.modes;
            if n.is_dir then go r id
          end)
        tt.ns
    in
    go "" 0
  in
  note ();
  List.iter
    (fun op ->
      (match exec ~ctl cw op with
      | () -> ()
      | exception e ->
          raise
            (Live_mismatch (Fmt.str "%a: %s" pp_op op (Printexc.to_string e))));
      tr_apply tt op;
      note ())
    ops;
  inos

(* {1 Checking an image} *)

let check_image ~(t : tr) ~inos img =
  let errs = ref [] in
  let err cls fmt = Fmt.kstr (fun s -> errs := (cls, s) :: !errs) fmt in
  let id_of ino =
    Option.value ~default:(-1) (Hashtbl.find_opt inos (Int64.to_int ino))
  in
  let entries =
    List.map
      (fun (path, ino, kind, mode) -> { path; ino = id_of ino; kind; mode })
      img
  in
  (* (d) *)
  let ids = List.map (fun e -> e.ino) entries in
  List.iter
    (fun e ->
      if e.ino < 0 then
        err "e-unknown-inode" "%s has an inode never made" e.path
      else if List.length (List.filter (( = ) e.ino) ids) > 1 then
        err "d-dup-inode" "inode #%d at %s and elsewhere" e.ino e.path)
    entries;
  let by_path = Hashtbl.create 8 in
  List.iter (fun e -> Hashtbl.replace by_path e.path e) entries;
  (* dir id of an image path *)
  let dir_id p =
    if p = "" then Some 0
    else Option.map (fun e -> e.ino) (Hashtbl.find_opt by_path p)
  in
  (* (e), (g), (a) per entry *)
  List.iter
    (fun e ->
      if e.ino >= 0 then begin
        let n = node t e.ino in
        if not (List.mem e.mode n.modes) then begin
          let m, u, g = e.mode in
          err "g-mode" "%s #%d has mode %o %d:%d, never had (had %a)" e.path
            e.ino m u g
            Fmt.(list ~sep:comma (fun ppf (m, u, g) -> pf ppf "%o %d:%d" m u g))
            n.modes
        end;
        match e.kind with
        | `D ->
            if not n.is_dir then
              err "kind" "%s is a dir, #%d is a file" e.path e.ino
        | `F c ->
            if n.is_dir then
              err "kind" "%s is a file, #%d is a dir" e.path e.ino
            else begin
              String.iteri
                (fun i ch ->
                  let ev =
                    Option.value ~default:[] (Hashtbl.find_opt n.ever i)
                  in
                  if ch <> '\000' && not (List.mem ch ev) then
                    err "e-never-written"
                      "%s #%d byte %d = %C never written there" e.path e.ino i
                      ch;
                  match Hashtbl.find_opt n.allowed i with
                  | Some l when not (List.mem ch l) ->
                      err "a-durable-byte"
                        "%s #%d byte %d = %C, durable bound allows %a" e.path
                        e.ino i ch
                        Fmt.(list ~sep:comma (fun ppf c -> pf ppf "%C" c))
                        l
                  | _ -> ())
                c;
              if String.length c < n.dlen then
                err "a-durable-len" "%s #%d length %d < durable %d" e.path e.ino
                  (String.length c) n.dlen
            end
      end)
    entries;
  (* (b)/(f): every key under a directory the image holds *)
  let keys = Hashtbl.create 8 in
  Hashtbl.iter (fun k _ -> Hashtbl.replace keys k ()) t.hist;
  (* image path -> key *)
  List.iter
    (fun e ->
      let d, name = split e.path in
      match dir_id d with
      | Some did when did >= 0 ->
          let ok = allowed_bindings t (did, name) in
          if not (List.mem (Some e.ino) ok) then
            err "bf-binding" "%s = #%d; %S in dir #%d allows %a" e.path e.ino
              name did
              Fmt.(list ~sep:comma (option ~none:(any "absent") (fmt "#%d")))
              ok
      | _ -> ())
    entries;
  (* absent entries: keys whose dir is in the image, allowed set lacks None *)
  let dirs_in_image =
    (0, "")
    :: List.filter_map
         (fun e -> match e.kind with `D -> Some (e.ino, e.path) | _ -> None)
         entries
  in
  Hashtbl.iter
    (fun (did, name) () ->
      match List.assoc_opt did dirs_in_image with
      | None -> ()
      | Some dp ->
          let p = if dp = "" then name else dp ^ "/" ^ name in
          if not (Hashtbl.mem by_path p) then begin
            let ok = allowed_bindings t (did, name) in
            if not (List.mem None ok) then
              err "bf-missing" "%s absent; dir #%d allows %a" p did
                Fmt.(list ~sep:comma (option ~none:(any "absent") (fmt "#%d")))
                ok
          end)
    keys;
  (* (c): a file durably linked (in a durable dir) and linked at the end *)
  let at_barrier key =
    let did, _ = key in
    let tb = Option.value ~default:(-1) (Hashtbl.find_opt t.bar did) in
    match Hashtbl.find_opt t.hist key with
    | None -> None
    | Some h -> (
        match List.find_opt (fun (tm, _) -> tm <= tb) h with
        | Some (_, b) -> b
        | None -> None)
  in
  let rec durable_dir did =
    did = 0
    || Hashtbl.fold
         (fun ((pd, _) as key) _ acc ->
           acc || (pd <> did && at_barrier key = Some did && durable_dir pd))
         t.hist false
  in
  (* Linked at the end below a durable directory: one a committed rename
     moved it into a directory that is not durable is lost with it
     (chaos.mli). *)
  let linked =
    Hashtbl.fold
      (fun (d, _) i acc -> if durable_dir d then i :: acc else acc)
      t.ns []
  in
  Hashtbl.iter
    (fun ((did, name) as key) _ ->
      match at_barrier key with
      | Some x
        when durable_dir did && (not (node t x).is_dir) && List.mem x linked
             && not (List.mem x ids) ->
          err "c-vanished"
            "file #%d durably at %S in #%d, linked at end, absent" x name did
      | _ -> ())
    t.hist;
  List.rev !errs

(* {1 Enumeration} *)

let image_key img =
  Fmt.str "%a"
    Fmt.(
      list ~sep:(any ";") (fun ppf (p, ino, k, (m, u, g)) ->
          pf ppf "%s#%Ld%s%o%d%d" p ino
            (match k with `F c -> "=" ^ String.escaped c | `D -> "/")
            m u g))
    (List.sort compare img)

let is_meta (u : Crash.unsynced) =
  match u.kind with Data | Truncate -> false | _ -> true

(* Reorder seeds are drawn until [quiet] in a row add no image, at most
   [max]. *)
let max_seeds = ref 3000
let quiet_seeds = ref 100

type stats = {
  mutable workloads : int;
  mutable states : int;
  mutable unsaturated : int;
  mutable runs : int;
}

let st = { workloads = 0; states = 0; unsaturated = 0; runs = 0 }

let check_workload ~user ~init ~ops ~report =
  let t = tr_init init in
  List.iter (tr_apply t) ops;
  match live ~user ~init ~ops t with
  | exception Live_mismatch s -> report "live-mismatch" "-" s
  | inos ->
      st.workloads <- st.workloads + 1;
      let seen = Hashtbl.create 64 in
      let consider pol (img, _) =
        st.runs <- st.runs + 1;
        let k = image_key img in
        if Hashtbl.mem seen k then false
        else begin
          Hashtbl.replace seen k ();
          st.states <- st.states + 1;
          List.iter
            (fun (cls, msg) ->
              report cls pol
                (Fmt.str "%s@.    image: %a" msg
                   Fmt.(
                     list ~sep:(any ", ") (fun ppf (p, ino, k, (m, u, g)) ->
                         let id =
                           Option.value ~default:(-1)
                             (Hashtbl.find_opt inos (Int64.to_int ino))
                         in
                         match k with
                         | `D -> pf ppf "%s/#%d" p id
                         | `F c -> pf ppf "%s=%S#%d(%o %d:%d)" p c id m u g))
                   img))
            (check_image ~t ~inos img);
          true
        end
      in
      let drop = run ~user ~init ~ops ~policy:Crash.Drop_unsynced ~bs:4096 in
      ignore (consider "Drop" drop);
      let uns = snd drop in
      let meta = List.filter is_meta uns
      and data = List.filter (fun u -> not (is_meta u)) uns in
      let nd = List.length data in
      for k = 0 to List.length meta do
        let keep_meta =
          List.filteri (fun i _ -> i < k) meta
          |> List.map (fun (u : Crash.unsynced) -> u.seq)
        in
        for mask = 0 to (1 lsl nd) - 1 do
          let keep_data =
            List.filteri (fun i _ -> mask land (1 lsl i) <> 0) data
            |> List.map (fun (u : Crash.unsynced) -> u.seq)
          in
          let pred (u : Crash.unsynced) =
            if is_meta u then List.mem u.seq keep_meta
            else List.mem u.seq keep_data
          in
          List.iter
            (fun bs ->
              ignore
                (consider
                   (Fmt.str "Survive(k=%d,mask=%d,bs=%d)" k mask bs)
                   (run ~user ~init ~ops ~policy:(Crash.Survive pred) ~bs)))
            [ 1; 4096 ]
        done
      done;
      let quiet = ref 0 and seed = ref 0 in
      while !quiet < !quiet_seeds && !seed < !max_seeds do
        let r =
          run ~user ~init ~ops ~policy:(Crash.Reorder_unsynced !seed) ~bs:1
        in
        if consider (Fmt.str "Reorder(bs=1,seed=%d)" !seed) r then quiet := 0
        else incr quiet;
        incr seed
      done;
      if !quiet < !quiet_seeds then st.unsaturated <- st.unsaturated + 1

let rec enum ~init ~depth ~alphabet prefix f =
  let ops = List.rev prefix in
  f ops;
  if depth > 0 then begin
    let t = tr_init init in
    List.iter (tr_apply t) ops;
    List.iter
      (fun mk ->
        let op = mk (List.length prefix) in
        if valid t op then
          enum ~init ~depth:(depth - 1) ~alphabet (op :: prefix) f)
      alphabet
  end

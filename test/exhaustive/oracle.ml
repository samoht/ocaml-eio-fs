(* Invariants over the live history of a workload, from chaos.mli and ext4:
   (a) synced bytes stay; (b) a name a barrier covered is there; (c) a
   rename a barrier covered loses neither name; (d) no inode twice; (e)
   every entry and byte is one the live tree had or a write made; (f) a
   removal a barrier covered is gone; (g) every mode and owner one the node
   had, (g2) and the last one once synced; (h) a link holds a target it had;
   (j) a write a failed sync lost shows only through a block a later change
   dirtied, or under Reorder_unsynced as the block stood at the failure. *)
open Core

type ev =
  | Fsync_ev of int64 * bool (* inode, success *)
  | Barrier of int64 (* directory inode *)
  | Meta
  | Data of int64 * int * int (* inode, touched [lo, hi) *)
  | Ren of int64 * string * int64 * string * int64 * int64 option
    (* P1 n1 P2 n2 X Y *)
  | Nothing

type hist = {
  root : int64;
  snaps : snap array; (* 0 = initial; i = after op i *)
  evs : ev list array; (* index i = op i (1-based), 0 unused *)
  blocks : (int64, Refm.t) Hashtbl.t;
      (* each file's data under Reorder_unsynced, by the block-level
         reference *)
}

let lookup s p = List.assoc_opt p s.paths

let parent_name p =
  match String.rindex_opt p '/' with
  | None -> ("", p)
  | Some i -> (String.sub p 0 i, String.sub p (i + 1) (String.length p - i - 1))

let ino_of root s p = if p = "" || p = "." then Some root else lookup s p

(* resolve following symlinks in the last component, relative links only *)
let rec resolve root s p depth =
  if depth > 8 then None
  else
    match ino_of root s p with
    | None -> None
    | Some i -> (
        match IM.find_opt i s.nodes with
        | Some { kind = L t; _ } ->
            let par, _ = parent_name p in
            let rec norm base t =
              match String.index_opt t '/' with
              | _ when String.length t >= 3 && String.sub t 0 3 = "../" ->
                  let b, _ = parent_name base in
                  norm b (String.sub t 3 (String.length t - 3))
              | _ -> if base = "" then t else base ^ "/" ^ t
            in
            resolve root s (norm par t) (depth + 1)
        | _ -> Some i)

let flen = function F s -> String.length s | _ -> 0

(* Build events from the op and the snapshots around it. *)
let events root (snaps : snap array) ops (hino : int64 option ref)
    (dino : int64 option ref) (dfino : int64 option ref) =
  let n = List.length ops in
  let evs = Array.make (n + 1) [] in
  List.iteri
    (fun j op ->
      let i = j + 1 in
      let s0 = snaps.(i - 1) and s1 = snaps.(i) in
      let len_in s ino =
        match IM.find_opt ino s.nodes with Some nd -> flen nd.kind | None -> 0
      in
      let data ino touched =
        evs.(i) <- Data (ino, fst touched, snd touched) :: evs.(i)
      in
      let fres p = resolve root s1 p 0 in
      match op with
      | Write (_, p, off, len) -> (
          match fres p with
          | Some ino ->
              let ol = len_in s0 ino in
              data ino (min off ol, off + len)
          | None -> ())
      | Write_h (off, len) -> (
          match !hino with
          | Some ino when IM.mem ino s0.nodes ->
              let ol = len_in s0 ino in
              data ino (min off ol, off + len)
          | Some ino -> data ino (off, off + len)
          | _ -> ())
      | Append (p, len) -> (
          match fres p with
          | Some ino ->
              let ol = len_in s0 ino in
              data ino (ol, ol + len)
          | None -> ())
      | Trunc (_, p, k) -> (
          match fres p with
          | Some ino ->
              let ol = len_in s0 ino in
              data ino (min k ol, max k ol)
          | None -> ())
      | Fsync p -> (
          match fres p with
          | Some ino -> evs.(i) <- [ Fsync_ev (ino, true) ]
          | None -> ())
      | Fsync_u p -> (
          match fres p with
          | Some ino -> evs.(i) <- [ Fsync_ev (ino, true) ]
          | None -> ())
      | Fsync_fail p | Fsync_fail_u p -> (
          match fres p with
          | Some ino -> evs.(i) <- [ Fsync_ev (ino, false) ]
          | None -> ())
      | Sync_h -> (
          match !hino with
          | Some ino -> evs.(i) <- [ Fsync_ev (ino, true) ]
          | None -> ())
      | Sync_dir p -> (
          match resolve root s0 p 0 with
          | Some ino -> evs.(i) <- [ Barrier ino ]
          | None -> ())
      | Sync_dh -> (
          match !dino with Some ino -> evs.(i) <- [ Barrier ino ] | None -> ())
      | Sync_in_d -> (
          match !dfino with
          | Some ino -> evs.(i) <- [ Barrier ino ]
          | None -> ())
      | Sync_meta -> evs.(i) <- [ Meta ]
      | Fake_barrier p -> (
          match resolve root s0 p 0 with
          | Some ino -> evs.(i) <- [ Barrier ino ]
          | None -> ())
      | Fake_fsync p -> (
          match fres p with
          | Some ino -> evs.(i) <- [ Fsync_ev (ino, true) ]
          | None -> ())
      | Open_h p -> hino := fres p
      | Open_dh p -> dino := resolve root s0 p 0
      | Open_in_d p -> dfino := resolve root s0 p 0
      | Rename (a, b) -> (
          let pa, na = parent_name a and pb, nb = parent_name b in
          match (ino_of root s0 pa, ino_of root s0 pb, lookup s0 a) with
          | Some p1, Some p2, Some x ->
              evs.(i) <- [ Ren (p1, na, p2, nb, x, lookup s0 b) ]
          | _ -> ())
      | _ -> ())
    ops;
  evs

(* Each file's data under Reorder_unsynced, by the block-level reference: its
   writes (op [i] writes letter [i]), truncations and syncs. A file not in
   the initial snapshot starts empty. *)
let blocks root (snaps : snap array) ops =
  let refs = Hashtbl.create 4 in
  let get ino =
    match Hashtbl.find_opt refs ino with
    | Some r -> r
    | None ->
        let init =
          match IM.find_opt ino snaps.(0).nodes with
          | Some { kind = F c; _ } -> c
          | _ -> ""
        in
        let r = Refm.v init in
        Hashtbl.replace refs ino r;
        r
  in
  let hino = ref None in
  List.iteri
    (fun j op ->
      let i = j + 1 in
      let s1 = snaps.(i) in
      let data len = String.make len (Char.chr (Char.code 'a' + i - 1)) in
      let fres p = resolve root s1 p 0 in
      let with_file p f = match fres p with Some ino -> f ino | None -> () in
      match op with
      | Write (_, p, off, len) ->
          with_file p (fun ino -> Refm.write (get ino) off (data len))
      | Append (p, len) ->
          with_file p (fun ino ->
              let r = get ino in
              Refm.write r r.Refm.clen (data len))
      | Write_h (off, len) -> (
          match !hino with
          | Some ino -> Refm.write (get ino) off (data len)
          | None -> ())
      | Trunc (_, p, k) -> with_file p (fun ino -> Refm.resize (get ino) k)
      | Fsync p | Fsync_u p | Fake_fsync p ->
          with_file p (fun ino -> Refm.sync_ok (get ino))
      | Fsync_fail p | Fsync_fail_u p ->
          with_file p (fun ino -> Refm.sync_fail ~reorder:true (get ino))
      | Sync_h -> (
          match !hino with Some ino -> Refm.sync_ok (get ino) | None -> ())
      | Open_h p -> hino := fres p
      | _ -> ())
    ops;
  refs

(* ---------- guarantees ---------- *)

let mem t (s : snap) = TS.mem t s.triples

let est h tau upto =
  (* last time tau was established, 0 if continuously present since 0 *)
  let rec go t =
    if t = 0 then 0
    else if mem tau h.snaps.(t) && not (mem tau h.snaps.(t - 1)) then t
    else go (t - 1)
  in
  go upto

let barrier_on h p lo hi =
  (* barrier on directory inode p at time j, lo < j <= hi *)
  let r = ref false in
  for j = lo + 1 to hi do
    List.iter
      (function
        | Barrier q when q = p -> r := true | Meta -> r := true | _ -> ())
      h.evs.(j)
  done;
  !r

let rec guaranteed h ((p, _, _) as tau) upto depth =
  depth < 10
  && mem tau h.snaps.(upto)
  && (let c = est h tau upto in
      (c = 0 && mem tau h.snaps.(0)) || barrier_on h p c upto)
  && reach h p upto (depth + 1)

and reach h p upto depth =
  p = h.root
  || TS.exists
       (fun ((_, _, x) as t') -> x = p && guaranteed h t' upto depth)
       h.snaps.(upto).triples

(* ---------- checks ---------- *)

type viol = { inv : string; detail : string }

let img_triples root (img : image) =
  let m = Hashtbl.create 16 in
  List.iter (fun (p, ino, _, _, _, _) -> Hashtbl.replace m p ino) img;
  List.filter_map
    (fun (p, ino, _, _, _, _) ->
      let par, name = parent_name p in
      let pino = if par = "" then Some root else Hashtbl.find_opt m par in
      Option.map (fun pi -> (pi, name, ino)) pino)
    img

let check ?(reorder = false) h (img : image) =
  let vs = ref [] in
  let add inv detail = vs := { inv; detail } :: !vs in
  let n = Array.length h.snaps - 1 in
  let fin = h.snaps.(n) in
  let itr = img_triples h.root img in
  let its = List.fold_left (fun s t -> TS.add t s) TS.empty itr in
  let inodes = List.map (fun (_, i, _, _, _, _) -> i) img in
  let img_node ino = List.find_opt (fun (_, i, _, _, _, _) -> i = ino) img in
  let at p n =
    TS.fold
      (fun (q, m, x) acc -> if q = p && m = n then Some x else acc)
      its None
  in
  (* (d) *)
  let sorted = List.sort compare inodes in
  let rec dup = function
    | a :: (b :: _ as r) -> if a = b then Some a else dup r
    | _ -> None
  in
  (match dup sorted with
  | Some i -> add "d:dup-inode" (Fmt.str "#%Ld" i)
  | None -> ());
  (* (e) names *)
  let ever =
    Array.fold_left (fun acc s -> TS.union acc s.triples) TS.empty h.snaps
  in
  List.iter
    (fun ((p, nm, x) as t) ->
      if not (TS.mem t ever) then
        add "e:never-live-entry" (Fmt.str "(#%Ld,%s,#%Ld)" p nm x))
    itr;
  (* per-node history *)
  let hist_nodes x =
    Array.to_list h.snaps |> List.filter_map (fun s -> IM.find_opt x s.nodes)
  in
  List.iter
    (fun (path, x, c, perm, uid, gid) ->
      let hn = hist_nodes x in
      (match c with
      | `File s ->
          let fs =
            List.filter_map
              (function { kind = F s; _ } -> Some s | _ -> None)
              hn
          in
          if fs = [] then add "e:kind" path
          else begin
            let hdata = ref [] in
            Array.iteri
              (fun k evl ->
                List.iter
                  (function
                    | Data (y, lo, hi) when y = x ->
                        hdata := (k, lo, hi) :: !hdata
                    | _ -> ())
                  evl)
              h.evs;
            let maxl =
              List.fold_left (fun a s -> max a (String.length s)) 0 fs
            in
            let maxl =
              List.fold_left (fun a (_, _, hi) -> max a hi) maxl !hdata
            in
            if String.length s > maxl then
              add "e:too-long"
                (Fmt.str "%s len %d > %d" path (String.length s) maxl);
            String.iteri
              (fun i ch ->
                if
                  ch <> '\000'
                  && (not
                        (List.exists
                           (fun f -> i < String.length f && f.[i] = ch)
                           fs))
                  && not
                       (List.exists
                          (fun (k, lo, hi) ->
                            lo <= i && i < hi
                            && ch = Char.chr (Char.code 'a' + k - 1))
                          !hdata)
                then add "e:never-written-byte" (Fmt.str "%s[%d]=%C" path i ch))
              s
          end
      | `Link t ->
          if
            not
              (List.exists
                 (function { kind = L t'; _ } -> t = t' | _ -> false)
                 hn)
          then add "h:link-target" (Fmt.str "%s->%s" path t)
      | `Dir ->
          if not (List.exists (fun nd -> nd.kind = D) hn) then add "e:kind" path);
      (* (g) *)
      if
        not
          (List.exists
             (fun nd -> nd.perm = perm && nd.uid = uid && nd.gid = gid)
             hn)
      then begin
        let pm = List.exists (fun nd -> nd.perm = perm) hn
        and ow = List.exists (fun nd -> nd.uid = uid && nd.gid = gid) hn in
        add
          (if pm && ow then "g:mode-owner-combination" else "g:mode-or-owner")
          (Fmt.str "%s %o uid %Ld" path perm uid)
      end)
    img;
  (* (b) *)
  TS.iter
    (fun ((p, nm, x) as t) ->
      if guaranteed h t n 0 && not (TS.mem t its) then
        add "b:barriered-name-missing" (Fmt.str "(#%Ld,%s,#%Ld)" p nm x))
    fin.triples;
  (* (f) *)
  TS.iter
    (fun ((p, nm, x) as t) ->
      if not (mem t fin) then begin
        let b = ref 0 in
        for i = 1 to n do
          if mem t h.snaps.(i - 1) && not (mem t h.snaps.(i)) then b := i
        done;
        if !b > 0 && barrier_on h p !b n && TS.mem t its then
          add "f:removed-name-present" (Fmt.str "(#%Ld,%s,#%Ld)" p nm x);
        let alive = TS.exists (fun (_, _, y) -> y = x) fin.triples in
        let last_loss = ref 0 in
        for i = 1 to n do
          let had s = TS.exists (fun (_, _, y) -> y = x) s.triples in
          if had h.snaps.(i - 1) && not (had h.snaps.(i)) then last_loss := i
        done;
        if
          (not alive) && !last_loss = !b && !b > 0 && barrier_on h p !b n
          && List.mem x inodes
          && TS.mem t its |> not
        then add "f':removed-inode-present" (Fmt.str "#%Ld" x)
      end)
    ever;
  (* (c) *)
  Array.iteri
    (fun r evl ->
      List.iter
        (function
          | Ren (p1, n1, p2, n2, x, y) ->
              let td = (p2, n2, x) and ts = (p1, n1, x) in
              if
                mem td fin
                && est h td n = r
                && guaranteed h ts (r - 1) 0
                && reach h p1 n 0 && reach h p2 n 0
              then begin
                let atd = at p2 n2 and ats = at p1 n1 in
                if atd <> Some x && ats <> Some x then
                  add "c:rename-lost-file" (Fmt.str "#%Ld %s->%s" x n1 n2);
                match y with
                | Some y when guaranteed h (p2, n2, y) (r - 1) 0 ->
                    if atd <> Some x && atd <> Some y then
                      add "c:rename-dest-empty" (Fmt.str "%s" n2)
                | _ -> ()
              end
          | _ -> ())
        evl)
    h.evs;
  (* (a)/(i) data *)
  IM.iter
    (fun x nd ->
      match (nd.kind, img_node x) with
      | F live, Some (path, _, `File s, _, _, _) ->
          let datas = ref [] and syncs = ref [] in
          Array.iteri
            (fun i evl ->
              List.iter
                (function
                  | Data (y, lo, hi) when y = x ->
                      datas := (i, lo, hi) :: !datas
                  | Fsync_ev (y, ok) when y = x -> syncs := (i, ok) :: !syncs
                  | _ -> ())
                evl)
            h.evs;
          let syncs = List.rev !syncs in
          let covered k =
            (* a successful sync after k with no failed sync in between *)
            let rec go = function
              | [] -> false
              | (j, ok) :: r -> if j <= k then go r else ok
            in
            go syncs
          in
          String.iteri
            (fun i ch ->
              let last =
                List.fold_left
                  (fun a (k, lo, hi) ->
                    if lo <= i && i < hi && k > a then k else a)
                  0 !datas
              in
              let ok_cov = if last = 0 then true else covered last in
              if ok_cov && (i >= String.length s || s.[i] <> ch) then
                add
                  (if List.exists (fun (_, ok) -> not ok) syncs then
                     "i:after-failed-sync"
                   else "a:synced-byte")
                  (Fmt.str "%s[%d] want %C got %s" path i ch
                     (if i >= String.length s then "EOF" else Fmt.str "%C" s.[i])))
            live
      | _ -> ())
    fin.nodes;
  (* (j) a write a failed sync lost shows only through a block a later change dirtied *)
  List.iter
    (fun (path, x, c, _, _, _) ->
      match c with
      | `File s ->
          let datas = ref [] and syncs = ref [] in
          Array.iteri
            (fun i evl ->
              List.iter
                (function
                  | Data (y, lo, hi) when y = x ->
                      datas := (i, lo, hi) :: !datas
                  | Fsync_ev (y, ok) when y = x -> syncs := (i, ok) :: !syncs
                  | _ -> ())
                evl)
            h.evs;
          let syncs = List.rev !syncs in
          let lost_at k =
            let rec go = function
              | [] -> None
              | (j, ok) :: r ->
                  if j <= k then go r else if ok then None else Some j
            in
            go syncs
          in
          String.iteri
            (fun i ch ->
              List.iter
                (fun (k, lo, hi) ->
                  let letter = Char.chr (Char.code 'a' + k - 1) in
                  if ch = letter && lo <= i && i < hi then
                    match lost_at k with
                    | Some j ->
                        let b0 = i / 2 * 2 in
                        let redirtied =
                          List.exists
                            (fun (k', lo', hi') ->
                              k' > j && lo' < b0 + 2 && hi' > b0)
                            !datas
                        in
                        (* Under Reorder_unsynced the block-level reference
                           judges the whole file, below. *)
                        if not (redirtied || reorder) then
                          add "j:lost-write-visible"
                            (Fmt.str "%s[%d]=%C from op %d lost at op %d" path i
                               ch k j)
                    | None -> ())
                !datas)
            s
      | _ -> ())
    img;
  (* (j) under Reorder_unsynced: each block one the block-level reference
     allows, a partly failed writeback or an evicted page included *)
  if reorder then
    List.iter
      (fun (path, x, c, _, _, _) ->
        match (c, Hashtbl.find_opt h.blocks x) with
        | `File s, Some r -> (
            match Refm.check ~policy:Refm.Reorder r s with
            | None -> ()
            | Some why -> add "j:blocks" (Fmt.str "%s = %S: %s" path s why))
        | _ -> ())
      img;
  (* (g2) mode durability *)
  IM.iter
    (fun x nd ->
      match img_node x with
      | Some (path, _, _, perm, uid, gid) ->
          let m = ref 0 in
          for i = 1 to n do
            match
              ( IM.find_opt x h.snaps.(i - 1).nodes,
                IM.find_opt x h.snaps.(i).nodes )
            with
            | Some a, Some b
              when (a.perm, a.uid, a.gid) <> (b.perm, b.uid, b.gid) ->
                m := i
            | _ -> ()
          done;
          if !m > 0 then begin
            let cov = ref false in
            for j = !m + 1 to n do
              List.iter
                (function
                  | Fsync_ev (y, true) when y = x -> cov := true
                  | Meta -> cov := true
                  | Barrier y when y = x -> cov := true
                  | _ -> ())
                h.evs.(j)
            done;
            if !cov && (perm, uid, gid) <> (nd.perm, nd.uid, nd.gid) then
              add "g2:synced-mode-lost"
                (Fmt.str "%s %o/%Ld want %o/%Ld" path perm uid nd.perm nd.uid)
          end
      | None -> ())
    fin.nodes;
  List.rev !vs

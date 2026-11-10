(* A differential of Chaos.Crash against the reference in ref.ml, under
   every policy: the image Drop_unsynced leaves, every Survive choice of
   uncommitted entries and data changes, compared exactly when the unsynced
   lists line up, and Reorder_unsynced seeds at block sizes 1, 7 and 4096,
   each image a member of what the reference allows. Data is compared, not
   only names and modes. *)

open Ref
module Crash = Eio_fs.Chaos.Private.Crash
module Chaos = Eio_fs.Chaos.Private

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

type policy =
  | P_drop
  | P_reorder of int * int
  (* bs, seed *)
  | P_survive of int * IS.t
(* k, data idx *)

let pp_policy ppf = function
  | P_drop -> Fmt.string ppf "Drop"
  | P_reorder (bs, s) -> Fmt.pf ppf "Reorder(bs=%d,seed=%d)" bs s
  | P_survive (k, m) ->
      Fmt.pf ppf "Survive(k=%d,data=%a)" k
        Fmt.(list ~sep:comma int)
        (IS.elements m)

let eio_user (u : user) : Eio_fs.user =
  { uid = u.uid; gid = u.gid; groups = u.groups; umask = 0o022 }

let exec_op ~sw ~ctl ~cw ~handles ~dhandles op =
  let p x = cw / x in
  match op with
  | Mkdir x -> Eio.Path.mkdir ~perm:0o755 (p x)
  | Rmdir x -> Eio.Path.rmdir (p x)
  | Creat x -> Eio.Path.with_open_out ~create:(`Exclusive 0o644) (p x) ignore
  | Write (x, off, d) ->
      Eio.Path.with_open_out ~create:`Never (p x) (fun f ->
          Eio.File.pwrite_all f ~file_offset:(i63 off) [ Cstruct.of_string d ])
  | Append (x, d) ->
      Eio.Path.with_open_out ~append:true ~create:`Never (p x) (fun f ->
          Eio.Flow.copy_string d f)
  | Truncate (x, n) ->
      Eio.Path.with_open_out ~create:`Never (p x) (fun f ->
          Eio.File.truncate f (Optint.Int63.of_int n))
  | Rename (a, b) -> Eio.Path.rename (p a) (p b)
  | Unlink x -> Eio.Path.unlink (p x)
  | Symlink (tg, x) -> Eio.Path.symlink ~link_to:tg (p x)
  | Chmod (x, perm) -> Eio.Path.chmod ~follow:true ~perm (p x)
  | Chown (x, u, g) ->
      Eio.Path.chown ~follow:true ~uid:(Int64.of_int u) ~gid:(Int64.of_int g)
        (p x)
  | Fsync x -> Eio.Path.with_open_out ~create:`Never (p x) Eio.File.sync
  | Fsync_ro x ->
      Eio.Path.with_open_in (p x) (fun f ->
          Eio_fs_sync.Barrier.sync_file Eio_fs_sync.Barrier.portable f)
  | Fsync_dir x -> Eio_fs_sync.sync_dir (p x)
  | Fsync_fail x ->
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
      Eio.Path.with_open_out ~create:`Never (xw / x) Eio.File.sync
  | Sync_meta -> Crash.sync_metadata ctl
  | Hopen (h, x) ->
      Hashtbl.replace handles h (Eio.Path.open_out ~sw ~create:`Never (p x))
  | Hwrite (h, off, d) ->
      Eio.File.pwrite_all (Hashtbl.find handles h) ~file_offset:(i63 off)
        [ Cstruct.of_string d ]
  | Hsync h -> Eio.File.sync (Hashtbl.find handles h)
  | Dopen (h, x) -> Hashtbl.replace dhandles h (Eio.Path.open_dir ~sw (p x))
  | Dsync h -> Eio_fs_sync.sync_dir (Hashtbl.find dhandles h)

let read_image base : image =
  let acc = ref [] in
  let rec go rel =
    let path = if rel = "" then base else base / rel in
    let st = Eio.Path.stat ~follow:false path in
    let m = st.perm and u = Int64.to_int st.uid and g = Int64.to_int st.gid in
    match st.kind with
    | `Directory ->
        acc := (rel, D (m, u, g)) :: !acc;
        List.iter
          (fun n -> go (if rel = "" then n else rel ^ "/" ^ n))
          (List.sort compare (Eio.Path.read_dir path))
    | `Symbolic_link -> acc := (rel, L (Eio.Path.read_link path)) :: !acc
    | `Regular_file -> acc := (rel, F (m, u, g, Eio.Path.load path)) :: !acc
    | _ -> failwith "odd kind"
  in
  go "";
  List.sort compare !acc

let setup base init =
  List.iter
    (function
      | I_dir p -> Eio.Path.mkdir ~perm:0o755 (base / p)
      | I_file (p, c) -> Eio.Path.save ~create:(`Exclusive 0o644) (base / p) c
      | I_link (p, tg) -> Eio.Path.symlink ~link_to:tg (base / p))
    init

type run = {
  results : bool list; (* per op: succeeded *)
  unsynced : Crash.unsynced list;
  image : image;
}

let is_meta (u : Crash.unsynced) =
  match u.kind with Data | Truncate -> false | _ -> true

(* Run [ops] under [policy]. [survive] receives the unsynced list mapping. *)
let run_eio ?(probe = []) ~user ~init ~ops ~policy () =
  Eio_mock.Backend.run @@ fun () ->
  let base = Eio_fs.Fs.v ~user:(eio_user user) () in
  setup base init;
  let pol, bs =
    match policy with
    | P_drop -> (Crash.Drop_unsynced, 4096)
    | P_reorder (bs, seed) -> (Crash.Reorder_unsynced seed, bs)
    | P_survive (k, data) ->
        let tbl = Hashtbl.create 8 in
        let mi = ref 0 and di = ref 0 in
        List.iter
          (fun (u : Crash.unsynced) ->
            let b =
              if is_meta u then (
                let i = !mi in
                incr mi;
                i < k)
              else
                let i = !di in
                incr di;
                IS.mem i data
            in
            Hashtbl.replace tbl u.seq b)
          probe;
        ( Crash.Survive
            (fun u ->
              match Hashtbl.find_opt tbl u.seq with
              | Some b -> b
              | None -> failwith "unknown seq"),
          4096 )
  in
  let ctl = Crash.v ~policy:pol ~block_size:bs () in
  let cw = Crash.wrap ctl base in
  let handles = Hashtbl.create 4 and dhandles = Hashtbl.create 4 in
  let results =
    Eio.Switch.run @@ fun sw ->
    let r =
      List.map
        (fun op ->
          match exec_op ~sw ~ctl ~cw ~handles ~dhandles op with
          | () -> true
          | exception (Eio.Io _ | Not_found | Invalid_argument _) -> false)
        ops
    in
    r
  in
  let unsynced = Crash.unsynced ctl in
  let post = Crash.post_crash ctl in
  { results; unsynced; image = read_image post }

(* Reference run: per-op success and the model. *)
let run_ref ?bs ~user ~init ~ops () =
  let t = Ref.v ?bs ~user init in
  let results =
    List.map
      (fun op ->
        match run_op t op with () -> true | exception Fail _ -> false)
      ops
  in
  (t, results)

(* {1 Generator} *)

let live_paths (t : Ref.t) =
  let rec go prefix d depth acc =
    List.fold_left
      (fun acc (name, c) ->
        let p = if prefix = "" then name else prefix ^ "/" ^ name in
        let acc = (p, c, depth) :: acc in
        match (node t c).k with Dir _ -> go p c (depth + 1) acc | _ -> acc)
      acc (ents t d)
  in
  go "" 0 1 []

let inits =
  [|
    [];
    [ I_dir "a" ];
    [ I_dir "a"; I_dir "b" ];
    [ I_dir "a"; I_file ("a/f", "ABCDEFGHIJ") ];
    [
      I_dir "a";
      I_dir "b";
      I_file ("a/f", "ABCDEFGHIJ");
      I_file ("b/g", "KLMNOPQ");
    ];
    [ I_dir "a"; I_dir "a/b"; I_file ("a/b/f", "ABCDEFGHIJ") ];
    [ I_file ("f", "ABCDEFGHIJKLMNO"); I_dir "a"; I_dir "a/b"; I_dir "a/b/c" ];
    [
      I_dir "a";
      I_dir "b";
      I_dir "b/c";
      I_file ("b/c/g", "KLM");
      I_link ("a/l", "../b/c/g");
    ];
  |]

let gen rs ~(user : user) =
  let int n = Random.State.int rs n in
  let pick a = a.(int (Array.length a)) in
  let pickl l = List.nth l (int (List.length l)) in
  let init = pick inits in
  let t = Ref.v ~user init in
  let n = 3 + int 5 in
  let nh = ref 0 and nd = ref 0 in
  let wchar = ref 0 in
  let data () =
    incr wchar;
    String.make (1 + int 6) (Char.chr (Char.code 'a' + (!wchar mod 26)))
  in
  let ops = ref [] in
  for _ = 1 to n do
    let lp = live_paths t in
    let dirs =
      ""
      :: List.filter_map
           (fun (p, c, d) ->
             match (node t c).k with Dir _ when d < 3 -> Some p | _ -> None)
           lp
    in
    let alld =
      ""
      :: List.filter_map
           (fun (p, c, _) ->
             match (node t c).k with Dir _ -> Some p | _ -> None)
           lp
    in
    let files =
      List.filter_map
        (fun (p, c, _) ->
          match (node t c).k with File _ -> Some p | _ -> None)
        lp
    in
    let links =
      List.filter_map
        (fun (p, c, _) ->
          match (node t c).k with Link _ -> Some p | _ -> None)
        lp
    in
    let nonroot = List.map (fun (p, _, _) -> p) lp in
    let j d x = if d = "" then x else d ^ "/" ^ x in
    let file () =
      if files = [] || int 10 = 0 then j (pickl alld) (pick [| "f"; "g" |])
      else pickl (files @ if int 3 = 0 then links else [])
    in
    let any () = if nonroot = [] then "a" else pickl nonroot in
    let newname () =
      j (pickl dirs) (pick [| "a"; "b"; "c"; "d"; "f"; "g"; "h" |])
    in
    let op =
      match int 22 with
      | 0 | 1 -> Mkdir (j (pickl dirs) (pick [| "a"; "b"; "c"; "d" |]))
      | 2 ->
          Rmdir (pickl (if List.length alld > 1 then List.tl alld else [ "a" ]))
      | 3 | 4 -> Creat (j (pickl alld) (pick [| "f"; "g"; "h" |]))
      | 5 | 6 -> Write (file (), int 10, data ())
      | 7 -> Append (file (), data ())
      | 8 -> Truncate (file (), int 12)
      | 9 | 10 | 11 -> Rename (any (), if int 3 = 0 then any () else newname ())
      | 12 -> Unlink (any ())
      | 13 -> Symlink (pick [| "f"; "g"; "../f"; "a/f" |], j (pickl alld) "l")
      | 14 ->
          let p = any () in
          let isdir = try is_dir t (resolve t p) with _ -> false in
          Chmod
            ( p,
              if isdir then pick [| 0o755; 0o700; 0o2775; 0o1777 |]
              else pick [| 0o644; 0o600; 0o4755; 0o2775; 0o755; 0o700 |] )
      | 15 ->
          if user.is_root then
            Chown (any (), pick [| 0; 1000 |], pick [| 0; 1000 |])
          else Chown (any (), 1000, pick [| 1000; 1001 |])
      | 16 -> pick [| Fsync (file ()); Fsync_ro (file ()) |]
      | 17 -> Fsync_dir (pickl alld)
      | 18 -> if int 3 = 0 then Sync_meta else Fsync_dir (pickl alld)
      | 19 -> Fsync_fail (file ())
      | 20 ->
          if !nh > 0 && int 2 = 0 then
            pick [| Hwrite (int !nh, int 10, data ()); Hsync (int !nh) |]
          else
            let h = !nh in
            incr nh;
            Hopen (h, file ())
      | _ ->
          if !nd > 0 && int 2 = 0 then Dsync (int !nd)
          else
            let h = !nd in
            incr nd;
            Dopen (h, pickl alld)
    in
    (match run_op t op with () -> () | exception Fail _ -> ());
    ops := op :: !ops
  done;
  (init, List.rev !ops)

(* {1 Differential} *)

type finding = { cls : string; policy : string; detail : string }

let classify t (obs : image) ~data =
  match matching_ks t obs with
  | [] -> "skeleton"
  | ks ->
      let choices = meta_choices t in
      let bad =
        List.concat_map
          (fun k ->
            let _, s, _, vis = List.nth choices k in
            List.filter_map
              (fun (p, d) ->
                match d with
                | F (_, _, _, c) when not (data s (List.assoc p vis) c) ->
                    Some p
                | _ -> None)
              obs)
          ks
      in
      if bad = [] then "?" else "content"

let subsets_cap rs n cap =
  let all = 1 lsl n in
  if all <= cap then List.init all (fun m -> m)
  else List.init cap (fun _ -> Random.State.int rs all)

let iset_of_mask m =
  IS.of_list
    (List.filter (fun i -> m land (1 lsl i) <> 0) (List.init 20 Fun.id))

let check ?(seeds = 40) ?(survive_cap = 64) ~user ~init ~ops () =
  let findings = ref [] in
  let add cls policy detail =
    findings := { cls; policy; detail } :: !findings
  in
  let t, rres = run_ref ~user ~init ~ops () in
  let probe = run_eio ~user ~init ~ops ~policy:P_drop () in
  if probe.results <> rres then begin
    add "live-mismatch" "-"
      (Fmt.str "ref %a eio %a"
         Fmt.(list ~sep:sp bool)
         rres
         Fmt.(list ~sep:sp bool)
         probe.results);
    (!findings, 0)
  end
  else begin
    let um = List.filter is_meta probe.unsynced
    and ud = List.filter (fun u -> not (is_meta u)) probe.unsynced in
    let mu = uncommitted t and md = pending_data t in
    let aligned =
      List.length um = List.length mu && List.length ud = List.length md
    in
    if not aligned then
      add "unsynced-mismatch" "-"
        (Fmt.str
           "eio meta=%d data=%d; ref meta=%d data=%d@.eio: %a@.ref meta: %a"
           (List.length um) (List.length ud) (List.length mu) (List.length md)
           Fmt.(list ~sep:(any "; ") Crash.pp_unsynced)
           probe.unsynced
           Fmt.(list ~sep:(any "; ") string)
           (List.map (fun e -> e.tag) mu));
    (* Drop *)
    let exp = survive_state t ~k:0 ~subset:IS.empty in
    if probe.image <> exp then
      add
        ("drop:"
        ^ classify t probe.image ~data:(fun _ id c ->
            c = content_survive t id IS.empty))
        "Drop"
        (Fmt.str "eio:@.%a@.ref:@.%a" pp_image probe.image pp_image exp);
    let observed = ref [ probe.image ] in
    let rs = Random.State.make [| 7 |] in
    (* Survive, pointwise when aligned *)
    if aligned then begin
      let dseqs = Array.of_list (List.map (fun (s, _, _) -> s) md) in
      for k = 0 to List.length mu do
        List.iter
          (fun m ->
            let im = iset_of_mask m in
            let r =
              run_eio ~probe:probe.unsynced ~user ~init ~ops
                ~policy:(P_survive (k, im))
                ()
            in
            observed := r.image :: !observed;
            let subset = IS.map (fun i -> dseqs.(i)) im in
            let exp = survive_state t ~k ~subset in
            if r.image <> exp then
              add
                ("survive:"
                ^ classify t r.image ~data:(fun s id c ->
                    survive_member ?cut:(clear_cut t s id) t id c))
                (Fmt.str "%a" pp_policy (P_survive (k, im)))
                (Fmt.str "eio:@.%a@.ref:@.%a" pp_image r.image pp_image exp))
          (subsets_cap rs (List.length md) survive_cap)
      done
    end;
    if not aligned then begin
      for k = 0 to List.length um do
        List.iter
          (fun m ->
            let im = iset_of_mask m in
            let r =
              run_eio ~probe:probe.unsynced ~user ~init ~ops
                ~policy:(P_survive (k, im))
                ()
            in
            observed := r.image :: !observed;
            let data s id c = survive_member ?cut:(clear_cut t s id) t id c in
            if not (member t ~data r.image) then
              add
                ("survive-u:" ^ classify t r.image ~data)
                (Fmt.str "%a" pp_policy (P_survive (k, im)))
                (Fmt.str "eio:@.%a" pp_image r.image))
          (subsets_cap rs (List.length ud) survive_cap)
      done
    end;
    (* Reorder *)
    List.iter
      (fun bs ->
        let t, _ = run_ref ~bs ~user ~init ~ops () in
        let choices = meta_choices t in
        let seen_len = Hashtbl.create 8 and seen_blk = Hashtbl.create 8 in
        for seed = 0 to seeds - 1 do
          let r = run_eio ~user ~init ~ops ~policy:(P_reorder (bs, seed)) () in
          observed := r.image :: !observed;
          let data s id c = reorder_member ?cut:(clear_cut t s id) t ~bs id c in
          if not (member t ~data r.image) then
            add
              ("reorder:" ^ classify t r.image ~data)
              (Fmt.str "%a" pp_policy (P_reorder (bs, seed)))
              (Fmt.str "eio:@.%a" pp_image r.image)
          else
            (* record data coverage under the first matching k *)
            match matching_ks t r.image with
            | k :: _ ->
                let _, _, _, vis = List.nth choices k in
                List.iter
                  (fun (p, d) ->
                    match d with
                    | F (_, _, _, c) ->
                        let id = List.assoc p vis in
                        Hashtbl.replace seen_len (id, String.length c) ();
                        let len = String.length c in
                        for i = 0 to Stdlib.((len + bs - 1) / bs) - 1 do
                          let lo = i * bs in
                          Hashtbl.replace seen_blk
                            (id, i, String.sub c lo (min bs (len - lo)))
                            ()
                        done
                    | _ -> ())
                  r.image
            | [] -> ()
        done;
        (* the written-back (live) version of each block of each file the
           full metadata image shows: observed at least once? *)
        let img = meta_image t (survivors t (n_uncommitted t)) in
        List.iter
          (fun (p, id) ->
            match (node t id).k with
            | File _ ->
                let live =
                  content_survive t id
                    (IS.of_list
                       (List.map (fun (s, _, _, _) -> s) (fd t id).pend))
                in
                let len = String.length live in
                let miss = ref [] in
                for i = 0 to Stdlib.((len + bs - 1) / bs) - 1 do
                  let lo = i * bs in
                  let want = String.sub live lo (min bs (len - lo)) in
                  let hit =
                    Hashtbl.fold
                      (fun (id', i', sl) () acc ->
                        acc
                        || id' = id && i' = i
                           && String.length sl >= String.length want
                           && String.sub sl 0 (String.length want) = want
                        || id' = id && i' = i
                           && String.length sl < String.length want
                           && String.sub want 0 (String.length sl) = sl
                           && false)
                      seen_blk false
                  in
                  if not hit then miss := i :: !miss
                done;
                if !miss <> [] then
                  add
                    (Fmt.str "live-blk-missing:bs%d" bs)
                    "-"
                    (Fmt.str "%s: written-back %S; blocks never seen: %a" p live
                       Fmt.(list ~sep:comma int)
                       (List.rev !miss))
            | _ -> ())
          (visible t img);
        (* candidates never observed, for inodes observed at all *)
        let ids =
          Hashtbl.fold
            (fun (id, _) () acc -> if List.mem id acc then acc else id :: acc)
            seen_len []
        in
        List.iter
          (fun id ->
            let lens, cands = reorder_cands t ~bs id in
            List.iter
              (fun l ->
                if not (Hashtbl.mem seen_len (id, l)) then
                  add
                    (Fmt.str "missing-len:bs%d" bs)
                    "-"
                    (Fmt.str "inode %d (%s) length %d never seen; lens=%a" id
                       (try path_of t id with _ -> "?")
                       l
                       Fmt.(list ~sep:comma int)
                       lens))
              lens;
            let maxlen = List.fold_left max 0 lens in
            for i = 0 to Stdlib.((maxlen + bs - 1) / bs) - 1 do
              List.iter
                (fun v ->
                  let hit =
                    Hashtbl.fold
                      (fun (id', i', sl) () acc ->
                        acc
                        || id' = id && i' = i
                           && String.length sl > 0
                           && String.sub v 0 (String.length sl) = sl
                           && String.length sl = bs)
                      seen_blk false
                  in
                  let hit_any =
                    Hashtbl.fold
                      (fun (id', i', sl) () acc ->
                        acc
                        || id' = id && i' = i
                           && String.sub v 0 (String.length sl) = sl)
                      seen_blk false
                  in
                  if not hit_any then
                    add
                      (Fmt.str "missing-blk:bs%d" bs)
                      "-"
                      (Fmt.str "inode %d (%s) block %d version %S never seen" id
                         (try path_of t id with _ -> "?")
                         i v)
                  else ignore hit)
                (cands i)
            done)
          ids)
      [ 1; 7; 4096 ];
    (* Coverage: metadata skeletons the reference allows that eio never showed *)
    let obs_sk = List.map obs_skeleton !observed in
    List.iter
      (fun (k, _, img, vis) ->
        let sk = skeleton t img vis in
        if not (List.mem sk obs_sk) then
          add "missing-skeleton" (Fmt.str "k=%d" k)
            (Fmt.str "%a"
               Fmt.(list ~sep:(any "; ") string)
               (List.map
                  (fun (p, d) ->
                    p
                    ^
                    match d with
                    | `D _ -> "/"
                    | `L tg -> "->" ^ tg
                    | `F _ -> "")
                  sk)))
      (meta_choices t);
    (!findings, List.length !observed)
  end

let pp_workload ppf (user, init, ops) =
  Fmt.pf ppf "user=%s init=[%a]@.%a"
    (if user.is_root then "root" else "1000")
    Fmt.(
      list ~sep:(any "; ") (fun ppf -> function
        | I_dir p -> pf ppf "%s/" p
        | I_file (p, c) -> pf ppf "%s=%S" p c
        | I_link (p, t) -> pf ppf "%s->%s" p t))
    init
    Fmt.(list ~sep:(any "@.") (fun ppf o -> pf ppf "  %a" pp_op o))
    ops

let root_user = { uid = 0; gid = 0; groups = []; is_root = true }
let plain_user = { uid = 1000; gid = 1000; groups = [ 1001 ]; is_root = false }

let cases : (string * user * init list * op list) list =
  [
    ( "opened-after-rename",
      root_user,
      [ I_file ("f", "OLD") ],
      [ Rename ("f", "g"); Hopen (0, "g"); Hwrite (0, 0, "NEW"); Hsync 0 ] );
    ( "opened-after-dir-rename",
      root_user,
      [ I_dir "d"; I_file ("d/f", "OLD") ],
      [ Rename ("d", "e"); Hopen (0, "e/f"); Hwrite (0, 0, "NEW"); Hsync 0 ] );
    ( "rename-over-filled-dir",
      root_user,
      [ I_dir "a"; I_dir "b"; I_file ("a/f", "AAAA"); I_file ("b/g", "GGGG") ],
      [ Rename ("b/g", "a/f"); Rename ("a", "b"); Fsync_dir "" ] );
    ( "rename-over-filled-dir2",
      root_user,
      [ I_dir "a"; I_dir "b"; I_file ("b/g", "GGGG") ],
      [ Rename ("b/g", "a/g"); Rename ("a", "b"); Fsync_dir "" ] );
    ( "truncate-sgid",
      plain_user,
      [ I_file ("g", "KLM") ],
      [ Chmod ("g", 0o2775); Sync_meta; Truncate ("g", 5) ] );
    ( "truncate-sgid2",
      plain_user,
      [ I_file ("g", "KLM") ],
      [ Chmod ("g", 0o2775); Truncate ("g", 5); Sync_meta ] );
    ( "write-sgid",
      plain_user,
      [ I_file ("g", "KLM") ],
      [ Chmod ("g", 0o2775); Write ("g", 0, "x"); Sync_meta ] );
    ( "write-suid-then-chmod",
      plain_user,
      [ I_file ("g", "KLM") ],
      [
        Chmod ("g", 0o4755);
        Sync_meta;
        Write ("g", 0, "x");
        Chmod ("g", 0o4755);
        Sync_meta;
        Fsync "g";
      ] );
    ( "chown-fsync-root",
      root_user,
      [],
      [ Creat "g"; Chown ("g", 1000, 1000); Fsync "g" ] );
    ( "chown-fsync-root2",
      root_user,
      [ I_file ("g", "x") ],
      [ Chown ("g", 1000, 1000); Fsync "g" ] );
    ( "write-sgid-dirsync",
      plain_user,
      [ I_file ("g", "KLM") ],
      [ Chmod ("g", 0o2775); Write ("g", 0, "x"); Fsync_dir "" ] );
    ( "unsynced-data-sync-meta",
      root_user,
      [ I_file ("g", "KLM") ],
      [ Write ("g", 0, "x"); Sync_meta ] );
    ( "unsynced-data-rename-replace",
      root_user,
      [ I_file ("f", "FFF"); I_file ("g", "GGG") ],
      [ Write ("g", 0, "x"); Rename ("f", "g"); Sync_meta ] );
    ( "hole-regrow",
      root_user,
      [ I_file ("f", "ABCDEFGHIJ") ],
      [ Truncate ("f", 0); Write ("f", 6, "x") ] );
    ( "hole-regrow2",
      root_user,
      [ I_file ("f", "ABCDEFGHIJ") ],
      [ Truncate ("f", 2); Write ("f", 6, "x") ] );
    ("sparse-creat", root_user, [], [ Creat "g"; Write ("g", 5, "bbb") ]);
    ( "grow-truncate",
      root_user,
      [ I_file ("f", "ABCDEFGHIJ") ],
      [ Truncate ("f", 3); Truncate ("f", 8) ] );
    ( "limbo-basic",
      root_user,
      [ I_dir "a"; I_dir "b"; I_file ("b/x", "XXXX") ],
      [ Mkdir "a/n"; Rename ("b/x", "a/n/x"); Fsync_dir "b" ] );
    ( "limbo-back",
      root_user,
      [ I_dir "a"; I_dir "b"; I_file ("b/x", "XXXX") ],
      [
        Mkdir "a/n";
        Rename ("b/x", "a/n/x");
        Fsync_dir "b";
        Rename ("a/n/x", "b/y");
        Fsync_dir "b";
      ] );
    ( "limbo-dir-child-out",
      root_user,
      [ I_dir "a"; I_dir "b"; I_dir "b/d"; I_file ("b/d/f", "FFFF") ],
      [
        Mkdir "a/n";
        Rename ("b/d", "a/n/d");
        Fsync_dir "b";
        Rename ("a/n/d/f", "b/f");
        Fsync_dir "b";
      ] );
    ( "limbo-data",
      root_user,
      [ I_dir "a"; I_dir "b"; I_file ("b/x", "XXXX") ],
      [
        Hopen (0, "b/x");
        Mkdir "a/n";
        Rename ("b/x", "a/n/x");
        Fsync_dir "b";
        Hwrite (0, 0, "YY");
        Hsync 0;
      ] );
    ( "late-commit-limbo",
      root_user,
      [ I_dir "a"; I_dir "c"; I_file ("c/f", "FFFF") ],
      [ Mkdir "a/d"; Rename ("c/f", "a/d/f"); Fsync_dir "c"; Fsync_dir "a" ] );
    ( "late-commit-order",
      root_user,
      [ I_dir "a"; I_dir "b" ],
      [
        Creat "a/f";
        Rename ("a/f", "b/f");
        Creat "a/f";
        Fsync_dir "b";
        Fsync_dir "a";
      ] );
    ( "ancestors",
      root_user,
      [ I_dir "p"; I_dir "p/a"; I_dir "p/a/b"; I_dir "q" ],
      [ Rename ("p/a/b", "q/b"); Rename ("p/a", "q/b/a"); Fsync_dir "q/b" ] );
    ( "ancestors2",
      root_user,
      [
        I_dir "p"; I_dir "p/a"; I_dir "p/a/b"; I_dir "q"; I_file ("p/a/b/f", "F");
      ],
      [
        Rename ("p/a/b", "q/b");
        Rename ("p/a", "q/b/a");
        Dopen (0, "q/b");
        Dsync 0;
      ] );
    ( "ancestors-chain",
      root_user,
      [ I_dir "a"; I_dir "a/b"; I_dir "a/b/c"; I_dir "d" ],
      [
        Rename ("a/b/c", "d/c");
        Rename ("a/b", "d/c/b");
        Rename ("a", "d/c/b/a");
        Fsync_dir "d/c/b";
      ] );
    ( "perdir-order",
      root_user,
      [ I_dir "a"; I_dir "b"; I_file ("a/f", "F") ],
      [ Creat "b/g"; Rename ("a/f", "b/f"); Fsync_dir "a" ] );
    ( "perdir-unlink-recreate",
      root_user,
      [ I_dir "a"; I_dir "b"; I_file ("a/f", "F") ],
      [ Unlink "a/f"; Creat "b/f"; Rename ("b/f", "a/f"); Fsync_dir "b" ] );
    ( "handle-after-rename",
      root_user,
      [ I_dir "a" ],
      [
        Dopen (0, "a");
        Rename ("a", "c");
        Mkdir "a";
        Creat "c/f";
        Creat "a/g";
        Dsync 0;
      ] );
    ( "handle-after-rmdir",
      root_user,
      [ I_dir "a" ],
      [ Dopen (0, "a"); Rmdir "a"; Mkdir "a"; Creat "a/g"; Dsync 0 ] );
    ( "handle-new-dir",
      root_user,
      [],
      [
        Mkdir "a";
        Dopen (0, "a");
        Creat "a/f";
        Dsync 0;
        Rename ("a", "b");
        Fsync_dir "";
      ] );
    ( "deferred-rename-out",
      root_user,
      [ I_dir "a"; I_file ("a/f", "F") ],
      [ Mkdir "n"; Rename ("a/f", "n/f"); Fsync_dir "n" ] );
    ( "deferred-chain",
      root_user,
      [],
      [ Mkdir "a"; Mkdir "a/b"; Creat "a/b/f"; Fsync_dir "a/b"; Fsync_dir "a" ]
    );
    ( "handle-after-rmdir2",
      root_user,
      [ I_dir "a"; I_file ("a/old", "O") ],
      [
        Dopen (0, "a");
        Unlink "a/old";
        Rmdir "a";
        Mkdir "a";
        Chmod ("a", 0o700);
        Creat "a/g";
        Dsync 0;
      ] );
    ( "handle-after-rmdir-path",
      root_user,
      [ I_dir "a"; I_file ("a/old", "O") ],
      [
        Unlink "a/old";
        Rmdir "a";
        Mkdir "a";
        Chmod ("a", 0o700);
        Creat "a/g";
        Fsync_dir "a";
      ] );
    ( "late-commit-renamed-dir",
      root_user,
      [ I_dir "a" ],
      [ Creat "a/f"; Rename ("a", "b"); Fsync_dir ""; Fsync_dir "b" ] );
    ( "late-commit-moved-file",
      root_user,
      [ I_dir "a"; I_dir "b" ],
      [
        Creat "a/f";
        Mkdir "b/c";
        Fsync_dir "b";
        Rename ("a/f", "b/c/f");
        Fsync_dir "b/c";
        Fsync_dir "a";
      ] );
    ( "late-commit-chmod",
      root_user,
      [ I_dir "a"; I_file ("a/f", "F") ],
      [
        Chmod ("a/f", 0o600);
        Rename ("a/f", "a/g");
        Chmod ("a/g", 0o640);
        Fsync "a/g";
        Fsync_dir "a";
      ] );
    ( "late-commit-data",
      root_user,
      [ I_dir "a" ],
      [
        Creat "a/f";
        Write ("a/f", 0, "DATA");
        Fsync "a/f";
        Rename ("a", "b");
        Fsync_dir "";
        Fsync_dir "b";
      ] );
    ( "chmod-dir-self",
      root_user,
      [ I_dir "a" ],
      [ Chmod ("a", 0o700); Fsync_dir "a" ] );
    ( "chmod-dir-self-ro",
      root_user,
      [ I_dir "a" ],
      [ Chmod ("a", 0o700); Dopen (0, "a"); Dsync 0 ] );
    ( "failed-sync-same-block",
      root_user,
      [ I_file ("f", "AAAAAAAA") ],
      [ Write ("f", 0, "BB"); Fsync_fail "f"; Write ("f", 4, "CC"); Fsync "f" ]
    );
    ( "failed-sync-other-block",
      root_user,
      [ I_file ("f", "AAAAAAAA") ],
      [
        Write ("f", 0, "BB"); Fsync_fail "f"; Write ("f", 6000, "CC"); Fsync "f";
      ] );
    ( "failed-sync-twice",
      root_user,
      [ I_file ("f", "ABCDEFGHIJ") ],
      [
        Write ("f", 1, "bbbbb");
        Fsync_fail "f";
        Write ("f", 7, "ccccc");
        Fsync_fail "f";
      ] );
    ( "failed-sync-cut-page",
      root_user,
      [ I_file ("f", "AAAAAAAA") ],
      [ Write ("f", 6, "CC"); Truncate ("f", 2); Fsync_fail "f" ] );
  ]

(* Findings that mean eio-fs left a state the reference forbids, or did not
   line up with it. The others are coverage: an allowed state eio-fs never
   showed. [missing-skeleton] is exact, as Survive tries every cut; the
   Reorder ones ([missing-len], [missing-blk], [live-blk-missing]) depend on
   the seeds drawn, so they are reported, not asserted. *)
let sampled cls =
  List.exists
    (fun p -> String.starts_with ~prefix:p cls)
    [ "missing-len"; "missing-blk"; "live-blk-missing" ]

let report = Option.is_some (Sys.getenv_opt "REFERENCE2_REPORT")

let pp_findings =
  Fmt.(
    list ~sep:(any "@.") (fun ppf f ->
        pf ppf "-- %s %s@.%s" f.cls f.policy f.detail))

let test_case ((name, user, init, ops) as _c) =
  Alcotest.test_case name `Quick (fun () ->
      let fs, _ = check ~seeds:64 ~survive_cap:256 ~user ~init ~ops () in
      let bad = List.filter (fun f -> not (sampled f.cls)) fs in
      if bad <> [] then
        Alcotest.failf "%a@.%a" pp_workload (user, init, ops) pp_findings bad)

let workloads = 500

let test_differential () =
  let counts = Hashtbl.create 8 and bad = ref [] in
  for i = 0 to workloads - 1 do
    let rs = Random.State.make [| i |] in
    let user = if i mod 3 = 2 then plain_user else root_user in
    let init, ops = gen rs ~user in
    let fs, _ = check ~user ~init ~ops () in
    let seen = Hashtbl.create 4 in
    List.iter
      (fun f ->
        if not (Hashtbl.mem seen f.cls) then begin
          Hashtbl.replace seen f.cls ();
          Hashtbl.replace counts f.cls
            (1 + Option.value ~default:0 (Hashtbl.find_opt counts f.cls));
          if not (sampled f.cls) then bad := (user, init, ops, f) :: !bad
        end)
      fs
  done;
  if report then
    Hashtbl.iter (fun cls n -> Fmt.epr "%-24s %d workloads@." cls n) counts;
  match List.rev !bad with
  | [] -> ()
  | (user, init, ops, f) :: _ ->
      Alcotest.failf "%d findings, e.g.@.%a@.%a" (List.length !bad) pp_workload
        (user, init, ops) pp_findings [ f ]

let suite =
  ( "reference2",
    Alcotest.test_case
      (Fmt.str "%d workloads under every policy" workloads)
      `Quick test_differential
    :: List.map test_case cases )

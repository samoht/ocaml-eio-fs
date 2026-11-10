(* A differential of Chaos.Crash against the reference model: every state
   eio-fs produces must be one the reference allows, and the states it
   never produces fall in the classes its documented rules explain. *)

open Reference
module Crash = Eio_fs.Chaos.Crash

let alice = { Eio_fs.uid = 1000; gid = 1000; groups = []; umask = 0o022 }
let setup = [ Mkdir "a"; Mkdir "b"; Creat ("a/f", '0') ]

let fresh ~as_alice =
  let admin = (Eio_fs.Fs.v () :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio.Path.chmod ~follow:true ~perm:0o777 admin;
  let base =
    if as_alice then (Eio_fs.Fs.as_user alice admin :> Eio.Fs.dir_ty Eio.Path.t)
    else admin
  in
  List.iter (exec base) setup;
  (admin, base)

(* The live trees after the setup and after each operation, and the events
   of each operation. *)
let live ~as_alice ops =
  Eio_mock.Backend.run @@ fun () ->
  let admin, base = fresh ~as_alice in
  let ids = ids () in
  let t0 = snapshot ids admin in
  let id_at p = id ids (Eio.Path.stat ~follow:true p).ino in
  Eio.Switch.run @@ fun sw ->
  let dirs = Hashtbl.create 2 and handle_ids = Hashtbl.create 2 in
  let trees, evs, _ =
    List.fold_left
      (fun (trees, evs, before) op ->
        (match op with
        | Dopen (h, p) ->
            Hashtbl.replace handle_ids h (id_at Eio.Path.(admin / p))
        | _ -> ());
        exec ~sw ~dirs base op;
        let after = snapshot ids admin in
        let barrier =
          match op with
          | Fsync p -> [ Bfile (id_at Eio.Path.(admin / p)) ]
          | Fsync_dir "" -> [ Bdir (id_at admin) ]
          | Fsync_dir p -> [ Bdir (id_at Eio.Path.(admin / p)) ]
          | Dsync h -> [ Bdir (Hashtbl.find handle_ids h) ]
          | _ -> []
        in
        (after :: trees, events ~before ~after ~barrier :: evs, after))
      ([ t0 ], [], t0) ops
  in
  (t0, List.rev trees, List.rev evs)

let is_data (u : Crash.unsynced) =
  match u.kind with Data | Truncate -> true | _ -> false

(* The image a crash after the first [k] operations leaves under [policy],
   and what was pending then. *)
let crash ?block_size ~as_alice ops k policy =
  Eio_mock.Backend.run @@ fun () ->
  let admin, base = fresh ~as_alice in
  let ids = ids () in
  ignore (snapshot ids admin);
  let ctl = Crash.v ?block_size ~policy () in
  let fs = (Crash.wrap ctl base :> Eio.Fs.dir_ty Eio.Path.t) in
  Eio.Switch.run @@ fun sw ->
  let dirs = Hashtbl.create 2 in
  List.iteri
    (fun i op ->
      if i < k then begin
        exec ~sw ~dirs fs op;
        ignore (snapshot ids admin)
      end)
    ops;
  let pending = Crash.unsynced ctl in
  Crash.crash ctl;
  let post = Eio_fs.Fs.as_user Eio_fs.root (Crash.post_crash ctl) in
  let image =
    List.map
      (fun (path, ino, kind) ->
        let perm = (Eio.Path.stat ~follow:false Eio.Path.(post / path)).perm in
        let id = Option.value ~default:(-1) (id_opt ids ino) in
        match kind with
        | `File content -> { path; k = `F; id; perm; content }
        | `Dir -> { path; k = `D; id; perm; content = "" }
        | `Link _ -> failwith "unexpected link")
      (Eio_fs.Chaos.Private.crash_image ctl)
  in
  (List.sort compare image, pending)

(* The images eio-fs leaves after the first [k] operations: under
   Drop_unsynced; under Survive with every data change kept and the
   uncommitted metadata cut after each pending entry; and under
   Reorder_unsynced for a few seeds at block sizes 1, 7 and 4096, kept
   apart as their data mixes blocks. *)
let reorder_seeds = 2

let eio_states ~as_alice ops k =
  let drop, pending = crash ~as_alice ops k Drop_unsynced in
  let cuts =
    -1
    :: List.filter_map
         (fun (u : Crash.unsynced) -> if is_data u then None else Some u.seq)
         pending
  in
  let exact =
    List.fold_left
      (fun acc cut ->
        let keep (u : Crash.unsynced) = is_data u || u.seq <= cut in
        let image, _ = crash ~as_alice ops k (Survive keep) in
        States.add image acc)
      (States.singleton drop) cuts
  in
  let reorder =
    List.concat_map
      (fun bs ->
        List.init reorder_seeds (fun seed ->
            let policy = Crash.Reorder_unsynced seed in
            (bs, fst (crash ~block_size:bs ~as_alice ops k policy))))
      [ 1; 7; 4096 ]
  in
  (exact, reorder)

(* Whether [content] is one a crash may leave from the [versions] a file
   had, block by block: a length one of them had or a block boundary
   between them, and each block as one of them held it. *)
let blocks_ok ~bs versions content =
  let n = String.length content in
  let lens = List.map String.length versions in
  let lo = List.fold_left min max_int lens and hi = List.fold_left max 0 lens in
  let block s b =
    String.init bs (fun j ->
        let x = (b * bs) + j in
        if x < String.length s then s.[x] else '\000')
  in
  (List.mem n lens || (n mod bs = 0 && n > lo && n < hi))
  && List.for_all
       (fun b ->
         let got = String.sub (block content b) 0 (min bs (n - (b * bs))) in
         List.exists
           (fun v -> String.sub (block v b) 0 (String.length got) = got)
           versions)
       (List.init ((n + bs - 1) / bs) Fun.id)

(* {1 Workloads} *)

let files = [ "a/f"; "a/g"; "b/f"; "c/f"; "a/d/f"; "b/d/f"; "c/d/f" ]
let dirs = [ "c"; "a/d"; "b/d"; "c/d" ]
let barrier_dirs = [ ""; "a"; "b"; "c"; "a/d"; "b/d"; "c/d" ]
let pick rng l = List.nth l (Random.State.int rng (List.length l))
let letter rng = Char.chr (Char.code 'a' + Random.State.int rng 26)

let gen_op rng =
  match Random.State.int rng 11 with
  | 0 | 1 -> Creat (pick rng files, letter rng)
  | 2 -> Append (pick rng files, letter rng)
  | 3 ->
      if Random.State.bool rng then Rename (pick rng files, pick rng files)
      else Rename (pick rng dirs, pick rng dirs)
  | 4 -> Unlink (pick rng files)
  | 5 -> Mkdir (pick rng dirs)
  | 6 -> Rmdir (pick rng dirs)
  | 7 -> Fsync (pick rng files)
  | 8 | 9 -> Fsync_dir (pick rng barrier_dirs)
  | _ ->
      Chmod
        ( pick rng (files @ dirs),
          pick rng [ 0o600; 0o644; 0o4755; 0o700; 0o755 ] )

(* [len] operations that succeed live. *)
let gen ~as_alice rng len =
  Eio_mock.Backend.run @@ fun () ->
  let _, base = fresh ~as_alice in
  let rec go acc tries =
    if List.length acc >= len || tries > 200 then List.rev acc
    else
      let op = gen_op rng in
      match exec base op with
      | () -> go (op :: acc) (tries + 1)
      | exception Eio.Io _ -> go acc (tries + 1)
  in
  go [] 0

(* {1 The differential} *)

(* Why a state the reference allows, without its data, is missing from
   eio-fs's:
   - [Prefix]: what was never committed survives as a program-order prefix
     ("metadata never reorders", chaos.mli);
   - [Ancestors]: a rename waits for the renames that moved a directory
     above where it lands;
   - [Clear]: a commit carries the setuid clears before it;
   - [Other]: nothing documented explains it. *)
type class_ = Prefix | Ancestors | Clear | Other

let class_name = function
  | Prefix -> "prefix"
  | Ancestors -> "ancestors"
  | Clear -> "clear"
  | Other -> "other"

type report = {
  states : int;
  unsound : string list;
  missing : (class_ * int) list;
  examples : (class_ * string) list;
}

let take k l = List.filteri (fun i _ -> i < k) l

(* Workloads the random ones may miss, run first: a file met after an
   uncommitted rename of it, a rename over a directory the image still
   fills, and a barrier through a directory handle once the directory is
   gone. *)
let directed =
  [
    [ Rename ("a/f", "b/f"); Append ("b/f", 's'); Fsync "b/f" ];
    [ Rename ("a", "c"); Append ("c/f", 's'); Fsync "c/f" ];
    [ Rename ("a/f", "b/f"); Rename ("b", "a"); Fsync_dir "" ];
    [
      Dopen (0, "a");
      Unlink "a/f";
      Rmdir "a";
      Mkdir "a";
      Creat ("a/g", 'g');
      Dsync 0;
    ];
  ]

let differential workloads =
  let unsound = ref [] and n_states = ref 0 in
  let missing = Hashtbl.create 4 and examples = Hashtbl.create 4 in
  for w = 0 to workloads - 1 do
    let rng = Random.State.make [| w |] in
    let as_alice = w mod 2 = 1 in
    let ops =
      match List.nth_opt directed w with
      | Some ops -> ops
      | None -> gen ~as_alice rng (3 + Random.State.int rng 4)
    in
    let ((root, init) as t0), trees, evs_by_op = live ~as_alice ops in
    let base = bases trees in
    for k = 0 to List.length ops do
      let evs = List.concat (take k evs_by_op) in
      let per_op = List.mapi (fun p e -> (p, e)) (take k evs_by_op) in
      (* A write's setuid clear is journaled at once, and any later
         journal event carries it. *)
      let clear_idx =
        let clears = Hashtbl.create 4 in
        ignore
          (List.fold_left
             (fun base (p, es) ->
               (match List.nth ops p with
               | Chmod _ -> ()
               | _ ->
                   List.iteri
                     (fun a e ->
                       match e with
                       | Attrs _ -> Hashtbl.replace clears (base + a) ()
                       | _ -> ())
                     es);
               base + List.length es)
             0 per_op);
        Hashtbl.mem clears
      in
      let evs_a = Array.of_list evs in
      let covers i j =
        clear_idx i
        && match evs_a.(j) with Meta _ | Init _ | Attrs _ -> true | _ -> false
      in
      let r = states ~covers ~root ~init ~base evs in
      let ref_meta = States.map without_data r.all in
      (* A state eio-fs leaves is a prefix one under its rules: the
         barriers' closure, with the renames above where one lands and the
         setuid clears a later commit carries, and a prefix of the rest. *)
      let shaped = States.map without_data r.eio in
      (* A file made since the setup starts empty. *)
      let versions id =
        List.sort_uniq compare
          ((if IM.mem id (snd t0) then [] else [ "" ])
          @ List.filter_map
              (fun (_, nodes) ->
                match IM.find_opt id nodes with
                | Some { kind = `F; data; _ } -> Some data
                | Some _ | None -> None)
              (take (k + 1) trees))
      in
      let exact, reorder = eio_states ~as_alice ops k in
      n_states := !n_states + States.cardinal exact + List.length reorder;
      let ctx what s =
        Fmt.str "w%d%s k=%d (%s): %a@.    events %a@.    state %a" w
          (if as_alice then " alice" else "")
          k what pp_ops (take k ops) pp_events evs pp_state s
      in
      let bad what s = unsound := ctx what s :: !unsound in
      States.iter
        (fun s ->
          if not (States.mem s r.all) then bad "outside, with its data" s
          else if not (States.mem (without_data s) shaped) then
            bad "not a prefix state" s)
        exact;
      List.iter
        (fun (bs, s) ->
          let m = without_data s in
          if not (States.mem m ref_meta) then bad "outside" s
          else if not (States.mem m shaped) then bad "not a prefix state" s
          else
            List.iter
              (fun e ->
                if e.k = `F && not (blocks_ok ~bs (versions e.id) e.content)
                then
                  bad
                    (Fmt.str "%s: blocks of %d bytes, versions %a" e.path bs
                       Fmt.(Dump.list Dump.string)
                       (versions e.id))
                    s)
              s)
        reorder;
      let eio =
        States.union
          (States.map without_data exact)
          (States.of_list (List.map (fun (_, s) -> without_data s) reorder))
      in
      let prefix = States.map without_data r.prefix
      and ancestors = States.map without_data r.ancestors in
      States.iter
        (fun s ->
          let c =
            if not (States.mem s prefix) then Prefix
            else if not (States.mem s ancestors) then Ancestors
            else if not (States.mem s shaped) then Clear
            else Other
          in
          Hashtbl.replace missing c
            (1 + Option.value ~default:0 (Hashtbl.find_opt missing c));
          if not (Hashtbl.mem examples c) then
            Hashtbl.add examples c (ctx "missing" s))
        (States.diff ref_meta eio)
    done
  done;
  let classes = [ Prefix; Ancestors; Clear; Other ] in
  {
    states = !n_states;
    unsound = List.rev !unsound;
    missing =
      List.map
        (fun c -> (c, Option.value ~default:0 (Hashtbl.find_opt missing c)))
        classes;
    examples =
      List.filter_map
        (fun c -> Option.map (fun e -> (c, e)) (Hashtbl.find_opt examples c))
        classes;
  }

let workloads = 3000

(* The states eio-fs produces that the reference forbids, known unsound
   until fixed. *)
let expected_unsound = 0

(* What eio-fs never produces over [workloads] workloads, by class. A count
   that moves is a change of the crash model: say why. *)
let expected_missing =
  [ (Prefix, 7556); (Ancestors, 0); (Clear, 6); (Other, 0) ]

let pp_missing =
  Fmt.(list ~sep:(any ", ") (fun ppf (c, n) -> pf ppf "%s %d" (class_name c) n))

let test_differential () =
  let r = differential workloads in
  if Option.is_some (Sys.getenv_opt "REFERENCE_REPORT") then
    Fmt.epr "@[<v>%d workloads, %d eio-fs states@,missing: %a@,%a@]@." workloads
      r.states pp_missing r.missing
      Fmt.(list (fun ppf (c, e) -> pf ppf "e.g. %s: %s" (class_name c) e))
      r.examples;
  let n = List.length r.unsound in
  if n <> expected_unsound then
    Alcotest.failf "%d states the reference forbids, not %d%a" n
      expected_unsound
      Fmt.(list (any "@.  " ++ string))
      (take 3 r.unsound);
  Alcotest.(check (list (pair string int)))
    "missing states, by class"
    (List.map (fun (c, n) -> (class_name c, n)) expected_missing)
    (List.map (fun (c, n) -> (class_name c, n)) r.missing)

let suite =
  ( "reference",
    [
      Alcotest.test_case
        (Fmt.str "%d workloads against the reference" workloads)
        `Quick test_differential;
    ] )

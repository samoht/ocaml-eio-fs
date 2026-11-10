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
  let trees, evs, _ =
    List.fold_left
      (fun (trees, evs, before) op ->
        exec base op;
        let after = snapshot ids admin in
        let barrier =
          match op with
          | Fsync p -> [ Bfile (id_at Eio.Path.(admin / p)) ]
          | Fsync_dir "" -> [ Bdir (id_at admin) ]
          | Fsync_dir p -> [ Bdir (id_at Eio.Path.(admin / p)) ]
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
let crash ~as_alice ops k policy =
  Eio_mock.Backend.run @@ fun () ->
  let admin, base = fresh ~as_alice in
  let ids = ids () in
  ignore (snapshot ids admin);
  let ctl = Crash.v ~policy () in
  let fs = (Crash.wrap ctl base :> Eio.Fs.dir_ty Eio.Path.t) in
  List.iteri
    (fun i op ->
      if i < k then begin
        exec fs op;
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

(* The states eio-fs leaves, without their data: every data change kept,
   and the uncommitted metadata cut after each pending entry. *)
let eio_states ~as_alice ops k =
  let _, pending = crash ~as_alice ops k (Survive (fun _ -> true)) in
  let cuts =
    -1
    :: List.filter_map
         (fun (u : Crash.unsynced) -> if is_data u then None else Some u.seq)
         pending
  in
  List.fold_left
    (fun acc cut ->
      let keep (u : Crash.unsynced) = is_data u || u.seq <= cut in
      let image, _ = crash ~as_alice ops k (Survive keep) in
      States.add (without_data image) acc)
    States.empty cuts

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

(* Why a state the reference allows is missing from eio-fs's:
   - [Mode]: eio-fs has it with other modes, as a mode change is journaled
     with the directory operations;
   - [Prefix]: what was never committed survives as a program-order prefix
     ("metadata never reorders", chaos.mli);
   - [Ancestors]: a rename waits for the renames that moved a directory
     above where it lands;
   - [Other]: nothing documented explains it. *)
type class_ = Mode | Prefix | Ancestors | Other

let class_name = function
  | Mode -> "mode"
  | Prefix -> "prefix"
  | Ancestors -> "ancestors"
  | Other -> "other"

type report = {
  states : int;
  unsound : string list;
  missing : (class_ * int) list;
  examples : (class_ * string) list;
}

let take k l = List.filteri (fun i _ -> i < k) l

let differential workloads =
  let unsound = ref [] and n_states = ref 0 in
  let missing = Hashtbl.create 4 and examples = Hashtbl.create 4 in
  for w = 0 to workloads - 1 do
    let rng = Random.State.make [| w |] in
    let as_alice = w mod 2 = 1 in
    let ops = gen ~as_alice rng (3 + Random.State.int rng 4) in
    let (root, init), trees, evs = live ~as_alice ops in
    let base = bases trees in
    for k = 0 to List.length ops do
      let evs = List.concat (take k evs) in
      let r = states ~root ~init ~base evs in
      let ref_all = States.map without_data r.all in
      let eio = eio_states ~as_alice ops k in
      n_states := !n_states + States.cardinal eio;
      let ctx s =
        Fmt.str "w%d%s k=%d: %a@.    events %a@.    state %a" w
          (if as_alice then " alice" else "")
          k pp_ops (take k ops) pp_events evs pp_state s
      in
      States.iter
        (fun s -> unsound := ctx s :: !unsound)
        (States.diff eio ref_all);
      let prefix = States.map without_data r.prefix
      and ancestors = States.map without_data r.ancestors in
      let eio_modeless = States.map without_modes eio in
      States.iter
        (fun s ->
          let c =
            if States.mem (without_modes s) eio_modeless then Mode
            else if not (States.mem s prefix) then Prefix
            else if not (States.mem s ancestors) then Ancestors
            else Other
          in
          Hashtbl.replace missing c
            (1 + Option.value ~default:0 (Hashtbl.find_opt missing c));
          if not (Hashtbl.mem examples c) then Hashtbl.add examples c (ctx s))
        (States.diff ref_all eio)
    done
  done;
  let classes = [ Mode; Prefix; Ancestors; Other ] in
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
let expected_unsound = 4

(* What eio-fs never produces over [workloads] workloads, by class. A count
   that moves is a change of the crash model: say why. *)
let expected_missing =
  [ (Mode, 1283); (Prefix, 6438); (Ancestors, 0); (Other, 2) ]

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

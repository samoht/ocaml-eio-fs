(* One file's data under failed syncs, at blocks of 2 bytes, against the
   block-level reference in refm.ml: every workload of two small scopes to
   depth 5, a failure injected above the model, beneath it, or through two
   stacked wrappers, and syncs through the open handle or a fresh read-only
   one. Drop_unsynced, Survive with every change, and Reorder_unsynced until
   no new image comes; each image must be one the reference allows. Directed
   cases check that states the reference allows are drawn. *)

open Eio_fs.Chaos

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

type op =
  | W of int * int (* offset, length *)
  | T of int
  | Sync
  | Fail
  | Ro_fail (* a failed sync through a fresh read-only handle *)

let pp_op ppf = function
  | W (o, n) -> Fmt.pf ppf "w(%d,%d)" o n
  | T m -> Fmt.pf ppf "t(%d)" m
  | Sync -> Fmt.string ppf "sync"
  | Fail -> Fmt.string ppf "FAIL"
  | Ro_fail -> Fmt.string ppf "FAIL-ro"

let pp_wl = Fmt.(list ~sep:sp pp_op)

type injection = Above | Beneath | Stacked

let pp_injection ppf = function
  | Above -> Fmt.string ppf "above"
  | Beneath -> Fmt.string ppf "beneath"
  | Stacked -> Fmt.string ppf "stacked"

let fail_on_sync n =
  {
    Schedule.trigger = Nth_op_on_path { n; kind = `Sync; path = "/f" };
    fault = Sync_error `Eio;
  }

let fail_once =
  {
    Schedule.trigger = Op_on_path { kind = `Sync; path = "/f" };
    fault = Sync_error `Eio;
  }

(* The [k]th write of [wl] writes its letter. *)
let letters wl =
  let k = ref 0 in
  List.map
    (function
      | W (_, n) ->
          let c = Char.chr (Char.code 'A' + !k) in
          incr k;
          String.make n c
      | T _ | Sync | Fail | Ro_fail -> "")
    wl

(* Run [wl] from [init]; the crash image of the file. *)
let run ~init ~injection ~policy wl =
  Eio.Switch.run @@ fun sw ->
  let raw = Eio_fs.Fs.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (raw / "f") init;
  (* The failing syncs, by rank among the syncs through the handle. *)
  let rules =
    let n = ref 0 in
    List.filter_map
      (function
        | Sync ->
            incr n;
            None
        | Fail ->
            incr n;
            Some (fail_on_sync !n)
        | W _ | T _ | Ro_fail -> None)
      wl
  in
  let sched = Schedule.scripted rules in
  let ctl = Crash.v ~policy ~block_size:2 () in
  let root, files =
    match injection with
    | Beneath ->
        let c = Crash.wrap ctl (wrap sched raw) in
        (c, c)
    | Above ->
        let c = Crash.wrap ctl raw in
        (c, wrap sched c)
    | Stacked ->
        let c = Crash.wrap ctl raw in
        (c, wrap sched (wrap Schedule.empty c))
  in
  let h = Eio.Path.open_out ~sw ~create:`Never (files / "f") in
  List.iter2
    (fun d -> function
      | W (o, _) ->
          Eio.File.pwrite_all h ~file_offset:(i63 o) [ Cstruct.of_string d ]
      | T m -> Eio.File.truncate h (i63 m)
      | Sync -> Eio.File.sync h
      | Fail -> (
          match Eio.File.sync h with
          | () -> failwith "the sync did not fail"
          | exception Eio.Io _ -> ())
      | Ro_fail ->
          Eio.Path.with_open_in
            (wrap (Schedule.scripted [ fail_once ]) root / "f")
            (fun f ->
              match Eio_fs_sync.file_barrier f with
              | None -> failwith "no barrier"
              | Some b -> (
                  match b () with
                  | () -> failwith "the sync did not fail"
                  | exception Eio.Io _ -> ())))
    (letters wl) wl;
  Eio.Path.load (Crash.post_crash ctl / "f")

let reference ~init ~reorder wl =
  let r = Refm.v init in
  List.iter2
    (fun d -> function
      | W (o, _) -> Refm.write r o d
      | T m -> Refm.resize r m
      | Sync -> Refm.sync_ok r
      | Fail | Ro_fail -> Refm.sync_fail ~reorder r)
    (letters wl) wl;
  r

let quiet_seeds = 20
let max_seeds = 1000

type failure = Outside of string | Unsaturated of string

(* Every image of [wl], checked; the Reorder images. *)
let check_workload ~init wl =
  let found = ref [] in
  let plain = reference ~init ~reorder:false wl
  and reorder = reference ~init ~reorder:true wl in
  let one ~injection ~policy ~ref_policy r =
    let img = run ~init ~injection ~policy wl in
    (match Refm.check ~policy:ref_policy r img with
    | None -> ()
    | Some why ->
        found :=
          Outside
            (Fmt.str "%a, %a: %S: %s" pp_wl wl pp_injection injection img why)
          :: !found);
    img
  in
  List.iter
    (fun injection ->
      ignore
        (one ~injection ~policy:Crash.Drop_unsynced ~ref_policy:Refm.Drop plain);
      ignore
        (one ~injection
           ~policy:(Crash.Survive (fun _ -> true))
           ~ref_policy:Refm.Survive_all plain))
    [ Above; Beneath; Stacked ];
  let seen = Hashtbl.create 32 in
  let rec go seed quiet =
    if quiet >= quiet_seeds then ()
    else if seed >= max_seeds then
      found := Unsaturated (Fmt.str "%a" pp_wl wl) :: !found
    else
      let injection =
        match seed mod 3 with 0 -> Above | 1 -> Beneath | _ -> Stacked
      in
      let img =
        one ~injection ~policy:(Crash.Reorder_unsynced seed)
          ~ref_policy:Refm.Reorder reorder
      in
      if Hashtbl.mem seen img then go (seed + 1) (quiet + 1)
      else begin
        Hashtbl.add seen img ();
        go (seed + 1) 0
      end
  in
  go 0 0;
  (!found, seen)

let failing wl = List.exists (function Fail | Ro_fail -> true | _ -> false) wl

let check_scope ~init ~depth alphabet () =
  Eio_mock.Backend.run @@ fun () ->
  let found = ref [] and n = ref 0 in
  let rec go acc d =
    let wl = List.rev acc in
    if failing wl then begin
      incr n;
      found := fst (check_workload ~init wl) @ !found
    end;
    if d < depth then List.iter (fun op -> go (op :: acc) (d + 1)) alphabet
  in
  go [] 0;
  match List.rev !found with
  | [] -> ()
  | f :: _ as fs ->
      Alcotest.failf "%d workloads, %d failures, e.g. %s" !n (List.length fs)
        (match f with Outside m | Unsaturated m -> m)

(* An empty file: a lost page and the cut that frees it. *)
let empty = [ W (0, 1); T 0; T 1; Sync; Fail; Ro_fail ]

(* A file of 3 bytes: a lost page past a cut on a block boundary. *)
let three = [ W (1, 2); W (3, 1); T 1; Sync; Fail; Ro_fail ]

(* States the reference allows, which Reorder_unsynced must draw. *)
let test_drawn () =
  Eio_mock.Backend.run @@ fun () ->
  let images wl =
    let _, seen = check_workload ~init:"pqr" wl in
    Hashtbl.fold (fun k () acc -> k :: acc) seen []
  in
  let need wl img =
    if not (List.mem img (images wl)) then
      Alcotest.failf "%a never gives %S" pp_wl wl img
  in
  (* evicted before a later sync *)
  need [ W (0, 1); Fail; W (1, 1) ] "pBr";
  (* the first of two failed writebacks landed *)
  need [ W (0, 1); Fail; W (1, 1); Fail ] "Aqr"

let suite =
  ( "blocks",
    [
      Alcotest.test_case "an empty file, depth 5" `Quick
        (check_scope ~init:"" ~depth:5 empty);
      Alcotest.test_case "a file of 3 bytes, depth 5" `Quick
        (check_scope ~init:"pqr" ~depth:5 three);
      Alcotest.test_case "states the reference allows are drawn" `Quick
        test_drawn;
    ] )

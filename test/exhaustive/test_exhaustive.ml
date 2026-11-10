(* Every workload of small scopes, every crash state each policy leaves:
   Drop_unsynced, every Survive cut of the metadata times every subset of
   the data, and Reorder_unsynced at blocks of 2 bytes until 100 seeds in a
   row bring no new image. Each image must keep the invariants of
   oracle.ml. Chaos.Crash runs over a fault wrapper, which fails a sync
   beneath it; a sync also fails above it, alone or through a second
   wrapper. *)

open Core

type scope = { name : string; init : init list; ops : op list; depth : int }

let w p = Write (Root, p, 0, 2)

let scopes =
  [
    (* files across nested directories, renames replacing files *)
    {
      name = "files";
      depth = 2;
      init =
        [
          IDir ("d", 0o755);
          IDir ("d/e", 0o755);
          IFile ("f", "xy", 0o644, 0);
          IFile ("d/e/f", "zw", 0o644, 0);
        ];
      ops =
        [
          w "f";
          w "g";
          w "d/f";
          w "d/e/f";
          Fsync "f";
          Fsync "g";
          Fsync "d/f";
          Rename ("f", "g");
          Rename ("g", "f");
          Rename ("f", "d/f");
          Rename ("d/f", "d/e/f");
          Rename ("d/e/f", "h");
          Unlink "f";
          Unlink "d/e/f";
          Sync_dir ".";
          Sync_dir "d";
          Sync_dir "d/e";
          Sync_meta;
        ];
    };
    (* directories: mkdir, rmdir, renames over empty directories, nesting *)
    {
      name = "directories";
      depth = 3;
      init =
        [
          IDir ("d", 0o755); IDir ("d/e", 0o755); IFile ("d/e/f", "xy", 0o644, 0);
        ];
      ops =
        [
          Mkdir "e";
          Rmdir "e";
          Rename ("d", "e");
          Rename ("e", "d");
          Rename ("d/e", "e");
          Rename ("e", "d/e");
          Rename ("d/e/f", "d/f");
          Rename ("d/f", "d/e/f");
          w "d/e/f";
          w "e/f";
          Fsync "d/e/f";
          Unlink "d/e/f";
          Rmdir "d/e";
          Sync_dir ".";
          Sync_dir "d";
          Sync_dir "d/e";
          Sync_dir "e";
          Sync_meta;
        ];
    };
    (* symbolic links *)
    {
      name = "links";
      depth = 3;
      init =
        [
          IFile ("f", "xy", 0o644, 0);
          IFile ("g", "zw", 0o644, 0);
          IDir ("d", 0o755);
        ];
      ops =
        [
          Symlink ("l", "f");
          Symlink ("l", "g");
          Symlink ("d/l", "../f");
          Rename ("l", "g");
          Rename ("g", "l");
          Rename ("l", "d/l");
          w "l";
          Fsync "l";
          Unlink "l";
          Unlink "f";
          w "f";
          Fsync "f";
          Rename ("f", "h");
          Sync_dir ".";
          Sync_dir "d";
          Sync_meta;
        ];
    };
    (* file handles across renames, directory handles across rename and
       replace *)
    {
      name = "handles";
      depth = 3;
      init =
        [ IDir ("d", 0o755); IDir ("e", 0o755); IFile ("d/f", "xy", 0o644, 0) ];
      ops =
        [
          Open_h "d/f";
          Write_h (0, 2);
          Write_h (2, 2);
          Sync_h;
          Rename ("d/f", "e/f");
          Rename ("d/f", "d/g");
          Rename ("d", "e");
          Rename ("e", "d");
          Open_dh "d";
          Sync_dh;
          Open_in_d "d";
          Sync_in_d;
          Mkdir "d";
          Write (Root, "d/g", 0, 1);
          Sync_dir "d";
          Sync_dir "e";
          Sync_dir ".";
          Unlink "d/f";
          Rmdir "e";
          Sync_meta;
        ];
    };
    (* one file at blocks of 2 bytes: boundaries, holes, append, truncations
       that cut a block mid-way, syncs failed above Chaos.Crash, through a
       second wrapper or beneath it *)
    {
      name = "blocks";
      depth = 3;
      init = [ IFile ("f", "xyz", 0o644, 0) ];
      ops =
        [
          Write (Root, "f", 0, 2);
          Write (Root, "f", 1, 2);
          Write (Root, "f", 3, 3);
          Write (Root, "f", 6, 1);
          Append ("f", 1);
          Trunc (Root, "f", 1);
          Trunc (Root, "f", 4);
          Fsync "f";
          Fsync_fail "f";
          Fsync_fail_s "f";
          Fsync_fail_b "f";
        ];
    };
    (* failed syncs with a second file, one beneath Chaos.Crash, rename over,
       handles *)
    {
      name = "failed syncs";
      depth = 3;
      init = [ IFile ("f", "xy", 0o644, 0) ];
      ops =
        [
          Write (Root, "g", 0, 3);
          Write (Root, "f", 1, 2);
          Open_h "f";
          Write_h (2, 2);
          Sync_h;
          Fsync "f";
          Fsync_fail "f";
          Fsync "g";
          Fsync_fail_b "g";
          Rename ("g", "f");
          Sync_dir ".";
          Trunc (Root, "f", 1);
          Sync_meta;
        ];
    };
    (* modes and owners, root or a user, setuid clears, a child's chmod and a
       barrier on its directory *)
    {
      name = "modes";
      depth = 2;
      init =
        [
          IDir ("d", 0o755);
          IFile ("f", "xy", 0o644, 0);
          IFile ("d/f", "zw", 0o644, 0);
        ];
      ops =
        [
          Chmod (Root, "f", 0o4777);
          Chmod (Root, "f", 0o6777);
          Chmod (Root, "f", 0o644);
          Chown ("f", 1000);
          Chmod (User, "f", 0o4755);
          Write (User, "f", 0, 2);
          Trunc (User, "f", 1);
          Write (Root, "f", 0, 2);
          Fsync "f";
          Fsync_fail "f";
          Sync_dir ".";
          Sync_meta;
          Chmod (Root, "d/f", 0o600);
          Chmod (Root, "d", 0o700);
          Write (Root, "d/g", 0, 1);
          Sync_dir "d";
        ];
    };
    (* a user's files in a setgid directory of a group the user is not in *)
    {
      name = "setgid";
      depth = 3;
      init = [ IDir_g ("s", 0o2777, 2000) ];
      ops =
        [
          Create (User, "s/f", 0o2755);
          Write (User, "s/g", 0, 2);
          Chmod (User, "s/f", 0o2755);
          Write (User, "s/f", 0, 1);
          Fsync "s/f";
          Sync_dir "s";
          Sync_meta;
        ];
    };
    (* a link to a file in a directory renamed over an empty one, handles
       through the link, failed syncs, a user's writes *)
    {
      name = "mixed";
      depth = 3;
      init =
        [
          IDir ("d", 0o755);
          IDir ("e", 0o755);
          IFile ("d/f", "xy", 0o4777, 0);
          ILink ("l", "d/f");
        ];
      ops =
        [
          Write (Root, "l", 1, 2);
          Write (User, "l", 0, 1);
          Fsync_fail "l";
          Fsync "l";
          Open_h "l";
          Write_h (2, 1);
          Sync_h;
          Rename ("d", "e");
          Symlink ("m", "e/f");
          Rename ("m", "l");
          Sync_dir "e";
          Open_dh "d";
          Sync_dh;
          Trunc (Root, "l", 1);
          Sync_dir ".";
          Sync_meta;
        ];
    };
  ]

(* A barrier and a sync that do nothing: the invariants must catch them. *)
let sanity =
  {
    name = "sanity";
    depth = 2;
    init =
      [
        IDir ("d", 0o755);
        IFile ("f", "xy", 0o644, 0);
        IFile ("g", "zw", 0o644, 0);
      ];
    ops =
      [
        w "h";
        Write (Root, "f", 1, 2);
        Unlink "g";
        Rename ("f", "g");
        Fake_barrier ".";
        Fake_fsync "f";
        Fake_fsync "h";
      ];
  }

let is_data (u : C.unsynced) =
  match u.kind with C.Data | C.Truncate -> true | _ -> false

let oracle_run ~init ops =
  let n = List.length ops in
  let snaps =
    Array.make (n + 1) { triples = TS.empty; nodes = IM.empty; paths = [] }
  in
  let root = ref 0L and unsynced = ref [] in
  let img =
    run ~init ~policy:C.Drop_unsynced
      ~observe:(fun i base ->
        let r, s = snapshot base in
        root := r;
        snaps.(i) <- s)
      ~before_crash:(fun t -> unsynced := C.unsynced t)
      ops
  in
  let evs = Oracle.events !root snaps ops (ref None) (ref None) (ref None) in
  let blocks, clears = Oracle.blocks !root snaps ops in
  ({ Oracle.root = !root; snaps; evs; blocks; clears }, !unsynced, img)

let max_seeds = 3000
let quiet_seeds = 100

type failure = Violation of string | Unsaturated of string

(* Every crash state of [ops], checked. *)
let check_workload ~init ops =
  let h, uns, drop = oracle_run ~init ops in
  let found = ref [] in
  let seen = Hashtbl.create 16 in
  let check ~reorder pol img =
    if not (Hashtbl.mem seen (reorder, img)) then begin
      Hashtbl.add seen (reorder, img) ();
      List.iter
        (fun (v : Oracle.viol) ->
          found :=
            Violation
              (Fmt.str "%s: %a under %s: %s@.    image: %a" v.inv pp_wl ops pol
                 v.detail pp_image img)
            :: !found)
        (Oracle.check ~reorder h img)
    end
  in
  check ~reorder:false "Drop" drop;
  let data = List.filter is_data uns
  and meta = List.filter (fun u -> not (is_data u)) uns in
  let rank = List.mapi (fun i (u : C.unsynced) -> (u.seq, i)) meta
  and idx = List.mapi (fun i (u : C.unsynced) -> (u.seq, i)) data in
  for cut = 0 to List.length meta do
    for mask = 0 to (1 lsl List.length data) - 1 do
      let pred (u : C.unsynced) =
        match List.assoc_opt u.seq rank with
        | Some r -> r < cut
        | None -> (
            match List.assoc_opt u.seq idx with
            | Some i -> mask land (1 lsl i) <> 0
            | None -> false)
      in
      check ~reorder:false
        (Fmt.str "Survive cut %d mask %x" cut mask)
        (run ~init ~policy:(C.Survive pred) ops)
    done
  done;
  if
    uns <> []
    || List.exists
         (function
           | Fsync_fail _ | Fsync_fail_u _ | Fsync_fail_s _ | Fsync_fail_b _ ->
               true
           | _ -> false)
         ops
  then begin
    let images = Hashtbl.create 16 in
    let rec go seed quiet =
      if quiet >= quiet_seeds then ()
      else if seed >= max_seeds then
        found :=
          Unsaturated
            (Fmt.str "%a: no saturation in %d seeds" pp_wl ops max_seeds)
          :: !found
      else
        let img = run ~init ~policy:(C.Reorder_unsynced seed) ops in
        if Hashtbl.mem images img then go (seed + 1) (quiet + 1)
        else begin
          Hashtbl.add images img ();
          check ~reorder:true (Fmt.str "Reorder %d" seed) img;
          go (seed + 1) 0
        end
    in
    go 0 0
  end;
  !found

(* Every workload of [s] that runs live, up to its depth. *)
let check_scope s =
  Eio_mock.Backend.run @@ fun () ->
  let found = ref [] and workloads = ref 0 in
  let rec dfs rev d =
    let ops = List.rev rev in
    let ok =
      ops = []
      ||
      match check_workload ~init:s.init ops with
      | fs ->
          incr workloads;
          found := fs @ !found;
          true
      | exception Invalid _ -> false
    in
    if ok && d < s.depth then
      List.iter (fun op -> dfs (op :: rev) (d + 1)) s.ops
  in
  dfs [] 0;
  (!workloads, List.rev !found)

let test_scope s =
  Alcotest.test_case (Fmt.str "%s, depth %d" s.name s.depth) `Quick (fun () ->
      match check_scope s with
      | _, [] -> ()
      | n, (f :: _ as fs) ->
          Alcotest.failf "%d workloads, %d failures, e.g. %s" n (List.length fs)
            (match f with Violation m | Unsaturated m -> m))

let test_sanity () =
  match check_scope sanity with
  | _, [] -> Alcotest.fail "a barrier that did nothing went unnoticed"
  | _ -> ()

let suite =
  ( "exhaustive",
    List.map test_scope scopes
    @ [
        Alcotest.test_case "the invariants catch a fake barrier" `Quick
          test_sanity;
      ] )

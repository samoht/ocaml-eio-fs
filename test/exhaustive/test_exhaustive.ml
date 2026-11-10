(* Every workload of two small scopes, every crash state each policy
   leaves, against invariants that follow from chaos.mli rather than from a
   model of it: durable bytes stay (a), a name binds what it did at its
   directory's last barrier or since (b, f), a durably linked file still
   linked is somewhere (c), no inode twice (d), every byte one written there
   or zero (e), and every mode one the inode had (g). *)

open Exhaustive

let ch i = Char.chr (Char.code 'a' + i)
let empty = { idirs = []; ifiles = []; imodes = [] }

(* Names and directories, as root, to depth 3. *)
let names =
  let files = [ "f"; "g"; "d/f"; "d/g" ] in
  ( Root,
    [
      empty;
      { empty with idirs = [ "d" ] };
      { empty with idirs = [ "d" ]; ifiles = [ ("d/f", "A"); ("g", "B") ] };
    ],
    List.map (fun p _ -> Creat p) files
    @ List.map (fun p i -> Write (p, 0, ch i)) files
    @ List.concat_map
        (fun a ->
          List.filter_map
            (fun b -> if a = b then None else Some (fun _ -> Rename (a, b)))
            files)
        files
    @ List.map (fun p _ -> Unlink p) files
    @ [ (fun _ -> Mkdir "d"); (fun _ -> Rmdir "d") ]
    @ List.map (fun p _ -> Fsync p) files
    @ [ (fun _ -> Fsyncdir ""); (fun _ -> Fsyncdir "d") ],
    3 )

(* One file's bytes, truncations, a failed sync and a mode, as a user, to
   depth 4. *)
let bytes =
  ( Plain,
    [ { empty with ifiles = [ ("f", "AB") ] } ],
    [
      (fun i -> Write ("f", 0, ch i));
      (fun i -> Write ("f", 1, ch i));
      (fun _ -> Trunc ("f", 0));
      (fun _ -> Trunc ("f", 1));
      (fun _ -> Chmod ("f", 0o4755));
      (fun _ -> Fsync "f");
      (fun _ -> Fsync_fail "f");
      (fun _ -> Sync_meta);
    ],
    4 )

let run (user, inits, alphabet, depth) () =
  max_seeds := 200;
  quiet_seeds := 20;
  let found = ref [] in
  List.iter
    (fun init ->
      enum ~init ~depth ~alphabet [] (fun ops ->
          check_workload ~user ~init ~ops ~report:(fun cls pol msg ->
              found := (cls, init, ops, pol, msg) :: !found)))
    inits;
  match List.rev !found with
  | [] -> ()
  | (cls, init, ops, pol, msg) :: _ ->
      Alcotest.failf "%d violations, e.g. %s: init %a ops [%a]@.  %s: %s"
        (List.length !found) cls pp_init init
        Fmt.(list ~sep:(any "; ") pp_op)
        ops pol msg

let suite =
  ( "exhaustive",
    [
      Alcotest.test_case "names and directories, depth 3" `Quick (run names);
      Alcotest.test_case "bytes, truncation, failed sync, mode, depth 4" `Quick
        (run bytes);
    ] )

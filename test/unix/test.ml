let hold_lock_arg = "--hold-eio-fs-lock"
let hold_lease_arg = "--hold-eio-fs-lease"
let held_lock_arg = "--hold-eio-fs-lock-until-release"

let held_lock path =
  let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  Unix.lockf fd Unix.F_LOCK 0;
  output_char stdout 'R';
  flush stdout;
  match Unix.select [ Unix.stdin ] [] [] 10.0 with
  | [], _, _ -> failwith "lock holder release timed out"
  | _ ->
      let release = Bytes.create 1 in
      if
        Unix.read Unix.stdin release 0 1 <> 1
        || not (Char.equal (Bytes.get release 0) 'X')
      then failwith "lock holder expected explicit release"

let hold_lock path ready seconds =
  let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      Out_channel.with_open_bin ready (fun _ -> ());
      Unix.sleepf seconds)

let hold_lease ready =
  Out_channel.with_open_bin ready (fun _ -> ());
  while true do
    Unix.sleep 3600
  done

let lease_test_arg = "--test-inherited-lease"
let try_lock_arg = "--try-eio-fs-lock"

(* Exit 0 when the record lock over [path] is granted to this process, 3 when
   another process holds it. *)
let try_lock path =
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0o600 in
  match Unix.lockf fd Unix.F_TLOCK 0 with
  | () -> exit 0
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) -> exit 3

let () =
  match Array.to_list Sys.argv with
  | [ _; arg; path ] when String.equal arg held_lock_arg -> held_lock path
  | [ _; arg; path; ready; seconds ] when String.equal arg hold_lock_arg ->
      hold_lock path ready (float_of_string seconds)
  | [ _; arg; ready ] when String.equal arg hold_lease_arg -> hold_lease ready
  | [ _; arg; path ] when String.equal arg try_lock_arg -> try_lock path
  | [ _; arg ] when String.equal arg lease_test_arg ->
      Sys.argv.(1) <- "test";
      Alcotest.run "eio-fs.unix.lease" [ Test_eio_fs_unix.lease_suite ]
  | _ -> Alcotest.run "eio-fs.unix" [ Test_eio_fs_unix.suite; Test_link.suite ]

let hold_lock_arg = "--hold-eio-fs-lock"
let hold_lease_arg = "--hold-eio-fs-lease"

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

let () =
  match Array.to_list Sys.argv with
  | [ _; arg; path; ready; seconds ] when String.equal arg hold_lock_arg ->
      hold_lock path ready (float_of_string seconds)
  | [ _; arg; ready ] when String.equal arg hold_lease_arg -> hold_lease ready
  | [ _; arg ] when String.equal arg lease_test_arg ->
      Sys.argv.(1) <- "test";
      Alcotest.run "eio-fs.unix.lease" [ Test_eio_fs_unix.lease_suite ]
  | _ -> Alcotest.run "eio-fs.unix" [ Test_eio_fs_unix.suite ]

let hold_lock_arg = "--hold-eio-fs-lock"

let hold_lock path ready seconds =
  let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      Out_channel.with_open_bin ready (fun _ -> ());
      Unix.sleepf seconds)

let () =
  match Array.to_list Sys.argv with
  | [ _; arg; path; ready; seconds ] when String.equal arg hold_lock_arg ->
      hold_lock path ready (float_of_string seconds)
  | _ -> Alcotest.run "eio-fs.unix" [ Test_eio_fs_unix.suite ]

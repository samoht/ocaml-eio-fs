(* (a) A pid lock file with stale-lock recovery, guarding a counter that is
   updated by temp-and-rename. *)
module P = Eio.Path

exception Held of int

let read_pid p =
  match P.load p with
  | s -> `Pid (int_of_string_opt (String.trim s))
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> `Gone

let acquire ?(bug = `None) ~pid ~alive dir =
  let lock = P.(dir / "lock") in
  let rec attempt n =
    if n > 3 then failwith "lock: still contended after 3 attempts";
    match
      P.with_open_out ~create:(`Exclusive 0o644) lock (fun f ->
          Eio.Flow.copy_string (string_of_int pid ^ "\n") f;
          Eio.File.sync f)
    with
    | () -> Eio_fs_sync.sync_dir dir
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> (
        match read_pid lock with
        | `Gone -> attempt (n + 1)
        | `Pid (Some p) when bug = `No_stale_check && p <> pid -> raise (Held p)
        | `Pid (Some p) when alive p -> raise (Held p)
        | `Pid (Some _) ->
            P.unlink lock;
            attempt (n + 1)
        | `Pid None when bug = `Empty_is_held -> raise (Held 0)
        | `Pid None ->
            (* The holder died between the create and the pid write. *)
            P.unlink lock;
            attempt (n + 1))
    | exception Eio.Io _ when bug = `Any_error_is_held -> raise (Held (-1))
  in
  attempt 0

let release dir =
  P.unlink P.(dir / "lock");
  Eio_fs_sync.sync_dir dir

let read_counter dir =
  match P.load P.(dir / "counter") with
  | s -> int_of_string (String.trim s)
  | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> 0

let write_counter dir n =
  P.with_open_out ~create:(`Or_truncate 0o644)
    P.(dir / "counter.tmp")
    (fun f ->
      Eio.Flow.copy_string (string_of_int n) f;
      Eio.File.sync f);
  P.rename P.(dir / "counter.tmp") P.(dir / "counter");
  Eio_fs_sync.sync_dir dir

let boots = ref 100

let test ?bug () =
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      let dir = env#fs and me = 1 in
      try
        for _ = 1 to 3 do
          acquire ?bug ~pid:me ~alive:(fun p -> p = me) dir;
          let n = read_counter dir + 1 in
          write_counter dir n;
          ack n;
          release dir
        done
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      (* A new boot: no earlier pid is alive. *)
      incr boots;
      let me = !boots in
      acquire ?bug ~pid:me ~alive:(fun p -> p = me) env#fs;
      let n = read_counter env#fs in
      let last = List.fold_left max 0 acked in
      if n < last || n > last + 1 then
        Fmt.failwith "counter is %d, acked %d" n last;
      release env#fs)

(* Under errors: acquire succeeds, or raises the error; it never reports a
   holder when there is none. *)
let test_errors ?bug () =
  Eio_fs.Chaos.check (fun env ->
      match acquire ?bug ~pid:1 ~alive:(fun p -> p = 1) env#fs with
      | () -> ( try release env#fs with Eio.Io _ -> ())
      | exception Eio.Io _ -> ()
      | exception Held p ->
          Fmt.failwith "no lock was held, yet acquire says %d holds it" p)

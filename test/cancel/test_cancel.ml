(* A seed draws files that are each written, by positioned and streaming
   writes, then read back, by positioned and streaming reads, and checked,
   while a fault cancels some of the reads and writes. A file may be written
   or read in a domain of its own, on a handle opened in the main one. *)

module Chaos = Eio_fs_chaos

let byte ~file i = Char.chr (((file * 31) + (i * 7)) land 0xff)

(* The fault of each read and write, from the seed and the operation alone,
   so the draws do not depend on the order in which domains reach them. *)
let plan seed (op : Chaos.Op.t) =
  match op.kind with
  | `Read | `Write ->
      let kind = if op.kind = `Read then 0 else 1 in
      let rng =
        Random.State.make
          [| seed; Hashtbl.hash op.path; kind; op.nth; op.offset |]
      in
      if Random.State.int rng 3 = 0 then
        Some (Chaos.Fault.Cancel (Random.State.int rng 5))
      else None
  | _ -> None

let int = Optint.Int63.of_int

let write_file dir ~file ~len ~chunk ~positioned =
  let data = String.init len (byte ~file) in
  Eio.Switch.run @@ fun sw ->
  let f =
    Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644)
      Eio.Path.(dir / Fmt.str "f%d" file)
  in
  let rec go off =
    if off < len then begin
      let n = min chunk (len - off) in
      let buf = Cstruct.of_string ~off ~len:n data in
      if positioned then Eio.File.pwrite_all f ~file_offset:(int off) [ buf ]
      else Eio.Flow.write f [ buf ];
      go (off + n)
    end
  in
  go 0

let read_file dir ~file ~len ~chunk ~positioned =
  let data = String.init len (byte ~file) in
  Eio.Switch.run @@ fun sw ->
  let f = Eio.Path.open_in ~sw Eio.Path.(dir / Fmt.str "f%d" file) in
  let size = Optint.Int63.to_int (Eio.File.size f) in
  if size <> len then Fmt.failwith "file %d: %d bytes, not %d" file size len;
  let buf = Cstruct.create chunk in
  let rec go got =
    let n =
      match
        if positioned then Eio.File.pread f ~file_offset:(int got) [ buf ]
        else Eio.Flow.single_read f buf
      with
      | n -> n
      | exception End_of_file -> 0
    in
    for i = 0 to n - 1 do
      if got + i >= len then
        Fmt.failwith "file %d: byte %d past its %d" file (got + i) len;
      if Cstruct.get_char buf i <> data.[got + i] then
        Fmt.failwith "file %d: byte %d is %C, not %C" file (got + i)
          (Cstruct.get_char buf i)
          data.[got + i]
    done;
    if n > 0 then go (got + n)
    else if got <> len then
      Fmt.failwith "file %d: %d of %d bytes read" file got len
  in
  go 0

(* [positioned] is false where the filesystem cannot read or write at an
   offset, as eio_windows 1.7: such a seed reads and writes in
   sequence only. *)
let program ~seed ~domain_mgr ~positioned dir =
  let rng = Random.State.make [| seed |] in
  let files = 1 + Random.State.int rng 4 in
  let draw () =
    ( 1 + Random.State.int rng 65536,
      1 + Random.State.int rng 8192,
      1 + Random.State.int rng 8192,
      Random.State.bool rng,
      Random.State.bool rng,
      Random.State.int rng 3 = 0 )
  in
  let plans = Array.init files (fun _ -> draw ()) in
  Eio.Fiber.List.iter
    (fun file ->
      let len, wchunk, rchunk, wpos, rpos, away = plans.(file) in
      let run () =
        write_file dir ~file ~len ~chunk:wchunk ~positioned:(positioned && wpos);
        read_file dir ~file ~len ~chunk:rchunk ~positioned:(positioned && rpos)
      in
      if away then Eio.Domain_manager.run domain_mgr run else run ())
    (List.init files Fun.id)

let seeds () =
  match Sys.getenv_opt "CANCEL_SEED" with
  | Some s -> [ int_of_string s ]
  | None ->
      let n =
        Option.fold ~none:16 ~some:int_of_string (Sys.getenv_opt "CANCEL_SEEDS")
      in
      List.init n Fun.id

let base env name ~seed : Eio.Fs.dir_ty Eio.Path.t =
  match name with
  | "host" ->
      let dir = Eio.Path.(Eio.Stdenv.cwd env / Fmt.str "seed-%d" seed) in
      Eio.Path.rmtree ~missing_ok:true dir;
      Eio.Path.mkdir ~perm:0o755 dir;
      (dir :> Eio.Fs.dir_ty Eio.Path.t)
  | _ -> (Eio_fs_mem.v () :> Eio.Fs.dir_ty Eio.Path.t)

let timeout () =
  Option.fold ~none:60. ~some:float_of_string (Sys.getenv_opt "CANCEL_TIMEOUT")

(* Whether [dir] reads and writes at an offset. *)
let positioned dir =
  let p = Eio.Path.(dir / "positioned") in
  match
    Eio.Switch.run @@ fun sw ->
    let f = Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) p in
    Eio.File.pwrite_all f ~file_offset:(int 0) [ Cstruct.of_string "x" ];
    ignore (Eio.File.pread f ~file_offset:(int 0) [ Cstruct.create 1 ] : int)
  with
  | () -> true
  | exception (Eio.Io _ | Unix.Unix_error _) -> false

let run_seed env ~name ~seed =
  let dir = base env name ~seed in
  let positioned = positioned dir in
  let wrapped = Chaos.wrap (plan seed) dir in
  match
    Eio.Time.with_timeout_exn env#clock (timeout ()) (fun () ->
        program ~seed ~domain_mgr:env#domain_mgr ~positioned wrapped)
  with
  | () ->
      if name = "host" then Eio.Path.rmtree dir;
      None
  | exception Eio.Time.Timeout ->
      Some (seed, Fmt.str "did not end within %g s" (timeout ()))
  | exception
      (( Failure _ | Invalid_argument _ | End_of_file | Eio.Io _
       | Eio.Exn.Multiple _ | Eio.Cancel.Cancelled _ ) as e) ->
      Some (seed, Printexc.to_string e)

let check name () =
  Eio_main.run @@ fun env ->
  let failed =
    List.filter_map (fun seed -> run_seed env ~name ~seed) (seeds ())
  in
  if failed <> [] then
    Alcotest.failf "@[<v>%a@]"
      Fmt.(
        list ~sep:cut (fun ppf (seed, e) ->
            pf ppf "seed %d: %s (replay: CANCEL_SEED=%d)" seed e seed))
      failed

let suite =
  ( "cancel",
    [
      Alcotest.test_case "the host's filesystem" `Quick (check "host");
      Alcotest.test_case "eio-fs.mem" `Quick (check "eio-fs.mem");
    ] )

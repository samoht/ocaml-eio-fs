(* (b) Content-addressed blobs, written by temp-and-rename, and an index
   file of the hashes, one per line, appended and synced. *)
module P = Eio.Path

let hash s = Digest.to_hex (Digest.string s)

let put ?(bug = `None) dir data =
  let h = hash data in
  let blobs = P.(dir / "blobs") in
  let tmp = P.(blobs / (h ^ ".tmp")) in
  P.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
      Eio.Flow.copy_string data f;
      if bug <> `No_blob_sync then Eio.File.sync f);
  P.rename tmp P.(blobs / h);
  if bug <> `No_dir_sync then Eio_fs_sync.sync_dir blobs;
  P.with_open_out ~append:true ~create:(`If_missing 0o644)
    P.(dir / "index")
    (fun f ->
      Eio.Flow.copy_string (h ^ "\n") f;
      Eio.File.sync f);
  h

(* The complete lines of the index that are hashes. A newline is no commit
   mark: a line may end with one and hold zeros where a sector the file had
   nothing of was not written yet. *)
let is_hash h =
  String.length h = 32
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) h

let index ?(bug = `None) dir =
  let s =
    try P.load P.(dir / "index")
    with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ""
  in
  let lines = String.split_on_char '\n' s in
  let complete =
    match List.rev lines with _partial :: rest -> List.rev rest | [] -> []
  in
  if bug = `Partial_line then List.filter (( <> ) "") lines
  else List.filter is_hash complete

let get dir h = P.load P.(dir / "blobs" / h)

let setup env =
  P.mkdir ~perm:0o755 P.(env#fs / "blobs");
  P.save ~create:(`Exclusive 0o644) P.(env#fs / "index") ""

let blobs =
  [ "alpha"; String.make 3000 'b'; "gamma, a third blob"; String.make 5000 'd' ]

let test ?put_bug ?index_bug () =
  Eio_fs.Chaos.Crash.check ~setup
    (fun ~ack env ->
      try List.iter (fun b -> ack (put ?bug:put_bug env#fs b)) blobs
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if not (P.is_directory P.(env#fs / "blobs")) then
        failwith "blobs/ is gone";
      let idx = index ?bug:index_bug env#fs in
      List.iter
        (fun h ->
          match get env#fs h with
          | b when hash b = h -> ()
          | b -> Fmt.failwith "blob %s holds %d wrong bytes" h (String.length b)
          | exception Eio.Io _ ->
              Fmt.failwith "index names %s, which is missing" h)
        idx;
      List.iter
        (fun h ->
          if not (List.mem h idx) then Fmt.failwith "acked %s is not indexed" h)
        acked)

(* A recover that checks only that indexed blobs exist. *)
let test_weak ?put_bug () =
  Eio_fs.Chaos.Crash.check ~setup
    (fun ~ack env ->
      try List.iter (fun b -> ack (put ?bug:put_bug env#fs b)) blobs
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if not (P.is_directory P.(env#fs / "blobs")) then
        failwith "blobs/ is gone";
      let idx = index env#fs in
      List.iter
        (fun h ->
          if not (P.is_file P.(env#fs / "blobs" / h)) then
            Fmt.failwith "missing %s" h)
        idx;
      List.iter
        (fun h ->
          if not (List.mem h idx) then Fmt.failwith "acked %s is not indexed" h)
        acked)

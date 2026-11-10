(* Programs under test. Each takes a [bug] so the tests can show they catch
   the mistake. *)

module P = Eio.Path

(* (a) Atomic save: temp file, fsync, rename, fsync the directory. *)
let save_config ?(bug = `None) dir contents =
  let tmp = P.(dir / "config.tmp") in
  P.with_open_out ~create:(`Or_truncate 0o644) tmp (fun f ->
      Eio.Flow.copy_string contents f;
      if bug <> `No_file_sync then Eio.File.sync f);
  P.rename tmp P.(dir / "config");
  if bug <> `No_dir_sync then Eio_fs_sync.sync_dir dir

let load_config dir = P.load P.(dir / "config")

(* (b) Append-only log: [len:4][sum:4][payload] per record. *)
module Log = struct
  let sum s =
    let a = ref 1 and b = ref 0 in
    String.iter
      (fun c ->
        a := (!a + Char.code c) mod 65521;
        b := (!b + !a) mod 65521)
      s;
    (!b lsl 16) lor !a

  let encode payload =
    let n = String.length payload in
    let b = Bytes.create (9 + n) in
    Bytes.set_int32_be b 0 (Int32.of_int n);
    Bytes.set_int32_be b 4 (Int32.of_int (sum payload));
    Bytes.blit_string payload 0 b 8 n;
    Bytes.set b (8 + n) '\255';
    Bytes.to_string b

  let open_ ~sw dir =
    let f =
      P.open_out ~sw ~append:true ~create:(`If_missing 0o644) P.(dir / "log")
    in
    Eio_fs_sync.sync_dir dir;
    f

  (* Returns once the record is durable. *)
  let append ?(bug = `None) f payload =
    Eio.Flow.copy_string (encode payload) f;
    match bug with
    | `No_sync -> ()
    | `Retry_sync -> ( try Eio.File.sync f with Eio.Io _ -> Eio.File.sync f)
    | _ -> Eio.File.sync f

  (* The longest valid prefix of records. *)
  let recover ?(bug = `None) dir =
    match P.load P.(dir / "log") with
    | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> []
    | s ->
        let len = String.length s in
        let rec go off acc =
          if off + 8 > len then List.rev acc
          else
            let n = Int32.to_int (String.get_int32_be s off) in
            let k =
              Int32.to_int (String.get_int32_be s (off + 4)) land 0xffffffff
            in
            if n < 0 || off + 9 + n > len then List.rev acc
            else
              let p = String.sub s (off + 8) n in
              let ok =
                match bug with
                | `No_checksum -> true
                | `Marker_only -> s.[off + 8 + n] = '\255'
                | _ -> sum p = k
              in
              if ok then go (off + 9 + n) (p :: acc) else List.rev acc
        in
        go 0 []
end

(* (c) A cache that must work on a read-only or locked-down filesystem: any
   error on the cache only costs a recomputation. An entry ends with ['$']. *)
module Cache = struct
  let soft ~bug f =
    match f () with
    | v -> Some v
    | exception
        Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _ | Eio.Fs.Not_found _), _)
      ->
        None
    | exception Eio.Io _ when bug <> `Only_eacces -> None

  let get ?(bug = `None) dir key ~compute =
    let path = P.(dir / key) in
    let valid v = String.length v > 0 && v.[String.length v - 1] = '$' in
    match soft ~bug (fun () -> P.load path) with
    | Some v when bug = `Direct_write -> v
    | Some v when valid v -> String.sub v 0 (String.length v - 1)
    | _ ->
        let v = compute key in
        let entry = if bug = `Direct_write then v else v ^ "$" in
        ignore
          (soft ~bug (fun () ->
               if bug = `Direct_write then
                 P.save ~create:(`Or_truncate 0o644) path entry
               else
                 let tmp = P.(dir / (key ^ ".tmp")) in
                 P.save ~create:(`Or_truncate 0o644) tmp entry;
                 P.rename tmp path));
        v
end

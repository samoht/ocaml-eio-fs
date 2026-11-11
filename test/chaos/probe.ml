module P = Eio.Path

(* Guard: a convergent "apply this config" step whose value setup already
   holds. With [`In_place], it rewrites the file in place. *)
let guard_idempotent ?bug () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "conf") "v1")
    (fun ~ack env ->
      try
        (match bug with
        | None -> Eio_fs_sync.save P.(env#fs / "conf") "v1"
        | Some `In_place ->
            P.with_open_out ~create:(`Or_truncate 0o644)
              P.(env#fs / "conf")
              (fun f ->
                Eio.Flow.copy_string "v1" f;
                Eio.File.sync f));
        ack "v1"
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let v = P.load P.(env#fs / "conf") in
      match List.rev acked with
      | last :: _ when v <> last -> Fmt.failwith "conf is %S, acked %S" v last
      | _ -> if v <> "v1" then Fmt.failwith "conf is %S" v)

(* Sector atomicity: a 16-byte header written by two pwrites in one sector,
   then synced. A nonzero length with no magic is a state a disk cannot
   produce. *)
let sector () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env ->
      Eio_fs_sync.save P.(env#fs / "hdr") (String.make 16 '\000'))
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let f = P.open_out ~sw ~create:(`If_missing 0o644) P.(env#fs / "hdr") in
        Eio.File.pwrite_all f ~file_offset:(Optint.Int63.of_int 0)
          [ Cstruct.of_string "MAGIC\000\000\000" ];
        Eio.File.pwrite_all f ~file_offset:(Optint.Int63.of_int 8)
          [ Cstruct.of_string "\000\000\000\042\000\000\000\000" ];
        Eio.File.sync f;
        ack ()
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let s = P.load P.(env#fs / "hdr") in
      let magic = String.sub s 0 5 = "MAGIC"
      and len = String.get_int32_be s 8 in
      if len <> 0l && not magic then
        failwith "length without magic: impossible on a sector-atomic disk";
      if acked <> [] && not (magic && len = 42l) then
        failwith "acked header lost")

(* TOCTOU: check the path is a regular file, then write it. [check] does not
   swap a stat's path for a link, so it passes [toctou ~bug:true]; the swap
   is asked for with [wrap]. *)
let toctou_setup env =
  P.mkdir ~perm:0o755 P.(env#fs / "secret");
  P.save ~create:(`Exclusive 0o600) P.(env#fs / "secret" / "key") "k";
  P.save ~create:(`Exclusive 0o644) P.(env#fs / "out") ""

let write_out ~bug fs =
  let out = P.(fs / "out") in
  try
    if P.kind ~follow:false out = `Regular_file then
      if bug then P.save ~create:(`Or_truncate 0o644) out "data"
      else
        (* Open without following, and check the open file. *)
        P.with_open_out ~follow:false ~create:(`Or_truncate 0o644) out (fun f ->
            Eio.Flow.copy_string "data" f)
  with Eio.Io _ -> ()

let toctou ?(bug = true) () =
  Eio_fs.Chaos.check ~setup:toctou_setup (fun env ->
      write_out ~bug env#fs;
      try
        if P.load P.(env#fs / "secret" / "key") <> "k" then
          failwith "wrote through a swapped link"
      with Eio.Io _ -> ())

(* The same with the swap asked for: the stat's path becomes a link to the
   key. *)
let toctou_swapped ?(bug = true) () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  toctou_setup
    object
      method fs = (fs :> Eio.Fs.dir_ty P.t)
    end;
  let swap (op : Eio_fs.Chaos.Op.t) : Eio_fs.Chaos.Fault.t option =
    if op.kind = `Stat && op.path = "/out" then Some (Swap_symlink "secret/key")
    else None
  in
  write_out ~bug (Eio_fs.Chaos.wrap swap fs);
  if P.load P.(fs / "secret" / "key") <> "k" then
    failwith "wrote through a swapped link"

(* Reinitialise on any read error: a transient EIO wipes the data. Only
   [check] can see it; [Crash.check] never fails a read. *)
let reinit_crash () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "db") "precious")
    (fun ~ack:_ env ->
      try
        let v =
          try P.load P.(env#fs / "db")
          with Eio.Io _ ->
            Eio_fs_sync.save P.(env#fs / "db") "empty";
            "empty"
        in
        ignore v
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      if P.load P.(env#fs / "db") <> "precious" then failwith "data wiped")

let reinit_check () =
  Eio_fs.Chaos.check
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "db") "precious")
    (fun env ->
      (try
         ignore
           (try P.load P.(env#fs / "db")
            with Eio.Io _ ->
              Eio_fs_sync.save P.(env#fs / "db") "empty";
              "empty")
       with Eio.Io _ -> ());
      match P.load P.(env#fs / "db") with
      | "precious" -> ()
      | v -> Fmt.failwith "data is %S" v
      | exception Eio.Io _ -> ())

(* A file in a new subdirectory: the file and the subdirectory are synced,
   the root is not, so the subdirectory's entry is not durable. *)
let mkdir_no_parent_sync () =
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      try
        P.mkdir ~perm:0o755 P.(env#fs / "data");
        Eio_fs_sync.save P.(env#fs / "data" / "f") "x";
        ack ()
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if
        acked <> []
        && try P.load P.(env#fs / "data" / "f") <> "x" with Eio.Io _ -> true
      then failwith "lost f")

(* A long log of records of varying lengths, synced one by one: its length
   classes, not its records, make the linear tier. *)
let log_varying n () =
  let rec_ i = String.make (1 + (i * 7 mod 61)) (Char.chr (97 + (i mod 26))) in
  let encode r = Fmt.str "%04d%s" (String.length r) r in
  let rec parse s off acc =
    if off + 4 > String.length s then List.rev acc
    else
      match int_of_string_opt (String.sub s off 4) with
      | Some n when n > 0 && off + 4 + n <= String.length s ->
          let r = String.sub s (off + 4) n in
          if String.for_all (fun c -> c = r.[0]) r then
            parse s (off + 4 + n) (r :: acc)
          else List.rev acc
      | _ -> List.rev acc
  in
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let f =
          P.open_out ~sw ~append:true ~create:(`If_missing 0o644)
            P.(env#fs / "log")
        in
        Eio_fs_sync.sync_dir env#fs;
        for i = 1 to n do
          Eio.Flow.copy_string (encode (rec_ i)) f;
          Eio.File.sync f;
          ack i
        done
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let s =
        try P.load P.(env#fs / "log")
        with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ""
      in
      if List.length (parse s 0 []) < List.length acked then
        failwith "lost records")

(* Budget: [n] synced appends; recover compacts the log into a new file. *)
let log_budget n () =
  let rec_ i = Fmt.str "r%04d\n" i in
  let parse s =
    String.split_on_char '\n' s |> List.filter (fun l -> String.length l = 5)
  in
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let f =
          P.open_out ~sw ~append:true ~create:(`If_missing 0o644)
            P.(env#fs / "log")
        in
        Eio_fs_sync.sync_dir env#fs;
        for i = 1 to n do
          Eio.Flow.copy_string (rec_ i) f;
          Eio.File.sync f;
          ack i
        done
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let s =
        try P.load P.(env#fs / "log")
        with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ""
      in
      let recs = parse s in
      Eio_fs_sync.save
        P.(env#fs / "log")
        (String.concat "" (List.map (fun r -> r ^ "\n") recs));
      if List.length recs < List.length acked then failwith "lost records")

(* A file of 64-byte records, each one pwrite and a sync, and a truncation
   back when one fails: a crash leaves a length the file had, so a whole
   number of records. *)
let fixed_records () =
  let record i = Fmt.str "rec%04d:%s" i (String.make 56 'x') in
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> Eio_fs_sync.save P.(env#fs / "hdr") "v1")
    (fun ~ack env ->
      try
        P.with_open_out ~create:(`Exclusive 0o644)
          P.(env#fs / "log")
          (fun f ->
            Eio_fs_sync.sync_dir env#fs;
            for i = 0 to 2 do
              (* A record that fails is cut away again. *)
              (try
                 Eio.File.pwrite_all f
                   ~file_offset:(Optint.Int63.of_int (i * 64))
                   [ Cstruct.of_string (record i) ];
                 Eio.File.sync f
               with Eio.Io _ as e ->
                 Eio.File.truncate f (Optint.Int63.of_int (i * 64));
                 Eio.File.sync f;
                 raise e);
              ack i
            done)
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      if P.load P.(env#fs / "hdr") <> "v1" then failwith "hdr";
      match P.load P.(env#fs / "log") with
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
          if acked <> [] then failwith "log lost"
      | s ->
          let n = String.length s in
          if n mod 64 <> 0 then Fmt.failwith "log holds %d bytes" n;
          List.iter
            (fun i ->
              if n < (i + 1) * 64 || String.sub s (i * 64) 64 <> record i then
                Fmt.failwith "record %d lost" i)
            acked)

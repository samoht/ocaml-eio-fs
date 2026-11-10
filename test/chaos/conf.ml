(* (e) Two config files, a.conf and b.conf, that must carry the same
   generation. Correct: write both under a new generation's names, sync,
   then switch a CURRENT pointer by temp-and-rename, then delete the old
   generation. *)
module P = Eio.Path

let write_sync ?(sync = true) dir name s =
  P.with_open_out ~create:(`Or_truncate 0o644)
    P.(dir / name)
    (fun f ->
      Eio.Flow.copy_string s f;
      if sync then Eio.File.sync f)

let content file g = Fmt.str "%s generation=%d\n" file g

let switch ?(bug = `None) dir g =
  write_sync dir "CURRENT.tmp" (string_of_int g);
  P.rename P.(dir / "CURRENT.tmp") P.(dir / "CURRENT");
  if bug <> `No_switch_sync then Eio_fs_sync.sync_dir dir

let update ?(bug = `None) dir ~old g =
  match bug with
  | `In_place ->
      (* Two atomic renames are not one. *)
      write_sync dir "a.tmp" (content "a" g);
      write_sync dir "b.tmp" (content "b" g);
      P.rename P.(dir / "a.tmp") P.(dir / "a.conf");
      P.rename P.(dir / "b.tmp") P.(dir / "b.conf");
      Eio_fs_sync.sync_dir dir
  | _ ->
      let sync = bug <> `No_data_sync in
      write_sync ~sync dir (Fmt.str "a.%d" g) (content "a" g);
      write_sync ~sync dir (Fmt.str "b.%d" g) (content "b" g);
      if bug = `Delete_first then (
        P.unlink P.(dir / Fmt.str "a.%d" old);
        P.unlink P.(dir / Fmt.str "b.%d" old));
      Eio_fs_sync.sync_dir dir;
      switch ~bug dir g;
      if bug <> `Delete_first then (
        P.unlink P.(dir / Fmt.str "a.%d" old);
        P.unlink P.(dir / Fmt.str "b.%d" old);
        if bug <> `No_switch_sync then Eio_fs_sync.sync_dir dir)

let load ?(bug = `None) dir =
  match bug with
  | `In_place -> (P.load P.(dir / "a.conf"), P.load P.(dir / "b.conf"))
  | _ ->
      let g = int_of_string (String.trim (P.load P.(dir / "CURRENT"))) in
      (P.load P.(dir / Fmt.str "a.%d" g), P.load P.(dir / Fmt.str "b.%d" g))

let gen_of s = Scanf.sscanf s "%_s generation=%d" Fun.id

let setup ?(bug = `None) env =
  let dir = env#fs in
  match bug with
  | `In_place ->
      write_sync dir "a.conf" (content "a" 1);
      write_sync dir "b.conf" (content "b" 1)
  | _ ->
      write_sync dir "a.1" (content "a" 1);
      write_sync dir "b.1" (content "b" 1);
      write_sync dir "CURRENT" "1"

let test ?bug () =
  Eio_fs.Chaos.Crash.check ~setup:(setup ?bug)
    (fun ~ack env ->
      try
        for g = 2 to 3 do
          update ?bug env#fs ~old:(g - 1) g;
          ack g
        done
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let a, b = load ?bug env#fs in
      let ga = gen_of a and gb = gen_of b in
      if ga <> gb then Fmt.failwith "a is at %d, b at %d" ga gb;
      let last = List.fold_left max 1 acked in
      if ga < last then Fmt.failwith "at %d, but %d was acked" ga last)

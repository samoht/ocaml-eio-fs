(* The first review's tests: a larger store, recovery that compacts, and a
   retry path a second fault breaks. *)

let first = [ ("k0", "s0"); ("k1", "s1") ]

let setup_store env =
  Eio.Switch.run @@ fun sw ->
  let db = Kv.open_ ~sw env#fs in
  List.iter (fun (k, v) -> Kv.put db k v) first

let check_first env =
  Eio.Switch.run @@ fun sw ->
  let db = Kv.open_ ~sw env#fs in
  List.iter
    (fun (k, v) ->
      if Kv.get db k <> Some v then Fmt.failwith "%s lost by a recovery" k)
    first;
  db

(* A recovery that compacts what setup left, acknowledged: the check refuses
   it, as every acknowledged value already holds after setup. *)
let kv_reopen ?bug () =
  Eio_fs.Chaos.Crash.check ~setup:setup_store
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        ignore (Kv.open_ ?bug ~sw env#fs);
        ack ()
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env -> ignore (check_first env))

(* The same, with a fresh put acknowledged. *)
let kv_reopen2 ?bug () =
  Eio_fs.Chaos.Crash.check ~setup:setup_store
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let db = Kv.open_ ?bug ~sw env#fs in
        Kv.put db "k9" "new";
        ack ()
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let db = check_first env in
      if acked <> [] && Kv.get db "k9" <> Some "new" then failwith "k9 lost")

(* The same with no ack: recover checks what setup made. *)
let kv_reopen3 ?bug () =
  Eio_fs.Chaos.Crash.check ~setup:setup_store
    (fun ~ack:_ env ->
      try Eio.Switch.run @@ fun sw -> ignore (Kv.open_ ?bug ~sw env#fs)
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env -> ignore (check_first env))

(* A larger store: 32 puts, one compaction at the 30th. *)
let big_puts =
  List.init 32 (fun i -> (Fmt.str "k%d" (i mod 5), Fmt.str "v%d" i))

let big ?bug () =
  Kv.every := 30;
  Fun.protect ~finally:(fun () -> Kv.every := 4) @@ fun () ->
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let db = Kv.open_ ?bug ~sw env#fs in
        List.iter
          (fun (k, v) ->
            Kv.put db k v;
            ack (k, v))
          big_puts
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env -> After.recover_kv ~puts:big_puts ~acked env#fs)

(* The retry bug, pinned with two faults through [wrap]: the first write
   fails, and the retry's first write moves three bytes. *)
let retry_pinned ?bug () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  let faults (op : Eio_fs.Chaos.Op.t) : Eio_fs.Chaos.Fault.t option =
    match (op.kind, op.nth) with
    | `Write, 1 -> Some (Fail `Eio)
    | `Write, 2 -> Some (Short 3)
    | _ -> None
  in
  Retry.save ?bug (Eio_fs.Chaos.wrap faults fs) After.value;
  let v = Retry.load fs in
  if v <> After.value then Fmt.failwith "saved %S" v

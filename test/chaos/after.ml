(* The design's tests, written with the API they propose. *)

(* (a) An atomic save of a config. *)
let test_a ?replay ~save () =
  Eio_fs.Chaos.Crash.check ?replay
    ~setup:(fun env -> save env#fs "old")
    (fun ~ack env ->
      match save env#fs "new" with () -> ack () | exception Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      match (App.load_config env#fs, acked) with
      | "new", _ | "old", [] -> ()
      | v, _ -> Fmt.failwith "config is %S" v)

(* A recover that ignores [acked]: the check must refuse it. *)
let test_a_weak ~save () =
  Eio_fs.Chaos.Crash.check
    ~setup:(fun env -> save env#fs "old")
    (fun ~ack env ->
      match save env#fs "new" with () -> ack () | exception Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      match App.load_config env#fs with
      | "new" | "old" -> ()
      | v -> Fmt.failwith "config is %S" v)

(* (b) An append-only log of checksummed records. *)
let records = List.init 6 (Fmt.str "record-%d")

let rec is_prefix p l =
  match (p, l) with
  | [], _ -> true
  | x :: p, y :: l -> x = y && is_prefix p l
  | _ -> false

let test_b ?budget ?replay ~append ~recover () =
  Eio_fs.Chaos.Crash.check ?budget ?replay
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let log = App.Log.open_ ~sw env#fs in
        List.iter
          (fun r ->
            append log r;
            ack r)
          records
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env ->
      let found = recover env#fs in
      if not (is_prefix acked found && is_prefix found records) then
        Fmt.failwith "acked %a, recovered %a"
          Fmt.Dump.(list string)
          acked
          Fmt.Dump.(list string)
          found)

(* (c) A cache that survives any filesystem error. *)
let compute k = "value of " ^ k

let test_c ?replay ~get () =
  Eio_fs.Chaos.check ?replay (fun env ->
      for _ = 1 to 2 do
        let v = get env#fs "k" ~compute in
        if v <> compute "k" then Fmt.failwith "got %S" v
      done)

(* A key-value store with a write-ahead log and compaction. *)
let puts = List.init 12 (fun i -> (Fmt.str "k%d" (i mod 5), Fmt.str "v%d" i))
let keys = List.sort_uniq compare (List.map fst puts)

(* Every acked put is visible, or the next put of its key. *)
let recover_kv ?bug ~puts ~acked dir =
  Eio.Switch.run @@ fun sw ->
  let db = Kv.open_ ?bug ~sw dir in
  let next = List.nth_opt puts (List.length acked) in
  List.iter
    (fun k ->
      let ok =
        List.assoc_opt k (List.rev acked)
        :: (match next with Some (k', v) when k' = k -> [ Some v ] | _ -> [])
      in
      if not (List.mem (Kv.get db k) ok) then Fmt.failwith "%s lost" k)
    keys

let test_kv ?bug () =
  Eio_fs.Chaos.Crash.check
    (fun ~ack env ->
      try
        Eio.Switch.run @@ fun sw ->
        let db = Kv.open_ ?bug ~sw env#fs in
        List.iter
          (fun (k, v) ->
            Kv.put db k v;
            ack (k, v))
          puts
      with Eio.Io _ -> ())
    ~recover:(fun ~acked env -> recover_kv ?bug ~puts ~acked env#fs)

(* A save that retries a failed write once, in place. *)
let value = "a value of some length"

let test_retry ?bug () =
  Eio_fs.Chaos.check (fun env ->
      match Retry.save ?bug env#fs value with
      | exception Eio.Io _ -> ()
      | () -> (
          match Retry.load env#fs with
          | v -> if v <> value then Fmt.failwith "saved %S" v
          | exception Eio.Io _ -> ()))

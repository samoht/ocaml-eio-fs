(* The states a cut may leave, as the sampled tier reaches them. *)

module P = Eio.Path

(* A file [keep] setup made, so recover has something to check. *)
let setup env = Eio_fs_sync.save P.(env#fs / "keep") "k"
let keep env = if P.load P.(env#fs / "keep") <> "k" then failwith "keep lost"

(* A new file whose first sector is stale and whose others hold the write:
   the cut's first state drops the file, so its draws say little of how many
   states the cut allows. *)
let stale_head () =
  let a = String.make 1100 'A' in
  let bad = String.make 512 '7' ^ String.sub a 512 588 in
  Eio_fs.Chaos.Crash.check ~setup
    (fun ~ack:_ env ->
      try P.save ~create:(`Exclusive 0o644) P.(env#fs / "f") a
      with Eio.Io _ -> ())
    ~recover:(fun ~acked:_ env ->
      keep env;
      match P.load P.(env#fs / "f") with
      | s when s = bad -> failwith "stale head"
      | _ | (exception Eio.Io _) -> ())

let suite =
  ( "states",
    [
      Alcotest.test_case "a stale sector of a new file is reached" `Quick
        (fun () ->
          let m = Expect.report stale_head in
          Alcotest.(check string)
            "error" "Failure(\"stale head\")" (Expect.field "error" m));
    ] )

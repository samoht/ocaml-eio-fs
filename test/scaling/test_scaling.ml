(* Each bound is processor time, not wall time, and sits about a hundred
   times above what the linear code takes, so only a cost that grows with
   the square of the size can reach it. *)

let ( / ) = Eio.Path.( / )

let within ~seconds what fn =
  let t0 = Sys.time () in
  fn ();
  let dt = Sys.time () -. t0 in
  if dt > seconds then
    Alcotest.failf "%s took %.1fs of processor time, over its %.0fs bound" what
      dt seconds

(* 80k creates in one directory: a list of entries made each one a scan. *)
let test_one_directory () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  within ~seconds:10. "80k creates and stats in one directory" (fun () ->
      for i = 1 to 80_000 do
        Eio.Path.save ~create:(`Exclusive 0o644) (fs / string_of_int i) "x"
      done;
      for i = 1 to 80_000 do
        ignore (Eio.Path.stat ~follow:false (fs / string_of_int i))
      done)

let suite =
  ("scaling", [ Alcotest.test_case "one directory" `Slow test_one_directory ])

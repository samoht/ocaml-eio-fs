(* 80k creates and stats in one directory, within a processor-time bound a
   hundred times above what the map of entries takes: a list of entries made
   each one a scan, 10^10 steps. Run with [dune build @scaling]. *)

let ( / ) = Eio.Path.( / )

let () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Eio_fs.Fs.v () in
  let t0 = Sys.time () in
  for i = 1 to 80_000 do
    Eio.Path.save ~create:(`Exclusive 0o644) (fs / string_of_int i) "x"
  done;
  for i = 1 to 80_000 do
    ignore (Eio.Path.stat ~follow:false (fs / string_of_int i))
  done;
  let dt = Sys.time () -. t0 in
  if dt > 10. then begin
    Printf.eprintf
      "80k creates and stats in one directory took %.1fs, over 10s\n" dt;
    exit 1
  end

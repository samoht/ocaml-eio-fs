let ( / ) = Eio.Path.( / )

(* [Assets] is generated from the fixture tree by the eio-fs-crunch binary at
   build time (see the dune rule), so this test fails if the generator's
   output stops compiling or stops rebuilding the tree faithfully. *)
let test_load () =
  Eio_mock.Backend.run @@ fun () ->
  let fs = Assets.load () in
  Alcotest.(check string)
    "file contents" "Hello, crunch!\n"
    (Eio.Path.load (fs / "hello.txt"));
  Alcotest.(check string)
    "nested contents" "nested\n"
    (Eio.Path.load (fs / "sub/nested.txt"));
  Alcotest.(check (list string))
    "root listing" [ "hello.txt"; "sub" ] (Eio.Path.read_dir fs)

let () =
  Alcotest.run "eio-fs-crunch"
    [ ("assets", [ Alcotest.test_case "load" `Quick test_load ]) ]

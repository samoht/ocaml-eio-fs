(* [Eio_fs.Nofollow] over a real POSIX directory, the in-memory backend and
   the [Chaos] wrapper: the final component is never followed, the bound and
   the validation hold before a byte is returned, and a failed read wipes its
   buffer. *)

module Chaos = Eio_fs.Chaos

let ( / ) = Eio.Path.( / )

let with_real_dir fn =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Filename.temp_file "eio-fs-nofollow" "" in
  Sys.remove dir;
  Eio.Path.mkdir ~perm:0o755 Eio.Path.(fs / dir);
  Fun.protect ~finally:(fun () ->
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / dir))
  @@ fun () -> fn (Eio.Path.(fs / dir) :> Eio.Fs.dir_ty Eio.Path.t)

let with_in_memory_fs fn =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env -> fn env#fs

let regular_file (stat : Eio.File.Stat.t) =
  match stat.kind with
  | `Regular_file -> Ok ()
  | _ -> Error "not a regular file"

exception Cancelled_read

let test_read_nofollow_reads_a_bounded_regular_file () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  match
    Eio_fs.Nofollow.read_file ~max_bytes:6 ~validate:regular_file
      (dir / "private")
  with
  | Error _ -> Alcotest.fail "a private regular file was refused"
  | Ok bytes ->
      Alcotest.(check string)
        "descriptor bytes" "secret" (Cstruct.to_string bytes);
      Cstruct.memset bytes 0

let test_read_nofollow_refuses_the_final_link () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "target") "target";
  Eio.Path.symlink ~link_to:"target" (dir / "private");
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (dir / "private")
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "a final-component symlink was read"
  | Error _ -> Alcotest.fail "a symlink reached validation instead of open"
  | exception Eio.Io _ -> ()

let test_read_nofollow_enforces_the_bound () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "three";
  match
    Eio_fs.Nofollow.read_file ~max_bytes:2 ~validate:regular_file
      (dir / "private")
  with
  | Error `Too_large -> ()
  | Error (`Invalid reason) -> Alcotest.fail reason
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "an over-bound file was returned"

let test_read_nofollow_returns_descriptor_validation () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  match
    Eio_fs.Nofollow.read_file ~max_bytes:6
      ~validate:(fun _ -> Error "descriptor policy")
      (dir / "private")
  with
  | Error (`Invalid reason) ->
      Alcotest.(check string) "validation reason" "descriptor policy" reason
  | Error `Too_large -> Alcotest.fail "validation was skipped for the bound"
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "descriptor validation was ignored"

let test_read_nofollow_uses_the_in_memory_backend () =
  with_in_memory_fs @@ fun fs ->
  Eio.Path.save ~create:(`Exclusive 0o600) (fs / "target") "target";
  Eio.Path.symlink ~link_to:"target" (fs / "private");
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (fs / "private")
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "the in-memory backend followed a final link"
  | Error _ -> Alcotest.fail "an in-memory link reached validation"
  | exception Eio.Io _ -> ()

let test_read_nofollow_is_one_open_through_its_wrapper () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  Eio.Path.with_open_dir dir @@ fun base ->
  let observed = ref [] in
  let schedule =
    Chaos.Schedule.observing
      (fun operation -> observed := operation :: !observed)
      Chaos.Schedule.empty
  in
  let wrapped = Chaos.wrap schedule base in
  (match
     Eio_fs.Nofollow.read_file ~max_bytes:6 ~validate:regular_file
       (wrapped / "private")
   with
  | Error _ -> Alcotest.fail "the wrapped private file was refused"
  | Ok bytes -> Cstruct.memset bytes 0);
  let opens =
    List.filter
      (fun (operation : Chaos.Op.t) -> operation.kind = `Open)
      !observed
  in
  Alcotest.(check int)
    "the supplied wrapper observes the one open" 1 (List.length opens)

let test_read_nofollow_refuses_the_final_link_beneath_a_capability () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "target") "target";
  Eio.Path.symlink ~link_to:"target" (dir / "private");
  Eio.Path.with_open_dir dir @@ fun confined ->
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (confined / "private")
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "a final-component symlink was read beneath a capability"
  | Error _ -> Alcotest.fail "a symlink reached validation instead of open"
  | exception Eio.Io _ -> ()

(* A FIFO with no writer: the open returns and validation refuses it, where a
   blocking open or a read before validation would wait on a writer forever. *)
let test_read_nofollow_validates_a_fifo_before_reading () =
  with_real_dir @@ fun dir ->
  Unix.mkfifo (snd (dir / "private")) 0o600;
  match
    Eio_fs.Nofollow.read_file ~max_bytes:16 ~validate:regular_file
      (dir / "private")
  with
  | Error (`Invalid reason) ->
      Alcotest.(check string) "validation reason" "not a regular file" reason
  | Error `Too_large -> Alcotest.fail "a FIFO was read up to the bound"
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "a FIFO was read"

let test_read_nofollow_wipes_a_failed_read () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "private") "secret";
  let stat = Eio.Path.stat ~follow:false (dir / "private") in
  let exposed = ref None in
  (match
     Eio_fs.Nofollow.run ~max_bytes:6 ~validate:regular_file ~stat
       ~read:(fun buffer ->
         Cstruct.blit_from_string "secret" 0 buffer 0 6;
         exposed := Some buffer;
         raise Cancelled_read)
   with
  | exception Cancelled_read -> ()
  | exception failure -> raise failure
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "the failed read returned bytes"
  | Error _ -> Alcotest.fail "the fixture did not enter its read");
  match !exposed with
  | None -> Alcotest.fail "the fixture exposed no partial buffer"
  | Some buffer ->
      let wiped = Cstruct.to_string buffer in
      Alcotest.(check string)
        "every byte read before cancellation is wiped"
        (String.make (String.length wiped) '\000')
        wiped

(* The bound is what a file may hold, not what a read costs. A caller bounds an
   untrusted file generously, and a six-byte file read under a one-mebibyte
   bound must not take a mebibyte for it: the buffer offered is the size the
   opened descriptor reports, plus the one byte that tells growth from the end.
   An implementation that allocates the bound up front offers 1048577 here. *)
let test_read_nofollow_costs_the_file_not_the_bound () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "small") "secret";
  let stat = Eio.Path.stat ~follow:false (dir / "small") in
  let offered = ref [] in
  let remaining = ref "secret" in
  let read buffer =
    offered := Cstruct.length buffer :: !offered;
    let n = min (Cstruct.length buffer) (String.length !remaining) in
    Cstruct.blit_from_string !remaining 0 buffer 0 n;
    remaining := String.sub !remaining n (String.length !remaining - n);
    n
  in
  (match
     Eio_fs.Nofollow.run ~max_bytes:(1024 * 1024) ~validate:regular_file ~stat
       ~read
   with
  | Ok bytes ->
      Alcotest.(check string)
        "the small file is read whole" "secret" (Cstruct.to_string bytes)
  | Error _ -> Alcotest.fail "the small file was refused");
  Alcotest.(check int)
    "the largest buffer offered is the file's size plus one" 7
    (List.fold_left max 0 !offered)

(* A file that grows past the size its descriptor reported is still read to the
   bound, so sizing the buffer from the descriptor loses no byte the bound
   admits, and a file growing past the bound is still refused. *)
let test_read_nofollow_follows_growth_to_the_bound () =
  with_real_dir @@ fun dir ->
  Eio.Path.save ~create:(`Exclusive 0o600) (dir / "growing") "ab";
  let stat = Eio.Path.stat ~follow:false (dir / "growing") in
  let feed contents =
    let remaining = ref contents in
    fun buffer ->
      let n = min (Cstruct.length buffer) (String.length !remaining) in
      Cstruct.blit_from_string !remaining 0 buffer 0 n;
      remaining := String.sub !remaining n (String.length !remaining - n);
      n
  in
  let grown = String.make 100 'g' in
  (match
     Eio_fs.Nofollow.run ~max_bytes:100 ~validate:regular_file ~stat
       ~read:(feed grown)
   with
  | Ok bytes ->
      Alcotest.(check string)
        "growth up to the bound is read" grown (Cstruct.to_string bytes)
  | Error _ -> Alcotest.fail "growth within the bound was refused");
  match
    Eio_fs.Nofollow.run ~max_bytes:100 ~validate:regular_file ~stat
      ~read:(feed (grown ^ "!"))
  with
  | Ok bytes ->
      Cstruct.memset bytes 0;
      Alcotest.fail "growth past the bound was read"
  | Error `Too_large -> ()
  | Error (`Invalid reason) -> Alcotest.failf "growth was invalid: %s" reason

let suite =
  ( "nofollow",
    [
      Alcotest.test_case "bounded read" `Quick
        test_read_nofollow_reads_a_bounded_regular_file;
      Alcotest.test_case "refuses the final link" `Quick
        test_read_nofollow_refuses_the_final_link;
      Alcotest.test_case "refuses the final link beneath a capability" `Quick
        test_read_nofollow_refuses_the_final_link_beneath_a_capability;
      Alcotest.test_case "enforces the bound" `Quick
        test_read_nofollow_enforces_the_bound;
      Alcotest.test_case "returns descriptor validation" `Quick
        test_read_nofollow_returns_descriptor_validation;
      Alcotest.test_case "uses the in-memory backend" `Quick
        test_read_nofollow_uses_the_in_memory_backend;
      Alcotest.test_case "is one open through a wrapper" `Quick
        test_read_nofollow_is_one_open_through_its_wrapper;
      Alcotest.test_case "validates a FIFO before reading" `Quick
        test_read_nofollow_validates_a_fifo_before_reading;
      Alcotest.test_case "costs the file, not the bound" `Quick
        test_read_nofollow_costs_the_file_not_the_bound;
      Alcotest.test_case "follows growth to the bound" `Quick
        test_read_nofollow_follows_growth_to_the_bound;
      Alcotest.test_case "wipes a failed read" `Quick
        test_read_nofollow_wipes_a_failed_read;
    ] )

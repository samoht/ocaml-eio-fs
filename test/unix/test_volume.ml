(* What the volume holding a path uses, against the size statvfs gives, and on
   macOS against diskutil's figure for each volume of one APFS container. *)

let with_tmp fn =
  Eio_main.run @@ fun env ->
  fn Eio.Path.(Eio.Stdenv.fs env / Filename.get_temp_dir_name ())

let bytes blocks (s : Eio_fs_unix.statvfs) = Int64.mul blocks s.block_size

(* Catches a figure of 0, and one larger than the size. The free space is a
   reading of its own call, which other writers move: only the size is the
   same in both. *)
let test_fits_its_container () =
  with_tmp @@ fun dir ->
  let used = Eio_fs_unix.volume_used dir in
  let s = Eio_fs_unix.statvfs dir in
  let total = bytes s.total_blocks s in
  Alcotest.(check bool)
    (Fmt.str "%Ld bytes used" used)
    true
    (Int64.compare used 0L > 0);
  Alcotest.(check bool)
    (Fmt.str "%Ld used fit in %Ld" used total)
    true
    (Int64.compare used total <= 0)

(* [prog args]'s standard output, or [None] if it does not exit with 0. *)
let run prog args =
  let ic = Unix.open_process_args_in prog (Array.of_list (prog :: args)) in
  let out = In_channel.input_all ic in
  match Unix.close_process_in ic with Unix.WEXITED 0 -> Some out | _ -> None

let hdiutil = "/usr/bin/hdiutil"
let diskutil = "/usr/sbin/diskutil"

(* The text of [<key>k</key>]'s value in a plist diskutil printed. *)
let plist_value plist k =
  let key = "<key>" ^ k ^ "</key>" in
  let rec find i =
    if i + String.length key > String.length plist then
      Alcotest.failf "diskutil gave no %s" k
    else if String.sub plist i (String.length key) = key then
      i + String.length key
    else find (i + 1)
  in
  let after = find 0 in
  let open_ = String.index_from plist after '>' + 1 in
  let close = String.index_from plist open_ '<' in
  String.sub plist open_ (close - open_)

let info target =
  match run diskutil [ "info"; "-plist"; target ] with
  | Some p -> p
  | None -> Alcotest.failf "diskutil info %s failed" target

(* What diskutil gives as the Volume Used Space of the volume at [mount]. *)
let diskutil_used mount =
  Int64.of_string (plist_value (info mount) "CapacityInUse")

(* A private APFS container on a sparse image, holding volume A mounted at [a]
   and volume B at [b], both inside a temporary directory and with no browsing,
   so no process other than the test writes to them. *)
let with_container fn =
  let tmp = Filename.temp_dir "eio-fs-volume" "" in
  let a = Filename.concat tmp "a" and b = Filename.concat tmp "b" in
  let image = Filename.concat tmp "v" in
  let skip_unless prog args =
    if Option.is_none (run prog args) then begin
      Fmt.pr "skipped: %s %s was refused@." prog (List.hd args);
      Alcotest.skip ()
    end
  in
  let must prog args =
    if Option.is_none (run prog args) then
      Alcotest.failf "%s %s failed" prog (String.concat " " args)
  in
  Fun.protect ~finally:(fun () -> ignore (run "/bin/rm" [ "-rf"; tmp ]))
  @@ fun () ->
  (* A container of 256 MiB refuses a second volume; a sparse image takes only
     the space written. *)
  skip_unless hdiutil
    [
      "create";
      "-quiet";
      "-size";
      "2g";
      "-fs";
      "APFS";
      "-volname";
      "A";
      "-type";
      "SPARSE";
      image;
    ];
  Unix.mkdir a 0o700;
  Unix.mkdir b 0o700;
  let attached =
    run hdiutil
      [
        "attach";
        "-plist";
        "-nobrowse";
        "-mountpoint";
        a;
        image ^ ".sparseimage";
      ]
  in
  let plist =
    match attached with
    | Some p -> p
    | None ->
        Fmt.pr "skipped: %s attach was refused@." hdiutil;
        Alcotest.skip ()
  in
  (* The image's whole disk comes first, and ejecting it takes the container
     and both volumes with it: detaching a mount point leaves the device
     attached. *)
  let disk = plist_value plist "dev-entry" in
  let ejected = ref false in
  let result =
    Fun.protect ~finally:(fun () ->
        ejected := Option.is_some (run diskutil [ "eject"; "force"; disk ]))
    @@ fun () ->
    let container = plist_value (info a) "APFSContainerReference" in
    let name = Fmt.str "eiofs%d" (Unix.getpid ()) in
    must diskutil [ "apfs"; "addVolume"; container; "APFS"; name; "-nomount" ];
    let device = plist_value (info name) "DeviceIdentifier" in
    must diskutil [ "mount"; "nobrowse"; "-mountPoint"; b; device ];
    fn ~a ~b
  in
  if not !ejected then Alcotest.failf "%s stayed attached" disk;
  result

(* On Darwin an APFS volume shares its container's space with the container's
   other volumes, and volume_used is the volume's own figure. Catches the
   container's blocks less its free blocks given instead, which counts A's
   bytes in B's and moves A's figure when B is written. *)
let test_not_its_container () =
  if not (Sys.file_exists hdiutil && Sys.file_exists diskutil) then begin
    Fmt.pr "skipped: no %s or %s, which macOS has@." hdiutil diskutil;
    Alcotest.skip ()
  end;
  with_container @@ fun ~a ~b ->
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let used p = Eio_fs_unix.volume_used Eio.Path.(fs / p) in
  let check_diskutil what p =
    Alcotest.(check int64)
      (what ^ " is diskutil's figure")
      (diskutil_used p) (used p)
  in
  let a0 = used a and b0 = used b in
  check_diskutil "A" a;
  check_diskutil "B" b;
  let written = 64 * 1024 * 1024 in
  let fd =
    Unix.openfile (Filename.concat b "f") [ Unix.O_WRONLY; Unix.O_CREAT ] 0o600
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let chunk = Bytes.make (1024 * 1024) 'x' in
      for _ = 1 to written / Bytes.length chunk do
        ignore (Unix.write fd chunk 0 (Bytes.length chunk) : int)
      done;
      Unix.fsync fd);
  (* APFS frees the blocks of the metadata a write replaced at its next
     checkpoint, which sync(2) makes now. *)
  ignore (run "/bin/sync" []);
  let a1 = used a and b1 = used b in
  check_diskutil "A" a;
  check_diskutil "B" b;
  Alcotest.(check int64) "A does not move when B is written" a0 a1;
  (* No upper bound: the metadata APFS allocates for the write varies from run
     to run, and diskutil's figure already pins the value. *)
  let grew = Int64.sub b1 b0 and written = Int64.of_int written in
  Alcotest.(check bool)
    (Fmt.str "B grew by %Ld for %Ld written" grew written)
    true
    (Int64.compare grew written >= 0)

let suite =
  ( "volume",
    [
      Alcotest.test_case "used fits its container" `Quick
        test_fits_its_container;
      Alcotest.test_case "used is not the container's" `Quick
        test_not_its_container;
    ] )

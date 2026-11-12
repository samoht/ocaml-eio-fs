(* The in-memory filesystem replays every scenario of the recorded traces:
   with [~os:`Linux] it must give Linux's outcomes, and with [~os:`Macos]
   macOS's, except where an exception below says why not.

   Traces recorded as a user, straight from the kernel through [Unix] and
   through eio_posix, on Linux (Docker image ocaml/opam, pinned by digest in
   scripts/generate.sh, on a tmpfs) and on macOS (case-insensitive APFS). The
   filesystem follows the kernel; from an operation [Unix] cannot make on, it
   follows eio_posix.
   Regenerate, on macOS: REGEN=1 dune build @test/os/regen-traces *)

type os = [ `Linux | `Macos ]

let os_name : os -> string = function `Linux -> "Linux" | `Macos -> "macOS"

(* Operations whose outcome differs from the kernel's, and why. The
   filesystem answers there as eio_posix does. *)
type exception_ = { os : os; scenario : string; ops : int list; why : string }

let path_max =
  "eio_posix resolves a path one component at a time, so no path is too long"

let exceptions =
  [
    { os = `Linux; scenario = "path max"; ops = [ 2 ]; why = path_max };
    { os = `Macos; scenario = "path max"; ops = [ 1; 2; 3 ]; why = path_max };
    {
      os = `Linux;
      scenario = "dir search only";
      ops = [ 4 ];
      why = "eio_posix opens a directory O_PATH, which needs no read bit";
    };
  ]

type row = {
  linux : string list;
  macos : string list;
  linux_eio : string list;
  macos_eio : string list;
}

let traces =
  lazy
    (In_channel.with_open_text "traces/fs.csv" In_channel.input_lines
    |> List.tl
    |> List.map (fun line ->
        let outcomes s = String.split_on_char '|' s in
        match String.split_on_char ',' line with
        | [ ops; l; m; le; me ] ->
            ( ops,
              {
                linux = outcomes l;
                macos = outcomes m;
                linux_eio = outcomes le;
                macos_eio = outcomes me;
              } )
        | _ -> Fmt.failwith "traces/fs.csv: %s" line))

let user = { Eio_fs.uid = 1000; gid = 1000; groups = [ 4242 ]; umask = 0o022 }
let ids = { Os_seq.gid = 1000; other_gid = 4242 }

let replay ~os i ops =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run ~os ~user ~cwd:"/w" @@ fun env ->
  let dir = Eio.Path.(env#cwd / string_of_int i) in
  Eio.Path.mkdir ~perm:0o755 dir;
  Os_seq.run ids dir ops

let check (os : os) i (name, ops) () =
  let row =
    match List.assoc_opt (Os_seq.key ops) (Lazy.force traces) with
    | Some r -> r
    | None -> Alcotest.failf "%s: not in traces/fs.csv, regenerate" name
  in
  let kernel, eio =
    match os with
    | `Linux -> (row.linux, row.linux_eio)
    | `Macos -> (row.macos, row.macos_eio)
  in
  let got = replay ~os i ops in
  (* From an operation [Unix] cannot make on, the kernel's state is not
     eio_posix's: follow eio_posix. *)
  let cut =
    let rec go j = function
      | [] -> max_int
      | "-" :: _ -> j
      | _ :: l -> go (j + 1) l
    in
    go 0 kernel
  in
  List.iteri
    (fun j op ->
      let want = List.nth (if j >= cut then eio else kernel) j in
      let excused =
        List.find_opt
          (fun e -> e.scenario = name && e.os = os && List.mem (j + 1) e.ops)
          exceptions
      in
      let got = List.nth got j in
      match excused with
      | None when got <> want ->
          Alcotest.failf "%s, %s, op %d %S: %s answers %s, the filesystem %s"
            name (os_name os) (j + 1) op (os_name os) want got
      | Some e when got = want ->
          Alcotest.failf "%s, %s, op %d %S: the exception %S is stale" name
            (os_name os) (j + 1) op e.why
      | None | Some _ -> ())
    ops

let suite (os : os) =
  ( os_name os,
    List.mapi
      (fun i ((name, _) as s) -> Alcotest.test_case name `Quick (check os i s))
      Os_seq.scenarios )

let () = Alcotest.run "os" [ suite `Linux; suite `Macos ]

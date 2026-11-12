(* Records each scenario's outcomes, one CSV row per scenario: its
   operations, separated by ";", then their outcomes straight from the kernel
   through [Unix], and through eio_posix, each separated by "|".

   record                   records in the current directory, which must be
                            empty, as the user running it
   record merge LINUX MACOS joins two recordings of the same scenarios *)

let clean s = String.map (function ',' | '\n' -> ' ' | c -> c) s

let record () =
  ignore (Unix.umask 0o022);
  let gid = Unix.getgid () in
  (* The directory takes the user's group: macOS gives a new entry its
     directory's. *)
  Unix.chown "." (-1) gid;
  let other_gid =
    match List.filter (( <> ) gid) (Array.to_list (Unix.getgroups ())) with
    | g :: _ -> g
    | [] -> failwith "record: the user needs a supplementary group"
  in
  let ids = { Os_seq.gid; other_gid } in
  Eio_posix.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  print_endline "ops,kernel,eio";
  let row l = clean (String.concat "|" l) in
  List.iteri
    (fun i (_, ops) ->
      let k = Fmt.str "k%d" i in
      Unix.mkdir k 0o755;
      let kernel = Os_seq.run_unix ids k ops in
      let dir = Eio.Path.(cwd / string_of_int i) in
      Eio.Path.mkdir ~perm:0o755 dir;
      let eio = Os_seq.run ids dir ops in
      Fmt.pr "%s,%s,%s@." (Os_seq.key ops) (row kernel) (row eio))
    Os_seq.scenarios

let rows file =
  In_channel.with_open_text file In_channel.input_lines
  |> List.tl
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ ops; kernel; eio ] -> (ops, (kernel, eio))
      | _ -> failwith ("record merge: " ^ line))

let merge linux macos =
  let macos = rows macos in
  print_endline "ops,linux,macos,linux eio_posix,macos eio_posix";
  List.iter
    (fun (ops, (lk, le)) ->
      let mk, me =
        Option.value
          ~default:("not recorded", "not recorded")
          (List.assoc_opt ops macos)
      in
      Fmt.pr "%s,%s,%s,%s,%s@." ops lk mk le me)
    (rows linux)

let () =
  match Array.to_list Sys.argv with
  | [ _ ] -> record ()
  | [ _; "merge"; linux; macos ] -> merge linux macos
  | _ ->
      prerr_endline "usage: record | record merge LINUX MACOS";
      exit 2

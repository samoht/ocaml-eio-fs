open Cmdliner

let dir =
  Arg.(
    required
    & pos 0 (some dir) None
    & info [] ~docv:"DIR" ~doc:"The directory tree to embed.")

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"FILE.ml"
        ~doc:
          "Write the generated module to $(docv) and its interface to the \
           matching .mli, instead of printing the module to standard output.")

let run dir output =
  Eio_main.run @@ fun env ->
  match Eio_fs.Crunch.scan Eio.Path.(env#fs / dir) with
  | entries -> (
      match output with
      | None ->
          Fmt.pr "%a@?" Eio_fs.Crunch.source entries;
          0
      | Some file when Filename.extension file <> ".ml" ->
          Fmt.epr "eio-fs-crunch: output file %s must end in .ml@." file;
          1
      | Some file ->
          let save name contents =
            Eio.Path.save ~create:(`Or_truncate 0o644)
              Eio.Path.(env#fs / name)
              contents
          in
          save file (Fmt.str "%a" Eio_fs.Crunch.source entries);
          save (Filename.remove_extension file ^ ".mli") Eio_fs.Crunch.interface;
          0)
  | exception (Eio.Io _ as e) ->
      Fmt.epr "eio-fs-crunch: %a@." Eio.Exn.pp e;
      1
  | exception Invalid_argument message ->
      Fmt.epr "eio-fs-crunch: %s@." message;
      1

let cmd =
  let doc = "Embed a directory tree as an OCaml module for eio-fs." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) walks $(i,DIR) and prints an OCaml module that embeds every \
         directory, file and symbolic link it contains. Linking the generated \
         module and calling its [load ()] rebuilds the tree in the eio-fs \
         in-memory filesystem, giving programs without a real filesystem \
         (unikernels, tests) an ordinary [Eio.Path] view of static assets.";
    ]
  in
  let info = Cmd.info "eio-fs-crunch" ~doc ~man in
  Cmd.v info
    Term.(
      const (fun dir output ->
          match run dir output with 0 -> () | n -> exit n)
      $ dir $ output)

let () = exit (Cmd.eval cmd)

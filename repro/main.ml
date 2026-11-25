(* main.exe CASE: one Eio operation on a Win32 device path through the
   unrestricted fs, printing its result or the exception that escapes. *)

let () =
  Eio_windows.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let open Eio.Path in
  let result =
    try
      match Sys.argv.(1) with
      | "load-null" -> Printf.sprintf "%S" (load (fs / Filename.null))
      | "load-dot-null" -> Printf.sprintf "%S" (load (fs / {|\\.\NUL|}))
      | "save-null" -> save ~create:`Never (fs / Filename.null) "x"; "ok"
      | "stat-null" ->
          Fmt.str "%a" Eio.File.Stat.pp_kind (stat ~follow:true (fs / Filename.null)).kind
      | c -> failwith ("unknown case " ^ c)
    with Eio.Io _ as e -> "Eio.Io: " ^ Printexc.to_string e
  in
  print_endline result

(* Writes every file of a git repository's HEAD into a fresh directory three
   ways, with 8 fibres each: one Eio.Path call per operation, Eio_fs_unix.Batch
   with 512 operations per hop, and unconfined Unix calls with 512 operations
   per run_in_systhread. Prints files/s for each. *)

type entry = { path : string; perm : int; link : bool; contents : string }

let fixture repo =
  let tmp = Filename.temp_file "batch-bench" "" in
  let cmd fmt = Fmt.kstr (fun c -> if Sys.command c <> 0 then failwith c) fmt in
  cmd "git -C %s ls-tree -r HEAD > %s.tree" (Filename.quote repo) tmp;
  let lines =
    In_channel.with_open_bin (tmp ^ ".tree") In_channel.input_lines
    |> List.filter_map (fun l ->
        match String.index_opt l '\t' with
        | None -> None
        | Some i -> (
            match String.split_on_char ' ' (String.sub l 0 i) with
            | [ mode; "blob"; hash ] ->
                Some (mode, hash, String.sub l (i + 1) (String.length l - i - 1))
            | _ -> None))
  in
  Out_channel.with_open_bin (tmp ^ ".in") (fun oc ->
      List.iter (fun (_, h, _) -> output_string oc (h ^ "\n")) lines);
  cmd "git -C %s cat-file --batch < %s.in > %s.out" (Filename.quote repo) tmp
    tmp;
  let ic = open_in_bin (tmp ^ ".out") in
  let entries =
    List.map
      (fun (mode, _, path) ->
        let header = input_line ic in
        let size =
          int_of_string (List.nth (String.split_on_char ' ' header) 2)
        in
        let contents = really_input_string ic size in
        ignore (input_char ic);
        let perm = if String.equal mode "100755" then 0o755 else 0o644 in
        { path; perm; link = String.equal mode "120000"; contents })
      lines
  in
  close_in ic;
  List.iter Sys.remove [ tmp; tmp ^ ".tree"; tmp ^ ".in"; tmp ^ ".out" ];
  entries

let dirs entries =
  let tbl = Hashtbl.create 4096 in
  let rec add d =
    if (not (String.equal d ".")) && not (Hashtbl.mem tbl d) then (
      add (Filename.dirname d);
      Hashtbl.add tbl d ())
  in
  List.iter (fun e -> add (Filename.dirname e.path)) entries;
  Hashtbl.fold (fun d () acc -> d :: acc) tbl []
  |> List.sort (fun a b -> compare (String.length a) (String.length b))

let rec chunks n l =
  match l with
  | [] -> []
  | l ->
      let rec take i acc = function
        | x :: r when i < n -> take (i + 1) (x :: acc) r
        | r -> (List.rev acc, r)
      in
      let c, rest = take 0 [] l in
      c :: chunks n rest

let fibres = 8
let batch = 512

let eio_path root entries =
  List.iter
    (fun d -> Eio.Path.mkdir ~perm:0o755 Eio.Path.(root / d))
    (dirs entries);
  Eio.Fiber.List.iter ~max_fibers:fibres
    (fun e ->
      let p = Eio.Path.(root / e.path) in
      if e.link then Eio.Path.symlink ~link_to:e.contents p
      else Eio.Path.save ~create:(`Exclusive e.perm) p e.contents)
    entries

let batched root entries =
  Eio.Switch.run @@ fun sw ->
  let t = Eio_fs_unix.Batch.v ~sw root in
  let check rs =
    List.iter
      (function
        | Ok _ -> () | Error e -> Fmt.failwith "%a" Eio_fs_unix.Batch.pp_error e)
      rs
  in
  check
    (Eio_fs_unix.Batch.run t
       (List.map
          (fun path -> Eio_fs_unix.Batch.Mkdir { path; perm = 0o755 })
          (dirs entries)));
  Eio.Fiber.List.iter ~max_fibers:fibres
    (fun c ->
      check
        (Eio_fs_unix.Batch.run t
           (List.map
              (fun e ->
                if e.link then
                  Eio_fs_unix.Batch.Symlink
                    { path = e.path; target = e.contents }
                else
                  Eio_fs_unix.Batch.Create
                    { path = e.path; perm = e.perm; contents = e.contents })
              c)))
    (chunks batch entries)

let raw native entries =
  let f = Filename.concat native in
  Eio_unix.run_in_systhread (fun () ->
      List.iter (fun d -> Unix.mkdir (f d) 0o755) (dirs entries));
  Eio.Fiber.List.iter ~max_fibers:fibres
    (fun c ->
      Eio_unix.run_in_systhread @@ fun () ->
      List.iter
        (fun e ->
          if e.link then Unix.symlink e.contents (f e.path)
          else
            let fd =
              Unix.openfile (f e.path)
                [ O_WRONLY; O_CREAT; O_EXCL; O_CLOEXEC ]
                e.perm
            in
            let n = String.length e.contents in
            let rec go o =
              if o < n then go (o + Unix.write_substring fd e.contents o (n - o))
            in
            go 0;
            Unix.fchmod fd e.perm;
            Unix.close fd)
        c)
    (chunks batch entries)

let () =
  let repo = Sys.argv.(1) and scratch = Sys.argv.(2) in
  let entries = fixture repo in
  let n = List.length entries in
  Fmt.pr "%d files@." n;
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let runs = 3 in
  List.iter
    (fun (name, write) ->
      let rates =
        List.init runs (fun i ->
            let native = Fmt.str "%s/%s-%d" scratch name i in
            Unix.mkdir native 0o755;
            let t0 = Unix.gettimeofday () in
            write native (Eio.Path.( / ) fs native);
            let dt = Unix.gettimeofday () -. t0 in
            Eio.Path.rmtree (Eio.Path.( / ) fs native);
            float n /. dt)
      in
      Fmt.pr "%-10s %a files/s@." name
        Fmt.(list ~sep:(any " ") (fmt "%.0f"))
        (List.sort compare rates))
    [
      ("eio-path", fun _ root -> eio_path root entries);
      ("batch", fun _ root -> batched root entries);
      ("raw-unix", fun native _ -> raw native entries);
    ]

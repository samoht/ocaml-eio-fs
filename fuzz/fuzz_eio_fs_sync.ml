open Alcobar

let ( / ) = Eio.Path.( / )

let no_lf =
  map [ bytes ] (fun s -> String.map (fun c -> if c = '\n' then ' ' else c) s)

let rec last n l = if List.length l <= n then l else last n (List.tl l)

let test_append before lines =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let p = env#fs / "log" in
  if before <> "" then Eio.Path.save ~create:(`Exclusive 0o644) p before;
  Eio_fs_sync.append_lines ~durable:false p lines;
  let after = try Eio.Path.load p with Eio.Io _ -> "" in
  if not (String.starts_with ~prefix:before after) then
    failf "the bytes held before were changed: %S then %S" before after;
  if lines <> [] then begin
    if not (String.ends_with ~suffix:"\n" after) then
      failf "the file does not end in LF: %S" after;
    let got =
      String.split_on_char '\n' (String.sub after 0 (String.length after - 1))
    in
    if not (List.equal String.equal (last (List.length lines) got) lines) then
      failf "the appended lines are not the file's last: %S" after
  end

let suite =
  ( "eio_fs_sync",
    [
      test_case "appended lines are the last" [ bytes; list no_lf ] test_append;
    ] )

open! Hermetic

let is_abs p = String.length p > 0 && p.[0] = '/'
let is_rel p = not (is_abs p)
let segs p = String.split_on_char '/' p

let is_directory_syntax p =
  let len = String.length p in
  len > 0
  && (p.[len - 1] = '/'
     || match List.rev (segs p) with "." :: _ | ".." :: _ -> true | _ -> false)

let normalize p =
  let absolute = is_abs p in
  let rec loop acc = function
    | [] -> List.rev acc
    | ("" | ".") :: rest -> loop acc rest
    | ".." :: rest -> (
        match acc with
        | segment :: acc when not (String.equal segment "..") -> loop acc rest
        | [] when absolute -> loop [] rest
        | acc -> loop (".." :: acc) rest)
    | segment :: rest -> loop (segment :: acc) rest
  in
  let directory = is_directory_syntax p in
  let body = String.concat "/" (loop [] (segs p)) in
  let path =
    match (absolute, body) with
    | true, "" -> "/"
    | true, body -> "/" ^ body
    | false, "" -> "."
    | false, body -> body
  in
  if directory && not (String.equal path "/") then path ^ "/" else path

let append base path =
  if is_abs path then path
  else
    let len = String.length base in
    if len = 0 || base.[len - 1] = '/' then base ^ path else base ^ "/" ^ path

let to_dir_path path =
  let len = String.length path in
  if len > 0 && path.[len - 1] = '/' then path else path ^ "/"

let without_trailing_slashes path =
  let rec last_non_slash i =
    if i <= 0 || path.[i] <> '/' then i else last_non_slash (i - 1)
  in
  let last = last_non_slash (String.length path - 1) in
  if last < 0 then path else String.sub path 0 (last + 1)

let basename path =
  let path = without_trailing_slashes path in
  if String.equal path "/" then ""
  else
    let name =
      match String.rindex_opt path '/' with
      | None -> path
      | Some index ->
          String.sub path (index + 1) (String.length path - index - 1)
    in
    match name with "." | ".." -> "" | name -> name

let rec parent path =
  let path = without_trailing_slashes path in
  if String.equal path "/" then path
  else
    let dir, last =
      match String.rindex_opt path '/' with
      | None -> ("", path)
      | Some i ->
          ( String.sub path 0 (i + 1),
            String.sub path (i + 1) (String.length path - i - 1) )
    in
    match last with
    | "" | "." -> if String.equal dir "" then "../" else parent dir
    | ".." -> if String.equal dir "/" then "/" else path ^ "/../"
    | _ -> if String.equal dir "" then "./" else dir

let components path =
  normalize path |> segs
  |> List.filter (fun segment ->
      not (String.equal segment "" || String.equal segment "."))

let is_rooted ~root path =
  if is_abs root <> is_abs path then false
  else
    let rec is_prefix prefix items =
      match (prefix, items) with
      | [], _ -> true
      | x :: xs, y :: ys when String.equal x y -> is_prefix xs ys
      | _ -> false
    in
    is_prefix (components root) (components path)

type refusal = Not_a_path | Relative | Root | Not_canonical

let traversal segment = String.equal segment "." || String.equal segment ".."

let canonical path =
  if String.equal path "" || String.contains path '\000' then Error Not_a_path
  else if is_rel path then Error Relative
  else
    let parts =
      List.filter (fun segment -> not (String.equal segment "")) (segs path)
    in
    if parts = [] then Error Root
    else if List.exists traversal parts then Error Not_canonical
      (* The components must rejoin to exactly the string given. This is the
         whole of the "already canonical" rule and it is one comparison: a
         trailing separator, a doubled separator and a leading double separator
         all lose to it, and each of them is a spelling that names the same
         object as the canonical one while splitting differently. *)
    else if not (String.equal ("/" ^ String.concat "/" parts) path) then
      Error Not_canonical
    else Ok parts

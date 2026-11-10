let field name report =
  let prefix = "  " ^ name ^ ":" in
  String.split_on_char '\n' report
  |> List.find_map (fun l ->
      if String.starts_with ~prefix l then
        let n = String.length prefix in
        Some (String.trim (String.sub l n (String.length l - n)))
      else None)
  |> Option.value ~default:""

let report f =
  match f () with
  | () -> Alcotest.fail "the check passed: it did not catch the bug"
  | exception Failure m -> m

let caught ~tier f () =
  let m = report f in
  if field "tier" m = "" then
    Alcotest.failf "the check failed for another reason: %s" m;
  Alcotest.(check string) "tier" tier (field "tier" m)

let refused prefix f () =
  let m = report f in
  if not (String.starts_with ~prefix m) then
    Alcotest.failf "the check failed with %S" m

let passes f () = f ()

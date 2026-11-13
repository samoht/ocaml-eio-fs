open Alcobar

(* The disjoint, ordered list of ranges the in-memory filesystem and the crash
   model each kept before [Extents]. *)
module Ref = struct
  let add a b held =
    if b <= a then held
    else
      let before = List.filter (fun (_, y) -> y < a) held
      and after = List.filter (fun (x, _) -> x > b) held
      and over = List.filter (fun (x, y) -> y >= a && x <= b) held in
      let a = List.fold_left (fun a (x, _) -> min a x) a over
      and b = List.fold_left (fun b (_, y) -> max b y) b over in
      before @ ((a, b) :: after)

  let cut n held =
    List.filter_map
      (fun (x, y) -> if x >= n then None else Some (x, min y n))
      held

  let overlaps a b held = List.exists (fun (x, y) -> x < b && y > a) held

  let pages held =
    let page = 4096 in
    snd
      (List.fold_left
         (fun (last, n) (x, y) ->
           let a = max (x / page) (last + 1) and b = (y - 1) / page in
           if y <= x || b < a then (last, n) else (b, n + b - a + 1))
         (-1, 0) held)
end

type op = Add of int * int | Cut of int | Overlaps of int * int | From of int

(* Offsets within a byte of a multiple of 1024 over three pages, so ranges
   often touch, overlap and share pages. *)
let offset = map [ range 13; range 3 ] (fun k d -> max 0 ((k * 1024) + d - 1))

let op =
  choose
    [
      map [ offset; offset ] (fun a n -> Add (a, a + n));
      map [ offset ] (fun n -> Cut n);
      map [ offset; offset ] (fun a b -> Overlaps (a, b));
      map [ offset ] (fun n -> From n);
    ]

let pp_ranges = Fmt.(Dump.list (Dump.pair int int))

let to_list ?from t =
  List.rev (Extents.fold ?from (fun x y l -> (x, y) :: l) t [])

let test_against_ref ops =
  ignore
    (List.fold_left
       (fun (t, l) op ->
         let t, l =
           match op with
           | Add (a, b) -> (Extents.add a b t, Ref.add a b l)
           | Cut n -> (Extents.cut n t, Ref.cut n l)
           | Overlaps (a, b) ->
               check_eq ~pp:Fmt.bool (Extents.overlaps a b t)
                 (Ref.overlaps a b l);
               (t, l)
           | From n ->
               check_eq ~pp:pp_ranges (to_list ~from:n t)
                 (List.filter (fun (_, y) -> y > n) l);
               (t, l)
         in
         check_eq ~pp:pp_ranges (to_list t) l;
         check_eq ~pp:Fmt.int (Extents.pages t) (Ref.pages l);
         (t, l))
       (Extents.empty, []) ops)

let suite =
  ( "extents",
    [ test_case "the ranges of a list of ranges" [ list op ] test_against_ref ]
  )

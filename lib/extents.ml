module M = Map.Make (Int)

(* Each range's start maps to its end. *)
type t = { ranges : int M.t; pages : int }

let empty = { ranges = M.empty; pages = 0 }
let page = 4096

(* The pages a run of consecutive ranges puts bytes in: two neighbours share
   at most one page, and the ranges in one page are a run. *)
let pages_of = function
  | [] -> 0
  | (x, y) :: rest ->
      let span x y = ((y - 1) / page) - (x / page) + 1 in
      snd
        (List.fold_left
           (fun (y, n) (x', y') ->
             let shared = if (y - 1) / page = x' / page then 1 else 0 in
             (y', n + span x' y' - shared))
           (y, span x y)
           rest)

(* Only the last range starting before [a] can reach it; every range starting
   in [\[a, b\]] meets [\[a, b)]. The pages change only within the run of
   merged ranges and their neighbours. *)
let add a b t =
  if b <= a then t
  else
    let a, b, merged, ranges =
      match M.find_last_opt (fun x -> x < a) t.ranges with
      | Some (x, y) when y >= a -> (x, max b y, [ (x, y) ], M.remove x t.ranges)
      | _ -> (a, b, [], t.ranges)
    in
    let rec merge b merged ranges =
      match M.find_first_opt (fun x -> x >= a) ranges with
      | Some (x, y) when x <= b ->
          merge (max b y) ((x, y) :: merged) (M.remove x ranges)
      | next -> (b, List.rev merged, next, ranges)
    in
    let b, merged, next, ranges = merge b merged ranges in
    let prev = M.find_last_opt (fun x -> x < a) ranges in
    let around l = Option.to_list prev @ l @ Option.to_list next in
    {
      ranges = M.add a b ranges;
      pages = t.pages + pages_of (around [ (a, b) ]) - pages_of (around merged);
    }

let cut n t =
  let below, at, above = M.split n t.ranges in
  let dropped =
    Option.fold ~none:[] ~some:(fun y -> [ (n, y) ]) at @ M.bindings above
  in
  let last = M.max_binding_opt below in
  let kept, ranges =
    match last with
    | Some (x, y) when y > n -> ([ (x, n) ], M.add x n below)
    | _ -> (Option.to_list last, below)
  in
  {
    ranges;
    pages = t.pages - pages_of (Option.to_list last @ dropped) + pages_of kept;
  }

let overlaps a b t =
  match M.find_last_opt (fun x -> x < b) t.ranges with
  | Some (_, y) -> y > a
  | None -> false

let pages t = t.pages

let fold ?(from = min_int) f t acc =
  let start =
    match M.find_last_opt (fun x -> x < from) t.ranges with
    | Some (x, y) when y > from -> x
    | _ -> from
  in
  Seq.fold_left (fun acc (x, y) -> f x y acc) acc (M.to_seq_from start t.ranges)

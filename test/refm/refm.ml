(* An ext4-like reference for one file's data under failed syncs, apart from
   Chaos.Crash.
   Each block keeps sets of possible contents: in the page cache, on disk
   (durable), possibly written by a partly failed writeback, and the versions
   dirtied since the last writeback. It over-approximates what Linux may
   leave; the model under test must stay inside it. *)

let bs = ref 2
let nb = 24

module SS = Set.Make (String)
module IS = Set.Make (Int)

type chg = CW of int * string | CR of int

type t = {
  mutable clen : int;
  cache : SS.t array;
  disk : SS.t array;
  partial : SS.t array;
  cut : SS.t array; (* lost blocks a durable truncation freed *)
  pcut : int array; (* least truncation since the block's partial capture *)
  dirty : SS.t array;
  isdirty : bool array;
  mutable dset : IS.t;
  mutable pend : chg list; (* non-lost changes since last good sync *)
}

let blk s b =
  let bs = !bs in
  String.init bs (fun i ->
      let p = (b * bs) + i in
      if p < String.length s then s.[p] else '\000')

let zero_from m b s =
  let bs = !bs in
  String.init bs (fun i -> if (b * bs) + i >= m then '\000' else s.[i])

let v s =
  {
    clen = String.length s;
    cache = Array.init nb (fun b -> SS.singleton (blk s b));
    disk = Array.init nb (fun b -> SS.singleton (blk s b));
    partial = Array.make nb SS.empty;
    cut = Array.make nb SS.empty;
    pcut = Array.make nb max_int;
    dirty = Array.make nb SS.empty;
    isdirty = Array.make nb false;
    dset = IS.singleton (String.length s);
    pend = [];
  }

let copy r =
  {
    r with
    cache = Array.copy r.cache;
    disk = Array.copy r.disk;
    partial = Array.copy r.partial;
    cut = Array.copy r.cut;
    pcut = Array.copy r.pcut;
    dirty = Array.copy r.dirty;
    isdirty = Array.copy r.isdirty;
  }

let mark r b =
  r.isdirty.(b) <- true;
  r.dirty.(b) <- SS.union r.dirty.(b) r.cache.(b)

let write r off data =
  let bs = !bs in
  let n = String.length data in
  let hi = off + n in
  for b = off / bs to (hi - 1) / bs do
    r.cache.(b) <-
      SS.map
        (fun s ->
          String.init bs (fun i ->
              let p = (b * bs) + i in
              if p >= off && p < hi then data.[p - off] else s.[i]))
        r.cache.(b)
  done;
  let lo = min r.clen off in
  r.clen <- max r.clen hi;
  for b = lo / bs to (hi - 1) / bs do
    mark r b
  done;
  r.pend <- r.pend @ [ CW (off, data) ]

let resize r m =
  let bs = !bs in
  if m < r.clen then begin
    for b = m / bs to (r.clen - 1) / bs do
      r.cache.(b) <- SS.map (zero_from m b) r.cache.(b)
    done;
    r.clen <- m;
    if m mod bs <> 0 then mark r (m / bs)
  end
  else if m > r.clen then begin
    let old = r.clen in
    r.clen <- m;
    for b = old / bs to (m - 1) / bs do
      mark r b
    done
  end;
  Array.iteri (fun b x -> r.pcut.(b) <- min x m) r.pcut;
  r.pend <- r.pend @ [ CR m ]

let app_len l = function CW (o, d) -> max l (o + String.length d) | CR m -> m

let sync_ok r =
  (* A truncation made durable frees the blocks past it: a non-dirty block
     reads zero there, and a lost block it cut away whole is gone. *)
  let zmin =
    List.fold_left
      (fun m -> function CR x -> min m x | CW _ -> m)
      max_int r.pend
  in
  for b = 0 to nb - 1 do
    if not r.isdirty.(b) then begin
      r.disk.(b) <- SS.map (zero_from zmin b) r.disk.(b);
      if b * !bs >= r.pcut.(b) then begin
        r.cut.(b) <- SS.union r.cut.(b) r.partial.(b);
        r.partial.(b) <- SS.empty
      end
    end
  done;
  let nd0 =
    IS.add r.clen (IS.map (fun d -> List.fold_left app_len d r.pend) r.dset)
  in
  for b = 0 to nb - 1 do
    if r.isdirty.(b) then begin
      (* the durable length may stop short of the page cache's *)
      r.disk.(b) <-
        IS.fold
          (fun l acc -> SS.union acc (SS.map (zero_from l b) r.cache.(b)))
          nd0 SS.empty;
      r.partial.(b) <- SS.empty;
      r.cut.(b) <- SS.empty;
      r.isdirty.(b) <- false;
      r.dirty.(b) <- SS.empty
    end
  done;
  let nd =
    IS.add r.clen (IS.map (fun d -> List.fold_left app_len d r.pend) r.dset)
  in
  r.dset <- nd;
  let mx = IS.max_elt nd in
  for b = 0 to nb - 1 do
    r.disk.(b) <- SS.map (zero_from mx b) r.disk.(b)
  done;
  r.pend <- []

let sync_fail ~reorder r =
  let bs = !bs in
  for b = 0 to nb - 1 do
    if r.isdirty.(b) then begin
      if reorder then begin
        if b * bs < r.clen then begin
          r.partial.(b) <- SS.union r.partial.(b) r.cache.(b);
          r.pcut.(b) <- max_int
        end;
        (* eviction: the page re-read from disk *)
        r.cache.(b) <-
          SS.union r.cache.(b)
            (let src = SS.union r.disk.(b) r.partial.(b) in
             let zm =
               List.fold_left
                 (fun m -> function CR x -> min m x | CW _ -> m)
                 r.clen r.pend
             in
             SS.union
               (SS.map (zero_from r.clen b) src)
               (SS.map (zero_from zm b) src))
      end;
      r.isdirty.(b) <- false;
      r.dirty.(b) <- SS.empty
    end
  done;
  r.pend <- List.filter (function CR _ -> true | CW _ -> false) r.pend

type policy = Drop | Survive_all | Reorder

let lengths ~policy r =
  match policy with
  | Drop -> r.dset
  | Survive_all -> IS.map (fun d -> List.fold_left app_len d r.pend) r.dset
  | Reorder ->
      let s = ref IS.empty in
      IS.iter
        (fun d ->
          let l = ref d in
          s := IS.add d !s;
          List.iter
            (fun c ->
              l := app_len !l c;
              s := IS.add !l !s)
            r.pend)
        r.dset;
      let lo = IS.min_elt !s and hi = IS.max_elt !s in
      let bs = !bs in
      let b = ref (((lo / bs) + 1) * bs) in
      while !b < hi do
        s := IS.add !b !s;
        b := !b + bs
      done;
      !s

(* [None] if the image is allowed, or a reason. *)
let check ~policy r img =
  let bs = !bs in
  let l = String.length img in
  let ls = lengths ~policy r in
  if not (IS.mem l ls) then
    Some
      (Fmt.str "length %d not in {%s}" l
         (String.concat "," (List.map string_of_int (IS.elements ls))))
  else
    let mind =
      List.fold_left
        (fun m -> function CR x -> min m x | CW _ -> m)
        (IS.min_elt r.dset) r.pend
    in
    let bad = ref None in
    for b = 0 to ((l + bs - 1) / bs) - 1 do
      if !bad = None then begin
        let got = blk img b in
        let n = min bs (l - (b * bs)) in
        let matches ~tol c =
          let ok = ref true in
          for i = 0 to n - 1 do
            if got.[i] <> c.[i] then
              if not (tol && got.[i] = '\000' && (b * bs) + i >= mind) then
                ok := false
          done;
          !ok
        in
        let disk_ok = SS.exists (matches ~tol:true) r.disk.(b) in
        let dirty_ok =
          policy <> Drop && SS.exists (matches ~tol:false) r.dirty.(b)
        in
        let part_ok =
          policy = Reorder && SS.exists (matches ~tol:false) r.partial.(b)
        in
        if not (disk_ok || dirty_ok || part_ok) then
          let cut_ok =
            policy = Reorder && SS.exists (matches ~tol:false) r.cut.(b)
          in
          let show s = String.escaped s in
          bad :=
            Some
              (Fmt.str "%sblock %d = %S; disk {%s} dirty {%s} partial {%s}"
                 (if cut_ok then "CUT-PARTIAL " else "")
                 b (String.sub got 0 n)
                 (String.concat "," (List.map show (SS.elements r.disk.(b))))
                 (String.concat "," (List.map show (SS.elements r.dirty.(b))))
                 (String.concat "," (List.map show (SS.elements r.partial.(b)))))
      end
    done;
    !bad

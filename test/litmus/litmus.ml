module Chaos = Eio_fs.Chaos
module Crash = Chaos.Crash

type op =
  | Creat of string
  | Write of string * string
  | Pwrite of string * int * string
  | Rename of string * string
  | Fsync of string
  | Mark
  | Mkdir of string
  | Unlink of string
  | Symlink of { target : string; name : string }
  | Truncate of string * int
  | Fsync_dir of string
  | Fsync_fails of string

type t = {
  name : string;
  setup : op list;
  body : op list;
  observe : string list;
}

type entry = Absent | File of string | Dir | Link of string
type outcome = { marked : bool; entries : (string * entry) list }

(* Runs of four or more bytes print as [c{n}], NUL as [\0]. *)
let pp_bytes ppf s =
  let n = String.length s in
  if n = 0 then Fmt.string ppf "\"\""
  else
    let pp_char ppf c =
      if c = '\000' then Fmt.string ppf "\\0" else Fmt.char ppf c
    in
    let rec go i =
      if i < n then begin
        let c = s.[i] in
        let j = ref i in
        while !j < n && s.[!j] = c do
          incr j
        done;
        let len = !j - i in
        if len >= 4 then Fmt.pf ppf "%a{%d}" pp_char c len
        else
          for _ = 1 to len do
            pp_char ppf c
          done;
        go !j
      end
    in
    go 0

let pp_entry ppf = function
  | Absent -> Fmt.string ppf "absent"
  | File s -> pp_bytes ppf s
  | Dir -> Fmt.string ppf "dir"
  | Link t -> Fmt.pf ppf "->%s" t

module Outcome = struct
  type t = outcome

  let compare = Stdlib.compare

  let pp ppf o =
    let pp_one ppf (n, e) = Fmt.pf ppf "%s=%a" n pp_entry e in
    Fmt.pf ppf "%s%a"
      (if o.marked then "marked " else "")
      Fmt.(list ~sep:(any ", ") pp_one)
      o.entries

  let file ?(marked = false) l =
    {
      marked;
      entries =
        List.map
          (fun (n, c) -> (n, match c with None -> Absent | Some s -> File s))
          l;
    }
end

module Set = Set.Make (Outcome)

let pp_set ppf s =
  Fmt.pf ppf "@[<v>%a@]" Fmt.(list ~sep:cut Outcome.pp) (Set.elements s)

let testable = Alcotest.testable pp_set Set.equal

(* {1 Running} *)

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

exception Power_lost

type fd = { file : Eio.File.rw_ty Eio.Resource.t; mutable off : int }

(* Run [ops] on [fs]. [fds] maps a descriptor's name to its handle; a name
   first met on a write is a file the setup made, opened without truncation. *)
let exec ~sw ~fds ~mark fs op =
  let fd name =
    match Hashtbl.find_opt fds name with
    | Some fd -> fd
    | None ->
        let file =
          (Eio.Path.open_out ~sw ~create:(`If_missing 0o644) (fs / name)
            :> Eio.File.rw_ty Eio.Resource.t)
        in
        let off = Optint.Int63.to_int (Eio.File.size file) in
        let fd = { file; off } in
        Hashtbl.replace fds name fd;
        fd
  in
  let pwrite fd off data =
    Eio.File.pwrite_all fd.file ~file_offset:(i63 off)
      [ Cstruct.of_string data ]
  in
  match op with
  | Creat name ->
      let file =
        (Eio.Path.open_out ~sw ~create:(`Or_truncate 0o644) (fs / name)
          :> Eio.File.rw_ty Eio.Resource.t)
      in
      Hashtbl.replace fds name { file; off = 0 }
  | Write (name, data) ->
      let fd = fd name in
      pwrite fd fd.off data;
      fd.off <- fd.off + String.length data
  | Pwrite (name, off, data) -> pwrite (fd name) off data
  | Rename (a, b) -> Eio.Path.rename (fs / a) (fs / b)
  | Fsync name -> Eio.File.sync (fd name).file
  | Mark -> mark ()
  | Mkdir d -> Eio.Path.mkdir ~perm:0o755 (fs / d)
  | Unlink name -> Eio.Path.unlink (fs / name)
  | Symlink { target; name } -> Eio.Path.symlink ~link_to:target (fs / name)
  | Truncate (name, len) -> Eio.File.truncate (fd name).file (i63 len)
  | Fsync_dir "" -> Eio_fs_sync.sync_dir fs
  | Fsync_dir d -> Eio_fs_sync.sync_dir (fs / d)
  | Fsync_fails name -> (
      match Eio.File.sync (fd name).file with
      | () -> failwith "the injected sync failure did not fire"
      | exception Eio.Io _ -> ())

(* The [n]th sync on a path fails for each [Fsync_fails]. *)
let schedule body =
  let syncs = Hashtbl.create 7 in
  let count name =
    let n = 1 + Option.value ~default:0 (Hashtbl.find_opt syncs name) in
    Hashtbl.replace syncs name n;
    n
  in
  List.filter_map
    (function
      | Fsync name ->
          ignore (count name);
          None
      | Fsync_fails path ->
          let n = count path in
          Some
            Chaos.Schedule.
              {
                trigger = Nth_op_on_path { n; kind = `Sync; path };
                fault = Chaos.Fault.Sync_error `Eio;
              }
      | _ -> None)
    body

let observe post name =
  let p = post / name in
  match Eio.Path.kind ~follow:false p with
  | `Not_found -> Absent
  | `Directory -> Dir
  | `Symbolic_link -> Link (Eio.Path.read_link p)
  | `Regular_file -> File (Eio.Path.load p)
  | _ -> Fmt.failwith "%s: unexpected kind" name

let run ?(block_size = 4096) ~policy ~crash_at t =
  Eio_mock.Backend.run @@ fun () ->
  Eio_fs.run @@ fun env ->
  let raw = env#fs in
  Eio.Switch.run (fun sw ->
      let fds = Hashtbl.create 7 in
      List.iter (exec ~sw ~fds ~mark:ignore raw) t.setup);
  let trigger =
    match crash_at with None -> Crash.Manual | Some n -> Crash.Nth_op n
  in
  let ctl = Crash.v ~policy ~trigger ~block_size () in
  let crashed = Crash.wrap ctl raw in
  let fs =
    match schedule t.body with
    | [] -> crashed
    | rules -> Chaos.wrap (Chaos.Schedule.scripted rules) crashed
  in
  let marked = ref false in
  let mark () = if not (Crash.is_crashed ctl) then marked := true in
  (try
     Eio.Switch.run @@ fun sw ->
     let fds = Hashtbl.create 7 in
     List.iter
       (fun op ->
         if Crash.is_crashed ctl then raise Power_lost;
         try exec ~sw ~fds ~mark fs op
         with Eio.Io _ when Crash.is_crashed ctl -> raise Power_lost)
       t.body
   with
  | Power_lost -> ()
  | Eio.Io _ when Crash.is_crashed ctl -> ());
  let post = Crash.post_crash ctl in
  let outcome =
    {
      marked = !marked;
      entries = List.map (fun n -> (n, observe post n)) t.observe;
    }
  in
  (outcome, Crash.unsynced ctl)

type exploration = { outcomes : Set.t; runs : int; last_new : int }

(* After each body operation, and at the end. A trigger past the last
   operation never fires, which is the end again. *)
let crash_points t =
  None :: List.init (List.length t.body) (fun i -> Some (i + 1))

let explore points =
  List.fold_left
    (fun acc (index, o) ->
      if Set.mem o acc.outcomes then { acc with runs = acc.runs + 1 }
      else
        {
          outcomes = Set.add o acc.outcomes;
          runs = acc.runs + 1;
          last_new = max acc.last_new index;
        })
    { outcomes = Set.empty; runs = 0; last_new = 0 }
    points

let reorder ?block_size ~seeds t =
  crash_points t
  |> List.concat_map (fun crash_at ->
      let drop = fst (run ?block_size ~policy:Drop_unsynced ~crash_at t) in
      (0, drop)
      :: List.init seeds (fun seed ->
          ( seed,
            fst (run ?block_size ~policy:(Reorder_unsynced seed) ~crash_at t) )))
  |> explore

(* Every subset of the pending changes, by sequence number. *)
let survive ?block_size t =
  crash_points t
  |> List.concat_map (fun crash_at ->
      let _, pending =
        run ?block_size ~policy:(Survive (fun _ -> true)) ~crash_at t
      in
      let seqs = List.map (fun (u : Crash.unsynced) -> u.seq) pending in
      let n = List.length seqs in
      if n > 12 then Fmt.failwith "%s: %d pending changes" t.name n;
      List.init (1 lsl n) (fun mask ->
          let keep = List.filteri (fun i _ -> mask land (1 lsl i) <> 0) seqs in
          let policy =
            Crash.Survive (fun (u : Crash.unsynced) -> List.mem u.seq keep)
          in
          (mask, fst (run ?block_size ~policy ~crash_at t))))
  |> explore

(* {1 Verdicts} *)

let seeds () =
  match Sys.getenv_opt "LITMUS_SEEDS" with
  | Some s -> int_of_string s
  | None -> 500

let report = Option.is_some (Sys.getenv_opt "LITMUS_REPORT")

let check ~name ~eio ~envelope ~ext4 ~extra ~missing ~impossible ~not_produced =
  let set = Set.of_list in
  let outside = Set.diff eio envelope in
  let weaker = Set.diff (Set.diff eio ext4) outside in
  if report then
    Fmt.epr
      "@[<v2>%s:@,\
       impossible:@,\
      \  %a@,\
       weaker than ext4:@,\
      \  %a@,\
       missing:@,\
      \  %a@,\
       not produced:@,\
      \  %a@]@."
      name pp_set outside pp_set weaker pp_set (Set.diff ext4 eio) pp_set
      (Set.diff envelope eio);
  Alcotest.check testable "impossible outcomes (eio-fs outside its envelope)"
    (set impossible) outside;
  Alcotest.check testable "deliberately weaker than ext4" (set extra) weaker;
  Alcotest.check testable "ext4 outcomes eio-fs misses" (set missing)
    (Set.diff ext4 eio);
  Alcotest.check testable "envelope outcomes eio-fs never produces"
    (set not_produced) (Set.diff envelope eio)

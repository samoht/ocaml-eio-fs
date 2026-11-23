(* The wrapper law: every interface a wrapped directory or file binds has an
   entry, and a wrapper that injects no fault does what the bare in-memory
   filesystem does, over generated sequences of operations that close, close
   twice, use after close, release a switch with resources open and cancel a
   fiber in the middle of an operation. Where the law says the wrapper
   refuses, it refuses. Under a fault, everything a run opened is released by
   the end of its switch. *)

open Eio.Std
module Chaos = Eio_fs_chaos
module Law = Chaos.Private.Law

let ( / ) = Eio.Path.( / )
let i63 = Optint.Int63.of_int

(* {1 Every binding has an entry} *)

let constructor_name (type a) (Eio.Resource.H (pi, _) : a Eio.Resource.binding)
    =
  Obj.Extension_constructor.(name (of_val pi))

(* Eio names its constructors [Eio__Fs.Pi.Dir]; the law, [Eio.Fs.Pi.Dir]. *)
let eio_name name =
  let prefix = "Eio__" in
  if String.starts_with ~prefix name then
    Some
      ("Eio."
      ^ String.sub name (String.length prefix)
          (String.length name - String.length prefix))
  else None

type seen = {
  where : string;
  handler : Law.iface;
  names : (string * [ `Interface of Law.iface | `Op of Law.t | `Other ]) list;
}

let seen where handler (Eio.Resource.T (_, ops)) =
  {
    where;
    handler;
    names =
      List.map
        (fun b -> (constructor_name b, Law.binding handler b))
        (Eio.Resource.bindings ops);
  }

let handlers root =
  Switch.run @@ fun sw ->
  let sub = Eio.Path.open_dir ~sw (root / "d") in
  [
    seen "the root" Law.Dir_handler (fst root);
    seen "a subtree" Law.Dir_handler (fst sub);
    seen "a file opened for reading" Law.File_handler
      (Eio.Path.open_in ~sw (root / "f"));
    seen "a file opened for writing" Law.File_handler
      (Eio.Path.open_out ~sw ~create:`Never (root / "f"));
  ]

let both_handlers () =
  Eio_mock.Backend.run @@ fun () ->
  let base = Eio_fs_mem.v () in
  Eio.Path.save ~create:(`Exclusive 0o644) (base / "f") "x";
  Eio.Path.mkdir ~perm:0o755 (base / "d");
  let wrapped = Chaos.wrap (fun _ -> None) base in
  (handlers base, handlers wrapped)

let bound s =
  List.filter_map (function _, `Op (e : Law.t) -> Some e | _ -> None) s.names

let interfaces s =
  List.filter_map (function _, `Interface i -> Some i | _ -> None) s.names

(* What the wrapper binds is in the law and not refused; what a bare backend
   binds of Eio's is in the law; and the wrapper binds each operation the
   law gives it. *)
let test_bindings () =
  let bare, wrapped = both_handlers () in
  List.iter
    (fun s ->
      List.iter
        (fun (name, b) ->
          match b with
          | `Other ->
              Alcotest.failf "%s binds %s, which has no entry" s.where name
          | `Op (e : Law.t) when e.cls = Unsupported ->
              Alcotest.failf "%s binds %s, which the law refuses" s.where name
          | `Op _ | `Interface _ -> ())
        s.names)
    wrapped;
  List.iter
    (fun s ->
      List.iter
        (fun (name, b) ->
          match (b, eio_name name) with
          | `Other, Some name ->
              if
                not
                  (List.exists
                     (fun (e : Law.t) -> String.equal e.name name)
                     Law.all)
              then
                Alcotest.failf "the bare %s binds %s, which has no entry"
                  s.where name
          | _ -> ())
        s.names)
    bare;
  List.iter
    (fun s ->
      let expected =
        List.filter
          (fun (e : Law.t) ->
            e.iface = s.handler && e.cls <> Unsupported
            && not (s.handler = File_handler && e.name = "close"))
          Law.all
      in
      List.iter
        (fun (e : Law.t) ->
          if not (List.memq e (bound s)) then
            Alcotest.failf "%s does not bind %s, which the law gives it" s.where
              e.name)
        expected;
      let want =
        match s.where with
        | "a file opened for reading" -> [ Law.Read ]
        | "a file opened for writing" -> [ Law.Read; Law.Write ]
        | _ -> [ Law.Dir ]
      in
      Alcotest.(check (list string))
        (s.where ^ ": its interfaces")
        (List.map (Fmt.str "%a" Law.pp_iface) want)
        (List.sort_uniq compare (interfaces s)
        |> List.map (Fmt.str "%a" Law.pp_iface)))
    wrapped

(* {1 Programs} *)

type create = Never | If_missing | Or_truncate | Exclusive

type step =
  | Open_in of int * int * string * bool  (** file, directory, path, follow *)
  | Open_out of int * int * string * create * bool  (** ..., append *)
  | Open_subtree of int * int * string  (** new directory, directory, path *)
  | Mkdir of int * string
  | Read_dir of int * string
  | Entries of int * string
  | Stat of int * string * bool
  | Unlink of int * string
  | Rmdir of int * string
  | Rename of int * string * int * string
  | Exclusive_rename of int * string * int * string
  | Read_link of int * string
  | Symlink of int * string * string  (** directory, path, target *)
  | Chmod of int * string * int
  | Chown of int * string
  | Native of int * string
  | Pp of int
  | Split of int * string
  | Join of int * string * string
  | Sync_dir of int * string
  | Dir_barrier of int * string
  | Process_local of int
  | Modeless of int
  | Adopt of int * int * string
      (** file, directory, path: a file of the base, held, adopted *)
  | Admin_stat of int * string
  | As_user_stat of int * string
  | Close_dir of int
  | Read of int * int
  | Pread of int * int * int
  | Write of int * string
  | Pwrite of int * int * string
  | Seek of int * int * [ `Set | `Cur | `End ]
  | Fstat of int
  | Fsync of int
  | File_sync of int
  | Truncate of int * int
  | Copy of int  (** a string into the file *)
  | Read_methods of int
  | Shutdown of int
  | Close of int
  | Scope of step list  (** run in a switch of its own, released at its end *)
  | Cancel of step  (** cancelled at its first suspension *)

let pp_create ppf = function
  | Never -> Fmt.string ppf "never"
  | If_missing -> Fmt.string ppf "if-missing"
  | Or_truncate -> Fmt.string ppf "or-truncate"
  | Exclusive -> Fmt.string ppf "exclusive"

let rec pp_step ppf = function
  | Open_in (f, d, p, follow) ->
      Fmt.pf ppf "f%d := open_in d%d %S%s" f d p
        (if follow then "" else " nofollow")
  | Open_out (f, d, p, c, append) ->
      Fmt.pf ppf "f%d := open_out d%d %S %a%s" f d p pp_create c
        (if append then " append" else "")
  | Open_subtree (n, d, p) -> Fmt.pf ppf "d%d := open_subtree d%d %S" n d p
  | Mkdir (d, p) -> Fmt.pf ppf "mkdir d%d %S" d p
  | Read_dir (d, p) -> Fmt.pf ppf "read_dir d%d %S" d p
  | Entries (d, p) -> Fmt.pf ppf "with_dir_entries d%d %S" d p
  | Stat (d, p, follow) ->
      Fmt.pf ppf "stat d%d %S%s" d p (if follow then "" else " nofollow")
  | Unlink (d, p) -> Fmt.pf ppf "unlink d%d %S" d p
  | Rmdir (d, p) -> Fmt.pf ppf "rmdir d%d %S" d p
  | Rename (d, p, d', p') -> Fmt.pf ppf "rename d%d %S d%d %S" d p d' p'
  | Exclusive_rename (d, p, d', p') ->
      Fmt.pf ppf "rename-exclusive d%d %S d%d %S" d p d' p'
  | Read_link (d, p) -> Fmt.pf ppf "read_link d%d %S" d p
  | Symlink (d, p, t) -> Fmt.pf ppf "symlink d%d %S -> %S" d p t
  | Chmod (d, p, perm) -> Fmt.pf ppf "chmod d%d %S %o" d p perm
  | Chown (d, p) -> Fmt.pf ppf "chown d%d %S" d p
  | Native (d, p) -> Fmt.pf ppf "native d%d %S" d p
  | Pp d -> Fmt.pf ppf "pp d%d" d
  | Split (d, p) -> Fmt.pf ppf "split d%d %S" d p
  | Join (d, a, b) -> Fmt.pf ppf "join d%d %S %S" d a b
  | Sync_dir (d, p) -> Fmt.pf ppf "sync_dir d%d %S" d p
  | Dir_barrier (d, p) -> Fmt.pf ppf "barrier d%d %S" d p
  | Process_local d -> Fmt.pf ppf "process_local d%d" d
  | Modeless d -> Fmt.pf ppf "modeless d%d" d
  | Adopt (f, d, p) -> Fmt.pf ppf "f%d := adopt d%d %S" f d p
  | Admin_stat (d, p) -> Fmt.pf ppf "admin stat d%d %S" d p
  | As_user_stat (d, p) -> Fmt.pf ppf "as_user stat d%d %S" d p
  | Close_dir d -> Fmt.pf ppf "close d%d" d
  | Read (f, n) -> Fmt.pf ppf "single_read f%d %d" f n
  | Pread (f, o, n) -> Fmt.pf ppf "pread f%d @%d %d" f o n
  | Write (f, s) -> Fmt.pf ppf "single_write f%d %S" f s
  | Pwrite (f, o, s) -> Fmt.pf ppf "pwrite f%d @%d %S" f o s
  | Seek (f, o, c) ->
      Fmt.pf ppf "seek f%d %d %s" f o
        (match c with `Set -> "set" | `Cur -> "cur" | `End -> "end")
  | Fstat f -> Fmt.pf ppf "stat f%d" f
  | Fsync f -> Fmt.pf ppf "sync f%d" f
  | File_sync f -> Fmt.pf ppf "barrier f%d" f
  | Truncate (f, n) -> Fmt.pf ppf "truncate f%d %d" f n
  | Copy f -> Fmt.pf ppf "copy \"copied\" -> f%d" f
  | Read_methods f -> Fmt.pf ppf "read_methods f%d" f
  | Shutdown f -> Fmt.pf ppf "shutdown f%d" f
  | Close f -> Fmt.pf ppf "close f%d" f
  | Scope steps ->
      Fmt.pf ppf "scope [%a]" Fmt.(list ~sep:(any "; ") pp_step) steps
  | Cancel s -> Fmt.pf ppf "cancel (%a)" pp_step s

(* {1 Running a program} *)

let slots = 3

type side = {
  dirs : Eio.Fs.dir_ty r option array;
  files : (Eio.File.ro_ty r * bool) option array; (* writable *)
  mutable opened : [ `File of Eio.File.ro_ty r | `Dir of Eio.Fs.dir_ty r ] list;
  mutable kinds : Chaos.Op.kind list;
      (* the wrapper asked about, latest first *)
  wrapped : bool;
  mutable cancels : bool list; (* per [Cancel]: the wrapper suspended *)
  base : Eio.Fs.dir_ty Eio.Path.t; (* the bare filesystem under every slot *)
  prefixes : string option array; (* where each directory is in [base] *)
}

let at d p = if d = 0 then "/" ^ p else p

type 'b on_dir = {
  dir : 'a. (module Eio.Fs.Pi.DIR with type t = 'a) -> 'a -> 'b;
}

let on_dir side d k =
  match side.dirs.(d) with
  | None -> "no directory"
  | Some (Eio.Resource.T (v, ops)) ->
      k.dir (Eio.Resource.get ops Eio.Fs.Pi.Dir) v

type 'b on_read = {
  read : 'a. (module Eio.File.Pi.READ with type t = 'a) -> 'a -> 'b;
}

type 'b on_write = {
  write : 'a. (module Eio.File.Pi.WRITE with type t = 'a) -> 'a -> 'b;
}

(* An interface is looked up among a handle's bindings, whatever its static
   type says, and a handle that binds none is refused as Eio refuses it. *)
let not_supported () = failwith "Operation not supported!"

let reader (type t) (ops : t Eio.Resource.binding list) :
    (module Eio.File.Pi.READ with type t = t) =
  match
    List.find_map
      (fun (Eio.Resource.H (pi, impl) : t Eio.Resource.binding) ->
        match pi with
        | Eio.File.Pi.Read ->
            Some (impl : (module Eio.File.Pi.READ with type t = t))
        | _ -> None)
      ops
  with
  | Some m -> m
  | None -> not_supported ()

let writer (type t) (ops : t Eio.Resource.binding list) :
    (module Eio.File.Pi.WRITE with type t = t) =
  match
    List.find_map
      (fun (Eio.Resource.H (pi, impl) : t Eio.Resource.binding) ->
        match pi with
        | Eio.File.Pi.Write ->
            Some (impl : (module Eio.File.Pi.WRITE with type t = t))
        | _ -> None)
      ops
  with
  | Some m -> m
  | None -> not_supported ()

let shutdown_side (type t) (ops : t Eio.Resource.binding list) :
    (module Eio.Flow.Pi.SHUTDOWN with type t = t) =
  match
    List.find_map
      (fun (Eio.Resource.H (pi, impl) : t Eio.Resource.binding) ->
        match pi with
        | Eio.Flow.Pi.Shutdown ->
            Some (impl : (module Eio.Flow.Pi.SHUTDOWN with type t = t))
        | _ -> None)
      ops
  with
  | Some m -> m
  | None -> not_supported ()

let on_read side f k =
  match side.files.(f) with
  | None -> "no file"
  | Some (Eio.Resource.T (v, ops), _) ->
      k.read (reader (Eio.Resource.bindings ops)) v

let on_write side f k =
  match side.files.(f) with
  | None -> "no file"
  | Some (Eio.Resource.T (v, ops), _) ->
      k.write (writer (Eio.Resource.bindings ops)) v

let string_of_create = function
  | Never -> `Never
  | If_missing -> `If_missing 0o644
  | Or_truncate -> `Or_truncate 0o644
  | Exclusive -> `Exclusive 0o644

let pp_stat ppf (s : Eio.File.Stat.t) =
  Fmt.pf ppf "%a ino=%Ld perm=%o nlink=%Ld uid=%Ld gid=%Ld size=%Ld t=%g/%g/%g"
    Eio.File.Stat.pp_kind s.kind s.ino s.perm s.nlink s.uid s.gid
    (Optint.Int63.to_int64 s.size)
    s.atime s.mtime s.ctime

let shutdown (Eio.Resource.T (v, ops)) =
  let module S = (val shutdown_side (Eio.Resource.bindings ops)) in
  S.shutdown v `All;
  "ok"

let outcome fn =
  match fn () with s -> s | exception ex -> "raised " ^ Printexc.to_string ex

let opened side x = side.opened <- x :: side.opened

let rec exec side ~sw step =
  match step with
  | Open_in (f, d, p, follow) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              let file = D.open_in v ~sw ~follow (at d p) in
              side.files.(f) <- Some (file, false);
              opened side (`File file);
              "opened");
        }
  | Open_out (f, d, p, c, append) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              let file =
                D.open_out v ~sw ~follow:true ~append
                  ~create:(string_of_create c) (at d p)
              in
              let file = (file :> Eio.File.ro_ty r) in
              side.files.(f) <- Some (file, true);
              opened side (`File file);
              "opened");
        }
  | Open_subtree (n, d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              let sub = D.open_subtree v ~sw (at d p) in
              let sub = (sub :> Eio.Fs.dir_ty r) in
              side.dirs.(n) <- Some sub;
              side.prefixes.(n) <-
                Option.map
                  (fun prefix -> if prefix = "" then p else prefix ^ "/" ^ p)
                  side.prefixes.(d);
              opened side (`Dir sub);
              "opened");
        }
  | Mkdir (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.mkdir v ~perm:0o755 (at d p);
              "ok");
        }
  | Read_dir (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              String.concat "," (D.read_dir v (at d p)));
        }
  | Entries (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.with_dir_entries v (at d p) (fun seq ->
                  List.of_seq seq
                  |> List.map (fun (k, n) ->
                      Fmt.str "%a:%s" Eio.File.Stat.pp_kind k n)
                  |> String.concat ","));
        }
  | Stat (d, p, follow) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              Fmt.str "%a" pp_stat (D.stat v ~follow (at d p)));
        }
  | Unlink (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.unlink v (at d p);
              "ok");
        }
  | Rmdir (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.rmdir v (at d p);
              "ok");
        }
  | Rename (d, p, d', p') -> (
      match side.dirs.(d') with
      | None -> "no directory"
      | Some target ->
          on_dir side d
            {
              dir =
                (fun (type a)
                  (module D : Eio.Fs.Pi.DIR with type t = a)
                  (v : a)
                ->
                  D.rename v (at d p) target (at d' p');
                  "ok");
            })
  | Exclusive_rename (d, p, d', p') -> (
      match (side.dirs.(d), side.dirs.(d')) with
      | Some src, Some dst ->
          Eio_fs.Sync.Rename.exclusive Eio_fs.Sync.Rename.capability
            (src, at d p)
            (dst, at d' p');
          "ok"
      | _ -> "no directory")
  | Read_link (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.read_link v (at d p));
        }
  | Symlink (d, p, target) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.symlink ~link_to:target v (at d p);
              "ok");
        }
  | Chmod (d, p, perm) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.chmod v ~follow:true ~perm (at d p);
              "ok");
        }
  | Chown (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.chown ~follow:false ~uid:7L ~gid:8L v (at d p);
              "ok");
        }
  | Native (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              Fmt.str "%a" Fmt.(Dump.option string) (D.native v (at d p)));
        }
  | Pp d ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              Fmt.str "%a" D.pp v);
        }
  | Split (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (_ : a) ->
              Fmt.str "%a"
                Fmt.(Dump.option (Dump.pair string string))
                (D.split (at d p)));
        }
  | Join (d, a, b) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (_ : a) ->
              D.join a b);
        }
  | Sync_dir (d, p) ->
      on_dir side d
        {
          dir =
            (fun (type a) (module D : Eio.Fs.Pi.DIR with type t = a) (v : a) ->
              D.sync_dir v (at d p);
              "ok");
        }
  | Dir_barrier (d, p) -> (
      match side.dirs.(d) with
      | None -> "no directory"
      | Some dir -> (
          match Eio_fs.Sync.dir_barrier dir with
          | None -> "no barrier"
          | Some barrier ->
              barrier ~path:(at d p) ~full:false;
              "ok"))
  | Process_local d -> (
      match side.dirs.(d) with
      | None -> "no directory"
      | Some dir -> string_of_bool (Eio_fs.Sync.is_process_local dir))
  | Modeless d -> (
      match side.dirs.(d) with
      | None -> "no directory"
      | Some dir -> string_of_bool (Eio_fs.Sync.is_modeless dir))
  | Adopt (f, d, p) -> (
      match (side.dirs.(d), side.prefixes.(d)) with
      | Some dir, Some prefix ->
          let held =
            Eio.Path.open_out ~sw ~create:`Never
              Eio.Path.(side.base / if prefix = "" then p else prefix ^ "/" ^ p)
          in
          let file =
            Eio_fs.Sync.adopt ~sw
              (dir, at d p)
              (held :> Eio.File.rw_ty Eio.Resource.t)
          in
          let file = (file :> Eio.File.ro_ty r) in
          side.files.(f) <- Some (file, true);
          opened side (`File file);
          "adopted"
      | _ -> "no directory")
  | Admin_stat (d, p) -> (
      match side.dirs.(d) with
      | None -> "no directory"
      | Some dir -> (
          match Eio_fs_internal.admin dir with
          | None -> "no admin"
          | Some admin ->
              Fmt.str "%a" pp_stat (Eio.Path.stat ~follow:false (admin, at d p))
          ))
  | As_user_stat (d, p) -> (
      match side.dirs.(d) with
      | None -> "no directory"
      | Some dir ->
          let user = Eio_fs_mem.as_user Eio_fs_mem.root (dir, at d p) in
          Fmt.str "%a" pp_stat (Eio.Path.stat ~follow:false user))
  | Close_dir d -> (
      match side.dirs.(d) with
      | None -> "no directory"
      | Some (Eio.Resource.T (v, ops)) -> (
          match Eio.Resource.get_opt ops Eio.Resource.Close with
          | Some close ->
              close v;
              "closed"
          | None -> "no close"))
  | Read (f, n) ->
      on_read side f
        {
          read =
            (fun (type a)
              (module R : Eio.File.Pi.READ with type t = a)
              (v : a)
            ->
              let buf = Cstruct.create n in
              let got = R.single_read v buf in
              Cstruct.to_string ~len:got buf);
        }
  | Pread (f, o, n) ->
      on_read side f
        {
          read =
            (fun (type a)
              (module R : Eio.File.Pi.READ with type t = a)
              (v : a)
            ->
              let buf = Cstruct.create n in
              let got = R.pread v ~file_offset:(i63 o) [ buf ] in
              Cstruct.to_string ~len:got buf);
        }
  | Write (f, s) ->
      on_write side f
        {
          write =
            (fun (type a)
              (module W : Eio.File.Pi.WRITE with type t = a)
              (v : a)
            -> string_of_int (W.single_write v [ Cstruct.of_string s ]));
        }
  | Pwrite (f, o, s) ->
      on_write side f
        {
          write =
            (fun (type a)
              (module W : Eio.File.Pi.WRITE with type t = a)
              (v : a)
            ->
              string_of_int
                (W.pwrite v ~file_offset:(i63 o) [ Cstruct.of_string s ]));
        }
  | Seek (f, o, c) ->
      on_read side f
        {
          read =
            (fun (type a)
              (module R : Eio.File.Pi.READ with type t = a)
              (v : a)
            -> Optint.Int63.to_string (R.seek v (i63 o) c));
        }
  | Fstat f ->
      on_read side f
        {
          read =
            (fun (type a)
              (module R : Eio.File.Pi.READ with type t = a)
              (v : a)
            -> Fmt.str "%a" pp_stat (R.stat v));
        }
  | Fsync f ->
      on_write side f
        {
          write =
            (fun (type a)
              (module W : Eio.File.Pi.WRITE with type t = a)
              (v : a)
            ->
              W.sync v;
              "ok");
        }
  | File_sync f -> (
      match side.files.(f) with
      | None -> "no file"
      | Some (file, _) -> (
          match Eio_fs.Sync.file_barrier file with
          | Some sync ->
              sync ~full:false;
              "ok"
          | None -> exec side ~sw (Fsync f)))
  | Truncate (f, n) ->
      on_write side f
        {
          write =
            (fun (type a)
              (module W : Eio.File.Pi.WRITE with type t = a)
              (v : a)
            ->
              W.truncate v (i63 n);
              "ok");
        }
  | Copy f ->
      on_write side f
        {
          write =
            (fun (type a)
              (module W : Eio.File.Pi.WRITE with type t = a)
              (v : a)
            ->
              W.copy v ~src:(Eio.Flow.string_source "copied");
              "ok");
        }
  | Read_methods f ->
      on_read side f
        {
          read =
            (fun (type a)
              (module R : Eio.File.Pi.READ with type t = a)
              (_ : a)
            -> string_of_int (List.length R.read_methods));
        }
  | Shutdown f -> (
      match side.files.(f) with
      | None -> "no file"
      | Some (file, _) -> shutdown file)
  | Close f -> (
      match side.files.(f) with
      | None -> "no file"
      | Some (file, _) ->
          Eio.Resource.close file;
          "closed")
  | Scope steps ->
      Switch.run (fun sw -> List.iter (run_step side ~sw) steps);
      "released"
  | Cancel step -> cancel side ~sw step

and cancel side ~sw step =
  let result = ref "not run" in
  let suspends =
    if side.wrapped then true
    else
      match side.cancels with
      | s :: rest ->
          side.cancels <- rest;
          s
      | [] -> false
  in
  let before = List.length side.kinds in
  (try
     Switch.run @@ fun inner ->
     Fiber.fork ~sw:inner (fun () ->
         result :=
           outcome (fun () ->
               if (not side.wrapped) && suspends then Fiber.yield ();
               exec side ~sw step));
     Switch.fail inner Exit
   with Exit -> ());
  if side.wrapped then
    side.cancels <- side.cancels @ [ List.length side.kinds > before ];
  !result

and run_step side ~sw step =
  match step with
  | Scope _ -> ignore (exec side ~sw step)
  | _ -> ignore (outcome (fun () -> exec side ~sw step))

(* {1 What a step is, by the law} *)

let file_writable side f =
  match side.files.(f) with Some (_, w) -> w | None -> true

let rec entry_of side step =
  let op iface name = Some (Law.get iface name) in
  let write f name =
    if file_writable side f then op Write name
    else op File_handler "write side of a file opened for reading"
  in
  match step with
  | Open_in _ -> op Dir "open_in"
  | Open_out _ -> op Dir "open_out"
  | Open_subtree _ -> op Dir "open_subtree"
  | Mkdir _ -> op Dir "mkdir"
  | Read_dir _ -> op Dir "read_dir"
  | Entries _ -> op Dir "with_dir_entries"
  | Stat _ -> op Dir "stat"
  | Unlink _ -> op Dir "unlink"
  | Rmdir _ -> op Dir "rmdir"
  | Rename _ -> op Dir "rename"
  | Exclusive_rename _ -> op Dir_handler "Eio_fs.Sync.Rename.Exclusive"
  | Read_link _ -> op Dir "read_link"
  | Symlink _ -> op Dir "symlink"
  | Chmod _ -> op Dir "chmod"
  | Chown _ -> op Dir "chown"
  | Native _ -> op Dir "native"
  | Pp _ -> op Dir "pp"
  | Split _ -> op Dir "split"
  | Join _ -> op Dir "join"
  | Sync_dir _ -> op Dir "sync_dir"
  | Dir_barrier _ -> op Dir_handler "Eio_fs.Sync.Dir_sync"
  | Process_local _ -> op Dir_handler "Eio_fs.Sync.Process_local"
  | Modeless _ -> op Dir_handler "Eio_fs.Sync.Modeless"
  | Adopt _ -> op Dir_handler "Eio_fs.Sync.Adopt"
  | Admin_stat _ -> op Dir_handler "Eio_fs_internal.Admin"
  | As_user_stat _ -> op Dir_handler "Eio_fs_internal.Rebase"
  | Close_dir _ -> op Dir_handler "close"
  | Read _ -> op Read "single_read"
  | Pread _ -> op Read "pread"
  | Seek _ -> op Read "seek"
  | Fstat _ -> op Read "stat"
  | Read_methods _ -> op Read "read_methods"
  | Close _ -> op Read "close"
  | Write (f, _) -> write f "single_write"
  | Pwrite (f, _, _) -> write f "pwrite"
  | Fsync f -> write f "sync"
  | Truncate (f, _) -> write f "truncate"
  | Copy f -> write f "copy"
  | File_sync _ -> op File_handler "Eio_fs.Sync.File_sync"
  | Shutdown _ -> op File_handler "Eio.Flow.Pi.Shutdown"
  | Cancel s -> entry_of side s
  | Scope _ -> None

(* {1 Comparing a wrapped run with a bare one} *)

type record = {
  steps : (string * string * Law.t option * Chaos.Op.kind list) list;
      (* step, outcome, its entry, what the wrapper asked about *)
  tree : string list;
  leaked : string list;
}

let rec walk (dir : _ Eio.Path.t) prefix acc =
  List.fold_left
    (fun acc name ->
      let p = dir / name and path = prefix ^ "/" ^ name in
      let s = Eio.Path.stat ~follow:false p in
      let line what = Fmt.str "%s %a %s" path pp_stat s what in
      match s.kind with
      | `Directory -> walk p path (line "" :: acc)
      | `Symbolic_link -> line ("-> " ^ Eio.Path.read_link p) :: acc
      | `Regular_file -> line (String.escaped (Eio.Path.load p)) :: acc
      | _ -> line "?" :: acc)
    acc
    (List.sort compare (Eio.Path.read_dir dir))

let still_open = function
  | `File f -> (
      match Eio.File.stat f with
      | _ -> true
      | exception Invalid_argument _ -> false)
  | `Dir d -> (
      match Eio.Path.stat ~follow:true (d, "") with
      | _ -> true
      | exception Invalid_argument _ -> false)

let record_run ?(fault = fun _ -> None) ?(crash = false) ~wrapped
    ?(cancels = []) program =
  Eio_mock.Backend.run @@ fun () ->
  let base = Eio_fs_mem.v () in
  let side =
    {
      dirs = Array.make slots None;
      files = Array.make slots None;
      opened = [];
      kinds = [];
      wrapped;
      cancels;
      base :> Eio.Fs.dir_ty Eio.Path.t;
      prefixes = Array.init slots (fun i -> if i = 0 then Some "" else None);
    }
  in
  let live = ref true in
  let root =
    if wrapped then
      let hook (op : Chaos.Op.t) =
        side.kinds <- op.kind :: side.kinds;
        Fiber.yield ();
        if !live then fault op else None
      in
      Chaos.wrap hook
        (if crash then Chaos.Private.Crash.wrap (Chaos.Private.Crash.v ()) base
         else base)
    else (base :> Eio.Fs.dir_ty Eio.Path.t)
  in
  side.dirs.(0) <- Some (fst root);
  let steps = ref [] in
  let rec go ~sw step =
    match step with
    | Scope inner -> Switch.run (fun sw -> List.iter (go ~sw) inner)
    | _ ->
        let entry = entry_of side step in
        side.kinds <- [];
        let o = outcome (fun () -> exec side ~sw step) in
        let entry =
          match o with "no file" | "no directory" -> None | _ -> entry
        in
        steps :=
          (Fmt.str "%a" pp_step step, o, entry, List.rev side.kinds) :: !steps
  in
  Switch.run (fun sw -> List.iter (go ~sw) program);
  live := false;
  let leaked =
    List.filter_map
      (fun x ->
        if still_open x then
          Some (match x with `File _ -> "a file" | `Dir _ -> "a directory")
        else None)
      side.opened
  in
  let leaked =
    match Chaos.Private.Internal.descriptors base with
    | Some 0 | None | Some 3 -> leaked
    | Some n -> Fmt.str "%d descriptors" n :: leaked
  in
  let tree = List.rev (walk (Eio.Path.( / ) base "") "" []) in
  ({ steps = List.rev !steps; tree; leaked }, side.cancels)

let refused = "raised Failure(\"Operation not supported!\")"

(* The wrapper asks the schedule as its entry says: never for an operation
   it forwards or refuses, and once, with the entry's kind, for one it
   intercepts, unless the operation failed before it reached the wrapper's
   hook. *)
let check_kinds name (step, outcome, entry, kinds) =
  match (entry : Law.t option) with
  | None -> ()
  | Some e -> (
      let failed = String.starts_with ~prefix:"raised" outcome in
      match (e.cls, kinds) with
      | (Forwarded | Unsupported), [] -> ()
      | Intercepted k, [ k' ] when k = k' -> ()
      | Intercepted _, [] when failed -> ()
      | Synthesized, _ -> ()
      | _ ->
          Alcotest.failf
            "%s: %s is %a but the wrapper asked about %d operations" name step
            Law.pp_cls e.cls (List.length kinds))

let compare_runs ~crash name program =
  let name = if crash then name ^ ", over a crash model" else name in
  let wrapped, cancels = record_run ~crash ~wrapped:true program in
  let bare, _ = record_run ~wrapped:false ~cancels program in
  List.iter (check_kinds name) wrapped.steps;
  List.iter2
    (fun (step, b, _, _) (_, w, entry, _) ->
      match (entry : Law.t option) with
      | Some { cls = Unsupported; _ } ->
          if not (String.equal w refused) then
            Alcotest.failf "%s: %s is unsupported, but the wrapper answered %s"
              name step w
      | _ ->
          if not (String.equal b w) then
            Alcotest.failf "%s: %s\n  bare:    %s\n  wrapped: %s\nin %a" name
              step b w
              Fmt.(Dump.list pp_step)
              program)
    bare.steps wrapped.steps;
  Alcotest.(check (list string)) (name ^ ": the tree") bare.tree wrapped.tree;
  Alcotest.(check (list string)) (name ^ ": nothing left open") [] bare.leaked;
  Alcotest.(check (list string))
    (name ^ ": nothing left open through the wrapper")
    [] wrapped.leaked;
  List.filter_map (fun (_, _, e, _) -> e) wrapped.steps

(* {1 Generated programs} *)

let setup =
  [
    Open_out (0, 0, "f", Exclusive, false);
    Write (0, "hello");
    Close 0;
    Mkdir (0, "d");
    Open_out (0, 0, "d/f", Exclusive, false);
    Write (0, "inner");
    Close 0;
    Symlink (0, "l", "f");
  ]

(* Each kind of handle, opened in slot 0. *)
let openers =
  [
    ("read-only file", `File, Open_in (0, 0, "f", true));
    ("read-write file", `File, Open_out (0, 0, "f", Never, false));
    ("appending file", `File, Open_out (0, 0, "f", Never, true));
    ("subtree", `Dir, Open_subtree (1, 0, "d"));
  ]

let file_uses f =
  [
    Read (f, 3);
    Read (f, 0);
    Pread (f, 1, 3);
    Write (f, "ab");
    Pwrite (f, 2, "xyz");
    Seek (f, 1, `Set);
    Seek (f, 0, `End);
    Fstat f;
    Fsync f;
    File_sync f;
    Truncate (f, 2);
    Copy f;
    Read_methods f;
    Shutdown f;
    Close f;
  ]

let dir_uses d =
  [
    Open_in (2, d, "f", true);
    Open_out (2, d, "g", If_missing, false);
    Adopt (2, d, "f");
    Open_subtree (2, d, ".");
    Mkdir (d, "e");
    Read_dir (d, ".");
    Entries (d, ".");
    Stat (d, "f", true);
    Unlink (d, "f");
    Rmdir (d, "e");
    Rename (d, "f", d, "h");
    Exclusive_rename (d, "f", d, "h");
    Read_link (d, "f");
    Symlink (d, "k", "f");
    Chmod (d, "f", 0o600);
    Chown (d, "f");
    Native (d, "f");
    Pp d;
    Split (d, "a/b");
    Join (d, "a", "b");
    Sync_dir (d, ".");
    Dir_barrier (d, ".");
    Process_local d;
    Modeless d;
    Admin_stat (d, "f");
    As_user_stat (d, "f");
    Close_dir d;
  ]

(* Every handle, after each lifecycle edge, meets each operation it has. *)
let lifecycle =
  List.concat_map
    (fun (what, kind, opener) ->
      let uses = match kind with `File -> file_uses 0 | `Dir -> dir_uses 1 in
      let close = match kind with `File -> Close 0 | `Dir -> Close_dir 1 in
      let edges =
        [
          ("open", [ opener ]);
          ("closed", [ opener; close ]);
          ("closed twice", [ opener; close; close ]);
          ("released", [ Scope [ opener ] ]);
          ("opened while cancelled", [ Cancel opener ]);
        ]
      in
      List.concat_map
        (fun (edge, steps) ->
          List.concat_map
            (fun use ->
              List.map
                (fun use ->
                  ( Fmt.str "%s %s, then %a" what edge pp_step use,
                    setup
                    @ [ Open_out (1, 0, "g", If_missing, false) ]
                    @ steps @ [ use ] ))
                [ use; Cancel use ])
            uses)
        edges)
    openers

let paths = [| "f"; "g"; "d"; "d/f"; "l"; "x"; ".." |]

(* The root takes most directory operations, so that more of them reach a
   directory; an open is the likeliest step, so that more reach a file. *)
let rec gen_step st ~depth =
  let int n = Random.State.int st n in
  let pick a = a.(int (Array.length a)) in
  let f () = int slots and p () = pick paths in
  let d () = if int 3 > 0 then 0 else 1 + int (slots - 1) in
  match int 50 with
  | 0 | 1 | 2 | 3 -> Open_in (f (), d (), p (), Random.State.bool st)
  | 4 | 5 | 6 | 7 ->
      Open_out
        ( f (),
          d (),
          p (),
          pick [| Never; If_missing; Or_truncate; Exclusive |],
          Random.State.bool st )
  | 8 | 9 | 10 ->
      Open_subtree (1 + int (slots - 1), d (), pick [| "d"; "."; "x" |])
  | 11 -> Mkdir (d (), p ())
  | 12 -> Read_dir (d (), pick [| "."; "d"; "x" |])
  | 13 -> Entries (d (), pick [| "."; "d" |])
  | 14 -> Stat (d (), p (), Random.State.bool st)
  | 15 -> Unlink (d (), p ())
  | 16 -> Rmdir (d (), p ())
  | 17 -> Rename (d (), p (), d (), p ())
  | 18 -> Exclusive_rename (d (), p (), d (), p ())
  | 19 -> Read_link (d (), p ())
  | 20 -> Symlink (d (), p (), p ())
  | 21 -> Chmod (d (), p (), pick [| 0o600; 0o755; 0o000 |])
  | 22 -> Chown (d (), p ())
  | 23 -> Native (d (), p ())
  | 24 -> Pp (d ())
  | 25 -> Sync_dir (d (), pick [| "."; "d"; "x" |])
  | 26 -> Admin_stat (d (), p ())
  | 27 -> Close_dir (1 + int (slots - 1))
  | 28 | 29 -> Read (f (), int 6)
  | 30 -> Pread (f (), int 8, int 6)
  | 31 | 32 -> Write (f (), pick [| ""; "a"; "bcd" |])
  | 33 -> Pwrite (f (), int 8, pick [| "a"; "bcd" |])
  | 34 -> Seek (f (), int 4, pick [| `Set; `Cur; `End |])
  | 35 -> Fstat (f ())
  | 36 -> Fsync (f ())
  | 37 -> File_sync (f ())
  | 38 -> Truncate (f (), int 8)
  | 39 -> Copy (f ())
  | 40 -> Shutdown (f ())
  | 41 | 42 -> Close (f ())
  | (43 | 44) when depth < 2 ->
      Scope (List.init (1 + int 5) (fun _ -> gen_step st ~depth:(depth + 1)))
  | (45 | 46 | 47) when depth < 2 -> Cancel (gen_step st ~depth:2)
  | 48 -> Process_local (d ())
  | 49 -> Modeless (d ())
  | _ -> Read_methods (f ())

let generated =
  List.init 400 (fun seed ->
      let st = Random.State.make [| seed |] in
      let program =
        List.init (4 + Random.State.int st 16) (fun _ -> gen_step st ~depth:0)
      in
      (Fmt.str "seed %d" seed, setup @ program))

(* The internal interfaces no caller outside this package can name, which
   [test_bindings] checks are bound, and the one Eio hides. *)
let not_driven =
  [
    "Eio_fs_internal.Base_dir";
    "Eio_fs_internal.Base_file";
    "Eio_fs_internal.Sync_failed";
    "Eio.Net.Pi.Socket";
    "a base's own interface";
  ]

(* A crash model with no cut forwards each operation too, so the two
   wrappers stacked do what the bare filesystem does. *)
let both name program =
  ignore (compare_runs ~crash:true name program);
  compare_runs ~crash:false name program

let test_lifecycle () =
  let reached =
    List.concat_map (fun (name, program) -> both name program) lifecycle
  in
  List.iter
    (fun (e : Law.t) ->
      if (not (List.memq e reached)) && not (List.mem e.name not_driven) then
        Alcotest.failf "no program reaches %a" Law.pp e)
    Law.all

let test_generated () =
  List.iter (fun (name, program) -> ignore (both name program)) generated

(* {1 Under a fault} *)

let faults : Chaos.Op.kind -> Chaos.Fault.t list = function
  | `Read -> [ Fail `Eio; Short 1 ]
  | `Read_dir -> [ Fail `Eio ]
  | `Write -> [ Fail `Enospc; Short 1; Torn 1 ]
  | `Truncate -> [ Fail `Eio ]
  | `Sync -> [ Fail `Eio; Evict `Eio ]
  | `Open_read | `Open_write -> [ Fail `Eacces; Swap_symlink "g" ]
  | `Stat -> [ Fail `Eio; Swap_symlink "g" ]
  | `Change -> [ Fail `Erofs ]

(* Each fault each operation of the use takes, injected there: whatever it
   cut short, nothing the run opened outlives its switch. *)
let test_released_under_faults () =
  List.iter
    (fun (name, program) ->
      let clean, _ = record_run ~wrapped:true program in
      let kinds = List.concat_map (fun (_, _, _, k) -> k) clean.steps in
      let before =
        List.length kinds
        - List.length
            (match List.rev clean.steps with
            | (_, _, _, k) :: _ -> k
            | [] -> [])
      in
      List.iteri
        (fun i kind ->
          if i >= before then
            List.iter
              (fun fault ->
                let n = ref 0 in
                let inject _ =
                  incr n;
                  if !n = i + 1 then Some fault else None
                in
                let run, _ = record_run ~fault:inject ~wrapped:true program in
                if run.leaked <> [] then
                  Alcotest.failf "%s, %a at operation %d: left open %a" name
                    Chaos.Fault.pp fault (i + 1)
                    Fmt.(Dump.list string)
                    run.leaked)
              (faults kind))
        kinds)
    lifecycle

let suite =
  ( "law",
    [
      Alcotest.test_case "every binding has an entry" `Quick test_bindings;
      Alcotest.test_case "each handle after each lifecycle edge" `Quick
        test_lifecycle;
      Alcotest.test_case "generated programs" `Quick test_generated;
      Alcotest.test_case "released under faults" `Quick
        test_released_under_faults;
    ] )

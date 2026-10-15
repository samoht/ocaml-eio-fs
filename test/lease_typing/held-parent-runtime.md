# Held-parent runtime contract

These contracts use only public Eio capabilities. The fixture runs against
native and descriptorless backends independently. Stat identity means the
backend's (device,inode,kind), never an invented descriptor identity.

```ocaml
# module Fixture : sig
    val native : (Eio.Fs.dir_ty Eio.Path.t -> unit) -> unit
    val memory : (Eio.Fs.dir_ty Eio.Path.t -> unit) -> unit
  end = struct
    let native fn = Eio_main.run @@ fun env ->
      let name = Filename.temp_file "held-parent-runtime" "" in
      Sys.remove name;
      let path = Eio.Path.(Eio.Stdenv.fs env / name) in
      Eio.Path.mkdir ~perm:0o700 path;
      Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true path)
        (fun () -> Eio.Path.with_open_dir path (fun root -> fn (root :> Eio.Fs.dir_ty Eio.Path.t)))
    let memory fn = Eio_mock.Backend.run @@ fun () ->
      Eio_fs.run @@ fun env -> fn env#fs
  end;;
module Fixture :
  sig
    val native : (Eio.Fs.dir_ty Eio.Path.t -> unit) -> unit
    val memory : (Eio.Fs.dir_ty Eio.Path.t -> unit) -> unit
  end
# module Contract (P : sig
    val with_parent : _ Eio.Path.t -> (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
  end) : sig val run : Eio.Fs.dir_ty Eio.Path.t -> unit end = struct
    let ( / ) = Eio.Path.( / )
    let require message b = if not b then failwith message
    let identity p = let s = Eio.Path.stat ~follow:false p in s.dev, s.ino, s.kind
    let io f = match f () with
      | () -> false
      | exception Eio.Io _ -> true
    let denied f = match f () with
      | () -> false
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) -> true
    let run root =
      Eio.Path.mkdir ~perm:0o700 (root / "parent");
      Eio.Path.save ~create:(`Exclusive 0o600) (root / "parent/leaf") "original";
      let original = identity (root / "parent/leaf") in
      let token = ref "callback return identity" in
      let borrowed = ref None in
      let result = P.with_parent (root / "parent/leaf") (fun leaf ->
        borrowed := Some leaf;
        require "nested leaf identity" (identity leaf = original);
        require "nested leaf content" (Eio.Path.load leaf = "original"); token) in
      require "callback result physical identity" (result == token);
      let expired () = match !borrowed with
        | None -> failwith "callback was not entered"
        | Some leaf -> require "borrowed authority expired" (match Eio.Path.load leaf with
            | _ -> false | exception _ -> true) in
      expired ();
      let exception_marker = Failure (Bytes.to_string (Bytes.of_string "callback exception identity")) in
      require "exception physical identity" (match
        P.with_parent (root / "parent/leaf") (fun leaf -> borrowed := Some leaf; raise exception_marker)
        with _ -> false | exception ex -> ex == exception_marker);
      expired ();
      require "caller root authority survives both exits" (identity (root / "parent/leaf") = original);
      List.iter (fun raw ->
        let called = ref false in
        let base, _ = root in
        require ("typed invalid path refusal: " ^ String.escaped raw)
          (io (fun () -> P.with_parent (base, raw) (fun _ -> called := true)));
        require "invalid path did not call callback" (not !called))
        [ ""; "/parent/leaf"; "."; ".."; "parent/./leaf";
          "parent/../leaf"; "parent//leaf"; "parent/leaf\000" ];
      require "invalid paths preserve original content" (Eio.Path.load (root / "parent/leaf") = "original");
      let called = ref false in
      require "missing parent typed Not_found" (match
        P.with_parent (root / "missing/leaf") (fun _ -> called := true)
        with () -> false
        | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true);
      require "missing parent no callback" (not !called);
      Eio.Path.save ~create:(`Exclusive 0o600) (root / "blocked") "blocked";
      Eio.Path.mkdir ~perm:0o700 (root / "outside");
      Eio.Path.save ~create:(`Exclusive 0o600) (root / "outside/leaf") "outside";
      let outside = identity (root / "outside/leaf") in
      Eio.Path.symlink ~link_to:"outside" (root / "link");
      List.iter (fun name ->
        called := false;
        require "blocked parent typed Permission_denied" (denied (fun () ->
          P.with_parent (root / (name ^ "/leaf")) (fun _ -> called := true)));
        require "blocked parent no callback" (not !called)) [ "blocked"; "link" ];
      Eio.Path.symlink ~link_to:"../outside/leaf" (root / "parent/final-link");
      let link = identity (root / "parent/final-link") in
      P.with_parent (root / "parent/final-link") (fun leaf ->
        require "final link own lstat identity" (identity leaf = link);
        require "final link remains symbolic" (let _, _, kind = identity leaf in kind = `Symbolic_link));
      P.with_parent (root / "parent/leaf") (fun leaf ->
        Eio.Path.rename (root / "parent") (root / "held-original");
        Eio.Path.symlink ~link_to:"outside" (root / "parent");
        require "renamed parent retains original leaf identity" (identity leaf = original);
        Eio.Path.save ~create:`Never leaf "held write");
      require "held write reached original authority" (Eio.Path.load (root / "held-original/leaf") = "held write");
      require "replacement outside inode survives" (identity (root / "outside/leaf") = outside);
      require "replacement outside content survives" (Eio.Path.load (root / "outside/leaf") = "outside");
      Eio.Path.mkdir ~perm:0o700 (root / "observed");
      Eio.Path.save ~create:(`Exclusive 0o600) (root / "observed/leaf") "observed";
      let observed_leaf = identity (root / "observed/leaf") in
      let swapped = ref false in
      let schedule = Eio_fs.Chaos.Schedule.observing (fun (op : Eio_fs.Chaos.Op.t) ->
        if op.kind = `Open && op.path = "observed" && not !swapped then begin
          swapped := true;
          Eio.Path.rename (root / "observed") (root / "observed-original");
          Eio.Path.mkdir ~perm:0o700 (root / "observed");
          Eio.Path.save ~create:(`Exclusive 0o600) (root / "observed/leaf") "replacement"
        end) Eio_fs.Chaos.Schedule.empty in
      let wrapped = Eio_fs.Chaos.wrap schedule root in
      called := false;
      require "observed/open identity mismatch denied" (denied (fun () ->
        P.with_parent (wrapped / "observed/leaf") (fun _ -> called := true)));
      require "deterministic parent-open rendezvous executed" !swapped;
      require "mismatch never enters callback" (not !called);
      require "observed original leaf identity preserved" (identity (root / "observed-original/leaf") = observed_leaf);
      require "observed original content preserved" (Eio.Path.load (root / "observed-original/leaf") = "observed");
      require "replacement content preserved" (Eio.Path.load (root / "observed/leaf") = "replacement")
  end;;
module Contract :
  (P : sig
         val with_parent :
           [> Eio.Fs.dir_ty ] Eio.Path.t ->
           (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
       end)
    -> sig val run : Eio.Fs.dir_ty Eio.Path.t -> unit end
```

Existing public fixture controls execute even on the pre-source API. They
check actual I/O on each backend, rather than claiming execution from typing.

```ocaml
# let control root =
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(root / "control") "control";
    assert (Eio.Path.load Eio.Path.(root / "control") = "control");;
val control : [> Eio.Fs.dir_ty ] Eio.Path.t -> unit = <fun>
# Fixture.native control;;
- : unit = ()
# Fixture.memory control;;
- : unit = ()
```

The following named assertions must become green without changing expected
outputs. An absent primitive is an assertion diff, never the expected result.

```ocaml
# module Runtime = Contract (Eio_fs_unix.Batch);;
module Runtime : sig val run : Eio.Fs.dir_ty Eio.Path.t -> unit end
# Fixture.native Runtime.run;;
- : unit = ()
# Fixture.memory Runtime.run;;
- : unit = ()
```

Descriptorless Batch must accept its already-held root without reopening the
empty path. A hard-link refusal must leave the source inode and content alone
and create no destination. This is a real in-memory backend, not an OS-fd fake.

```ocaml
# Fixture.memory (fun root ->
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(root / "source") "source";
    let before = Eio.Path.stat ~follow:false Eio.Path.(root / "source") in
    let empty_open = ref false in
    let schedule = Eio_fs.Chaos.Schedule.observing (fun (op : Eio_fs.Chaos.Op.t) ->
      if op.kind = `Open && op.path = "" then empty_open := true)
      Eio_fs.Chaos.Schedule.empty in
    let wrapped = Eio_fs.Chaos.wrap schedule root in
    Eio.Switch.run (fun sw ->
      let batch = Eio_fs_unix.Batch.v ~sw wrapped in
      assert (match Eio_fs_unix.Batch.run batch
        [Eio_fs_unix.Batch.Link { src = "source"; dst = "destination" }] with
        | [Error (`Refused _)] -> true | _ -> false));
    assert (not !empty_open);
    let after = Eio.Path.stat ~follow:false Eio.Path.(root / "source") in
    assert ((before.dev, before.ino, before.kind) = (after.dev, after.ino, after.kind));
    assert (Eio.Path.load Eio.Path.(root / "source") = "source");
    assert (match Eio.Path.stat ~follow:false Eio.Path.(root / "destination") with
      | _ -> false | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> true));;
- : unit = ()
```

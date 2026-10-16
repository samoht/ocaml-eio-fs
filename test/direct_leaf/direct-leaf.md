# Direct-leaf borrowed authority

A direct child has no parent traversal, but its callback capability is still
borrowed for the callback scope. The caller retains its root authority after
both normal and exceptional exits. Identity checks cover the original leaf;
content checks also catch modifications that preserve its inode.

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
# let direct_leaf : Eio.Fs.dir_ty Eio.Path.t -> unit = fun root ->
    let ( / ) = Eio.Path.( / ) in
    let require message condition = if not condition then failwith message in
    let identity path =
      let s = Eio.Path.stat ~follow:false path in s.dev, s.ino, s.kind in
    let original_path = root / "leaf" in
    Eio.Path.save ~create:(`Exclusive 0o600) original_path "original direct leaf";
    let original = identity original_path in
    let captured = ref None in
    let inspect leaf =
      captured := Some leaf;
      require "direct callback original dev/ino/kind" (identity leaf = original);
      require "direct callback original content"
        (Eio.Path.load leaf = "original direct leaf") in
    let expired exit = match !captured with
      | None -> failwith "direct callback was not entered"
      | Some leaf -> require ("direct borrowed capability expires after " ^ exit)
          (match Eio.Path.load leaf with _ -> false | exception _ -> true) in
    let caller_survives exit =
      require ("caller root retains original dev/ino/kind after " ^ exit)
        (identity original_path = original);
      require ("caller root retains original content after " ^ exit)
        (Eio.Path.load original_path = "original direct leaf") in
    let token = ref ["arbitrary callback result"] in
    let result = Eio_fs_unix.Batch.with_parent original_path (fun leaf ->
      inspect leaf; token) in
    require "direct callback result physical identity" (result == token);
    expired "normal return";
    caller_survives "normal return";
    captured := None;
    let exception_token = Failure (Bytes.to_string (Bytes.of_string "direct exit token")) in
    require "direct callback exception physical identity"
      (match Eio_fs_unix.Batch.with_parent original_path (fun leaf ->
         inspect leaf; raise exception_token)
       with _ -> false | exception ex -> ex == exception_token);
    expired "raised exit";
    caller_survives "raised exit";;
val direct_leaf : Eio.Fs.dir_ty Eio.Path.t -> unit = <fun>
# Fixture.native direct_leaf;;
- : unit = ()
# Fixture.memory direct_leaf;;
- : unit = ()
```

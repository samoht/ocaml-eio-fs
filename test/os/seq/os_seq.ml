(* Each operation is a line of words. A word [{s}n] is [s] repeated [n]
   times; [-] is the empty string. *)

type ids = { gid : int; other_gid : int }

(* NFC and NFD spellings of e-acute, and other names a fold changes. *)
let e_nfc = "\xc3\xa9"
let e_upper = "\xc3\x89"
let e_nfd = "e\xcc\x81"
let sharp_s = "\xc3\x9f"
let kelvin = "\xe2\x84\xaa"
let e_upper_nfd = "E\xcc\x81"
let smiley = "\xf0\x9f\x98\x80"

let scenarios =
  [
    (* Names: case and Unicode normalisation. *)
    ("case lookup", [ "write a x"; "stat A"; "read A"; "ls ." ]);
    ("case exclusive create", [ "write a x"; "open A x"; "ls ." ]);
    ("case create", [ "write a x"; "write A y"; "read a"; "ls ." ]);
    ("case mkdir", [ "mkdir d"; "mkdir D"; "write d/f x"; "ls D"; "ls ." ]);
    ("case rename", [ "write a x"; "rename a A"; "ls ."; "read a" ]);
    ("case rename dir", [ "mkdir d"; "rename d D"; "ls ." ]);
    ( "case rename over",
      [ "write a x"; "write b y"; "rename b A"; "ls ."; "read a" ] );
    ("case rename dir over", [ "mkdir a"; "mkdir B"; "rename a b"; "ls ." ]);
    ("case unlink", [ "write a x"; "unlink A"; "ls ." ]);
    ("case rmdir", [ "mkdir d"; "rmdir D"; "ls ." ]);
    ("case symlink", [ "symlink a l"; "symlink b L"; "readlink L"; "ls ." ]);
    ("case component", [ "mkdir d"; "write D/f x"; "ls d" ]);
    ("case non-ascii", [ "write " ^ e_nfc ^ " x"; "stat " ^ e_upper; "ls ." ]);
    ("case sharp s", [ "write " ^ sharp_s ^ " x"; "stat ss"; "stat SS" ]);
    ("case kelvin", [ "write k x"; "stat " ^ kelvin; "write " ^ kelvin ^ " y" ]);
    ("nfd lookup", [ "write " ^ e_nfc ^ " x"; "stat " ^ e_nfd; "ls ." ]);
    ( "nfd create",
      [
        "write " ^ e_nfd ^ " x";
        "write " ^ e_nfc ^ " y";
        "ls .";
        "read " ^ e_nfd;
      ] );
    ( "nfd rename",
      [ "write " ^ e_nfc ^ " x"; "rename " ^ e_nfc ^ " " ^ e_nfd; "ls ." ] );
    ("nfd case", [ "write " ^ e_upper_nfd ^ " x"; "stat " ^ e_nfc; "ls ." ]);
    ("invalid utf-8", [ "write \xff x"; "mkdir \xfe"; "ls ." ]);
    ("invalid utf-8 lookup", [ "stat \xff"; "unlink \xff"; "rename \xff a" ]);
    (* Names: limits. *)
    ("name max", [ "write {a}255 x"; "stat {a}256"; "write {a}256 x" ]);
    ( "name max multibyte",
      [ "write {" ^ e_nfc ^ "}128 x"; "write {" ^ e_nfc ^ "}255 x"; "ls ." ] );
    ("name max 256 characters", [ "write {" ^ e_nfc ^ "}256 x"; "ls ." ]);
    ( "name max decomposed",
      [ "write {" ^ e_nfd ^ "}128 x"; "write {" ^ e_nfc ^ "}128 y"; "ls ." ] );
    ( "name max astral",
      [ "write {" ^ smiley ^ "}128 x"; "write {" ^ smiley ^ "}255 x"; "ls ." ]
    );
    ("path max", [ "stat {a/}520"; "stat {a/}2100"; "mkdir {a/}520" ]);
    ("nul byte", [ "stat a\000b"; "write a\000b x" ]);
    (* unlink, rmdir, mkdir. *)
    ("unlink dir", [ "mkdir d"; "unlink d"; "unlink d/"; "ls ." ]);
    ("unlink file slash", [ "write f x"; "unlink f/"; "ls ." ]);
    ("unlink dots", [ "mkdir d"; "unlink d/."; "unlink d/.."; "unlink ." ]);
    ("rmdir dot", [ "mkdir d"; "rmdir d/."; "ls ." ]);
    ("rmdir dot-dot", [ "mkdir d"; "mkdir d/e"; "rmdir d/e/.."; "ls ." ]);
    ("rmdir non-empty", [ "mkdir d"; "write d/f x"; "rmdir d" ]);
    ("rmdir file", [ "write f x"; "rmdir f"; "rmdir f/" ]);
    ("rmdir link", [ "mkdir d"; "symlink d l"; "rmdir l"; "ls ." ]);
    ("rmdir link slash", [ "mkdir d"; "symlink d l"; "rmdir l/"; "ls ." ]);
    ("unlink link slash", [ "mkdir d"; "symlink d l"; "unlink l/"; "ls ." ]);
    ("rename link slash", [ "mkdir d"; "symlink d l"; "rename l/ m"; "ls ." ]);
    ( "mkdir exists",
      [ "mkdir d"; "mkdir d"; "mkdir d/"; "write f x"; "mkdir f"; "mkdir f/" ]
    );
    ( "mkdir dangling link",
      [ "symlink nowhere l"; "mkdir l"; "mkdir l/"; "ls ." ] );
    ("mkdir slash", [ "mkdir n/"; "ls ." ]);
    ("mkdir under file", [ "write f x"; "mkdir f/x" ]);
    ("mkdir dots", [ "mkdir ."; "mkdir d"; "mkdir d/."; "mkdir d/.." ]);
    (* open. *)
    ( "open file",
      [
        "write f abc";
        "open f r";
        "open f w";
        "open f c";
        "open f x";
        "open f t";
        "read f";
      ] );
    ("open missing", [ "open m r"; "open m w"; "open m c"; "ls ." ]);
    ( "open dir",
      [ "mkdir d"; "open d r"; "open d w"; "open d c"; "open d x"; "open d t" ]
    );
    ( "open slash",
      [
        "write f x";
        "open f/ r";
        "open f/ c";
        "open n/ c";
        "mkdir d";
        "open d/ r";
        "open d/ c";
        "ls .";
      ] );
    ( "open nofollow",
      [
        "write f x";
        "symlink f l";
        "open l rn";
        "open l wn";
        "open l cn";
        "open l xn";
        "open l c";
      ] );
    ( "open dangling link",
      [ "symlink t l"; "open l c"; "ls ."; "symlink u m"; "open m x" ] );
    ("open dangling slash link", [ "symlink t/ l"; "open l c"; "ls ." ]);
    ( "open dir handle",
      [ "mkdir d"; "opendir d"; "write f x"; "opendir f"; "opendir m" ] );
    ("read dir", [ "mkdir d"; "readd d" ]);
    ( "pwrite append",
      [ "write f abc"; "pwa f XY 0"; "read f"; "pwa f Z 10"; "read f" ] );
    (* Symbolic links. *)
    ("symlink empty", [ "symlink - l"; "readlink l"; "stat l"; "ls ." ]);
    ( "symlink empty follow",
      [ "symlink - l"; "open l r"; "ls l"; "open l c"; "ls ." ] );
    ( "symlink exists",
      [ "write f x"; "symlink a f"; "symlink a f/"; "symlink a n/"; "ls ." ] );
    ("readlink not link", [ "write f x"; "readlink f"; "mkdir d"; "readlink d" ]);
    ("symlink chain 32", [ "chain 32"; "stat l32"; "open l32 r"; "open l32 c" ]);
    ("symlink chain 33", [ "chain 33"; "stat l33"; "open l33 r"; "open l33 c" ]);
    ("symlink chain 40", [ "chain 40"; "stat l40"; "open l40 r" ]);
    ("symlink chain 41", [ "chain 41"; "stat l41" ]);
    ("symlink loop", [ "symlink a b"; "symlink b a"; "stat a"; "open a c" ]);
    ( "trailing slash",
      [
        "write f x";
        "stat f/";
        "lstat f/";
        "symlink f l";
        "stat l/";
        "mkdir d";
        "symlink d m";
        "lstat m/";
        "stat f/.";
        "stat f/..";
      ] );
    (* rename. *)
    ( "rename into itself",
      [ "mkdir d"; "mkdir d/e"; "rename d d/e/x"; "rename d/e d/e"; "ls d" ] );
    ( "rename over non-empty",
      [ "mkdir a"; "mkdir b"; "write b/f x"; "rename a b"; "rename a b/" ] );
    ("rename kinds", [ "mkdir d"; "write f x"; "rename d f"; "rename f d" ]);
    ("rename dot", [ "mkdir d"; "rename d/. e"; "ls ." ]);
    ("rename dot-dot", [ "mkdir d"; "rename d/.. e"; "ls ." ]);
    ( "rename to dot",
      [
        "mkdir d";
        "write f x";
        "rename f d/.";
        "rename f d/..";
        "rename f .";
        "ls .";
      ] );
    ( "rename slash",
      [
        "write f x";
        "rename f/ g";
        "rename f g/";
        "mkdir d";
        "rename d/ e/";
        "ls .";
      ] );
    ("rename missing", [ "rename m n"; "write f x"; "rename f f"; "ls ." ]);
    ("rename above", [ "mkdir a"; "mkdir a/b"; "rename a/b a"; "ls a" ]);
    (* Modes and owners, as a user. *)
    ( "chmod link",
      [
        "write f x";
        "symlink f l";
        "lchmod l 700";
        "mode f";
        "chmod l 600";
        "mode f";
      ] );
    ( "dir no write",
      [ "mkdir d 500"; "write d/f x"; "mkdir d/e"; "ls d"; "chmod d 700" ] );
    ( "dir write and search",
      [
        "mkdir d";
        "chmod d 300";
        "write d/f x";
        "mkdir d/e";
        "stat d/f";
        "chmod d 700";
      ] );
    ( "dir no read",
      [
        "mkdir d";
        "write d/f x";
        "chmod d 300";
        "ls d";
        "stat d/f";
        "chmod d 700";
      ] );
    ( "dir no search",
      [
        "mkdir d";
        "write d/f x";
        "chmod d 600";
        "stat d/f";
        "ls d";
        "chmod d 700";
      ] );
    ( "dir search only",
      [
        "mkdir d";
        "write d/f x";
        "chmod d 100";
        "opendir d";
        "lsd d";
        "stat d/f";
        "chmod d 700";
      ] );
    ( "file no read",
      [ "write f x"; "chmod f 200"; "open f r"; "open f w"; "chmod f 600" ] );
    ( "file no write",
      [
        "write f x";
        "chmod f 400";
        "open f w";
        "open f t";
        "read f";
        "chmod f 600";
      ] );
    ( "group inherited",
      [
        "mkdir d";
        "chgrp d";
        "write d/f x";
        "group d/f";
        "mkdir d/e";
        "group d/e";
        "mode d/e";
      ] );
    ( "setgid dir",
      [
        "mkdir d";
        "chgrp d";
        "chmod d 2775";
        "write d/f x";
        "group d/f";
        "mkdir d/e";
        "group d/e";
        "mode d/e";
      ] );
    ( "write clears setuid",
      [ "write f x"; "chmod f 4755"; "mode f"; "write f y"; "mode f" ] );
    ("chgrp", [ "write f x"; "chgrp f"; "group f"; "chmod f 2755"; "mode f" ]);
    ( "mkdir modes",
      [ "mkdir d 7777"; "mode d"; "write f x"; "chmod f 7777"; "mode f" ] );
  ]

let expand word =
  if word = "-" then ""
  else
    match String.index_opt word '{' with
    | Some 0 -> (
        match String.index_opt word '}' with
        | Some j ->
            let s = String.sub word 1 (j - 1) in
            let n =
              int_of_string
                (String.sub word (j + 1) (String.length word - j - 1))
            in
            String.concat "" (List.init n (fun _ -> s))
        | None -> word)
    | _ -> word

let words op = List.map expand (String.split_on_char ' ' op)
let key ops = String.concat ";" (List.map String.escaped ops)

(* An error, by its errno: by its Eio class, else by the strerror text both
   backends print. *)
let errnos =
  [
    ("not a directory", "ENOTDIR");
    ("is a directory", "EISDIR");
    ("directory not empty", "ENOTEMPTY");
    ("invalid argument", "EINVAL");
    ("busy", "EBUSY");
    ("name too long", "ENAMETOOLONG");
    ("illegal byte sequence", "EILSEQ");
    ("not supported", "ENOTSUP");
  ]

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

let errno exn =
  match exn with
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> "ENOENT"
  | Eio.Io (Eio.Fs.E (Already_exists _), _) -> "EEXIST"
  | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> "denied"
  | Eio.Io (Eio.Fs.E Symlink, _) -> "ELOOP"
  | Eio.Io (Eio.Fs.E File_too_large, _) -> "EFBIG"
  | Eio.Io (Eio.Exn.Not_available _, _) -> "ENOTSUP"
  | Invalid_argument _ -> "invalid_argument"
  | End_of_file -> "eof"
  | exn -> (
      let s = String.lowercase_ascii (Printexc.to_string exn) in
      match List.find_opt (fun (sub, _) -> contains ~sub s) errnos with
      | Some (_, e) -> e
      | None -> "exn " ^ Printexc.to_string exn)

let perm s = int_of_string ("0o" ^ s)

let kind (st : Eio.File.Stat.t) =
  match st.kind with
  | `Regular_file -> Fmt.str "file %Ld" (Optint.Int63.to_int64 st.size)
  | `Directory -> "dir"
  | `Symbolic_link -> "link"
  | _ -> "other"

let names l = "[" ^ String.concat "/" (List.map String.escaped l) ^ "]"

let open_ dir p flags =
  let has c = String.contains flags c in
  let follow = not (has 'n') in
  Eio.Switch.run @@ fun sw ->
  if has 'r' then ignore (Eio.Path.open_in ~sw ~follow Eio.Path.(dir / p))
  else
    let create =
      if has 'x' then `Exclusive 0o644
      else if has 't' then `Or_truncate 0o644
      else if has 'c' then `If_missing 0o644
      else `Never
    in
    ignore
      (Eio.Path.open_out ~sw ~follow ~append:(has 'a') ~create
         Eio.Path.(dir / p))

let chain dir n =
  if not (Eio.Path.is_file Eio.Path.(dir / "f")) then
    Eio.Path.save ~create:(`Exclusive 0o644) Eio.Path.(dir / "f") "x";
  for i = 1 to n do
    let target = if i = 1 then "f" else Fmt.str "l%d" (i - 1) in
    Eio.Path.symlink ~link_to:target Eio.Path.(dir / Fmt.str "l%d" i)
  done

exception Bad_op of string

let op ids dir line =
  let ( / ) = Eio.Path.( / ) in
  let ok () = "ok" in
  match words line with
  | [ "write"; p; data ] ->
      Eio.Path.save ~create:(`Or_truncate 0o644) (dir / p) data;
      ok ()
  | [ "read"; p ] -> String.escaped (Eio.Path.load (dir / p))
  | [ "readd"; p ] ->
      Eio.Switch.run @@ fun sw ->
      let f = Eio.Path.open_in ~sw (dir / p) in
      let buf = Cstruct.create 16 in
      string_of_int (Eio.Flow.single_read f buf)
  | [ "pwa"; p; data; off ] ->
      Eio.Switch.run @@ fun sw ->
      let f = Eio.Path.open_out ~sw ~append:true ~create:`Never (dir / p) in
      Eio.File.pwrite_all f
        ~file_offset:(Optint.Int63.of_string off)
        [ Cstruct.of_string data ];
      ok ()
  | [ "open"; p; flags ] ->
      open_ dir p flags;
      ok ()
  | [ "opendir"; p ] ->
      Eio.Path.with_open_dir (dir / p) ignore;
      ok ()
  | [ "lsd"; p ] ->
      Eio.Path.with_open_dir (dir / p) (fun d -> names (Eio.Path.read_dir d))
  | [ "mkdir"; p ] ->
      Eio.Path.mkdir ~perm:0o755 (dir / p);
      ok ()
  | [ "mkdir"; p; m ] ->
      Eio.Path.mkdir ~perm:(perm m) (dir / p);
      ok ()
  | [ "unlink"; p ] ->
      Eio.Path.unlink (dir / p);
      ok ()
  | [ "rmdir"; p ] ->
      Eio.Path.rmdir (dir / p);
      ok ()
  | [ "rename"; a; b ] ->
      Eio.Path.rename (dir / a) (dir / b);
      ok ()
  | [ "symlink"; target; p ] ->
      Eio.Path.symlink ~link_to:target (dir / p);
      ok ()
  | [ "readlink"; p ] -> String.escaped (Eio.Path.read_link (dir / p))
  | [ "stat"; p ] -> kind (Eio.Path.stat ~follow:true (dir / p))
  | [ "lstat"; p ] -> kind (Eio.Path.stat ~follow:false (dir / p))
  | [ "mode"; p ] -> Fmt.str "%o" (Eio.Path.stat ~follow:false (dir / p)).perm
  | [ "group"; p ] ->
      let g = Int64.to_int (Eio.Path.stat ~follow:false (dir / p)).gid in
      if g = ids.gid then "primary"
      else if g = ids.other_gid then "other"
      else "foreign"
  | [ "chmod"; p; m ] ->
      Eio.Path.chmod ~follow:true ~perm:(perm m) (dir / p);
      ok ()
  | [ "lchmod"; p; m ] ->
      Eio.Path.chmod ~follow:false ~perm:(perm m) (dir / p);
      ok ()
  | [ "chgrp"; p ] ->
      Eio.Path.chown ~follow:false ~gid:(Int64.of_int ids.other_gid) (dir / p);
      ok ()
  | [ "ls"; p ] -> names (Eio.Path.read_dir (dir / p))
  | [ "chain"; n ] ->
      chain dir (int_of_string n);
      ok ()
  | _ -> raise (Bad_op line)

let run ids dir ops =
  List.map
    (fun line ->
      try op ids dir line with Bad_op _ as e -> raise e | exn -> errno exn)
    ops

(* The same operations through [Unix], straight to the kernel. [-] marks one
   [Unix] cannot make. *)

let unix_errno (e : Unix.error) =
  match e with
  | ENOENT -> "ENOENT"
  | EEXIST -> "EEXIST"
  | EACCES | EPERM -> "denied"
  | ELOOP -> "ELOOP"
  | EFBIG -> "EFBIG"
  | EOPNOTSUPP -> "ENOTSUP"
  | e -> (
      let s = String.lowercase_ascii (Unix.error_message e) in
      match List.find_opt (fun (sub, _) -> contains ~sub s) errnos with
      | Some (_, n) -> n
      | None -> "unix " ^ Unix.error_message e)

let unix_kind (st : Unix.LargeFile.stats) =
  match st.st_kind with
  | S_REG -> Fmt.str "file %Ld" st.st_size
  | S_DIR -> "dir"
  | S_LNK -> "link"
  | _ -> "other"

let unix_names p =
  let d = Unix.opendir p in
  let rec go acc =
    match Unix.readdir d with
    | "." | ".." -> go acc
    | n -> go (n :: acc)
    | exception End_of_file -> acc
  in
  let l = Fun.protect ~finally:(fun () -> Unix.closedir d) (fun () -> go []) in
  names (List.sort String.compare l)

let unix_open p flags =
  let has c = String.contains flags c in
  let flags =
    (if has 'r' then [ Unix.O_RDONLY ] else [ Unix.O_RDWR ])
    @ (if has 'x' then [ Unix.O_CREAT; O_EXCL ]
       else if has 't' then [ Unix.O_CREAT; O_TRUNC ]
       else if has 'c' then [ Unix.O_CREAT ]
       else [])
    @ if has 'a' then [ Unix.O_APPEND ] else []
  in
  Unix.close (Unix.openfile p (Unix.O_CLOEXEC :: flags) 0o644)

let unix_op ids dir line =
  let ( / ) d p = if p = "" then d ^ "/" else d ^ "/" ^ p in
  let ok () = "ok" in
  let with_fd p flags f =
    let fd = Unix.openfile p flags 0o644 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> f fd)
  in
  match words line with
  | [ "write"; p; data ] ->
      with_fd (dir / p) [ O_WRONLY; O_CREAT; O_TRUNC ] (fun fd ->
          ignore (Unix.write_substring fd data 0 (String.length data)));
      ok ()
  | [ "read"; p ] ->
      with_fd (dir / p) [ O_RDONLY ] (fun fd ->
          let buf = Buffer.create 64 and b = Bytes.create 4096 in
          let rec go () =
            match Unix.read fd b 0 4096 with
            | 0 -> String.escaped (Buffer.contents buf)
            | n ->
                Buffer.add_subbytes buf b 0 n;
                go ()
          in
          go ())
  | [ "readd"; p ] ->
      with_fd (dir / p) [ O_RDONLY ] (fun fd ->
          string_of_int (Unix.read fd (Bytes.create 16) 0 16))
  (* [Unix] binds no pwrite, lchmod or O_NOFOLLOW. *)
  | [ "pwa"; _; _; _ ] | [ "lchmod"; _; _ ] -> "-"
  | [ "open"; _; flags ] when String.contains flags 'n' -> "-"
  | [ "open"; p; flags ] ->
      unix_open (dir / p) flags;
      ok ()
  | [ "opendir"; p ] ->
      Unix.closedir (Unix.opendir (dir / p));
      ok ()
  | [ "lsd"; p ] | [ "ls"; p ] -> unix_names (dir / p)
  | [ "mkdir"; p ] ->
      Unix.mkdir (dir / p) 0o755;
      ok ()
  | [ "mkdir"; p; m ] ->
      Unix.mkdir (dir / p) (perm m);
      ok ()
  | [ "unlink"; p ] ->
      Unix.unlink (dir / p);
      ok ()
  | [ "rmdir"; p ] ->
      Unix.rmdir (dir / p);
      ok ()
  | [ "rename"; a; b ] ->
      Unix.rename (dir / a) (dir / b);
      ok ()
  | [ "symlink"; target; p ] ->
      Unix.symlink target (dir / p);
      ok ()
  | [ "readlink"; p ] -> String.escaped (Unix.readlink (dir / p))
  | [ "stat"; p ] -> unix_kind (Unix.LargeFile.stat (dir / p))
  | [ "lstat"; p ] -> unix_kind (Unix.LargeFile.lstat (dir / p))
  | [ "mode"; p ] -> Fmt.str "%o" (Unix.LargeFile.lstat (dir / p)).st_perm
  | [ "group"; p ] ->
      let g = (Unix.LargeFile.lstat (dir / p)).st_gid in
      if g = ids.gid then "primary"
      else if g = ids.other_gid then "other"
      else "foreign"
  | [ "chmod"; p; m ] ->
      Unix.chmod (dir / p) (perm m);
      ok ()
  | [ "chgrp"; p ] ->
      Unix.chown (dir / p) (-1) ids.other_gid;
      ok ()
  | [ "chain"; n ] ->
      if not (Sys.file_exists (dir / "f")) then
        with_fd (dir / "f") [ O_WRONLY; O_CREAT; O_EXCL ] (fun fd ->
            ignore (Unix.write_substring fd "x" 0 1));
      for i = 1 to int_of_string n do
        let target = if i = 1 then "f" else Fmt.str "l%d" (i - 1) in
        Unix.symlink target (dir / Fmt.str "l%d" i)
      done;
      ok ()
  | _ -> raise (Bad_op line)

let run_unix ids dir ops =
  List.map
    (fun line ->
      try unix_op ids dir line with
      | Bad_op _ as e -> raise e
      | Unix.Unix_error (e, _, _) -> unix_errno e
      | End_of_file -> "eof")
    ops

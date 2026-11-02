module P = Eio_fs_path

let refusal =
  Alcotest.testable
    (fun ppf -> function
      | P.Not_a_path -> Fmt.string ppf "not a path"
      | P.Relative -> Fmt.string ppf "relative"
      | P.Root -> Fmt.string ppf "the root"
      | P.Not_canonical -> Fmt.string ppf "not canonical")
    ( = )

let answer = Alcotest.(result (list string) refusal)

let accepts path parts =
  Alcotest.check answer
    (Fmt.str "%S is canonical" path)
    (Ok parts) (P.canonical path)

let refuses why path =
  Alcotest.check answer
    (Fmt.str "%S is %a" path (Alcotest.pp refusal) why)
    (Error why) (P.canonical path)

(* A canonical path is the one spelling of itself: what comes back rejoins to
   the string that went in, so the caller's name for the object and the
   filesystem's are the same string. *)
let test_a_canonical_path_is_its_own_spelling () =
  accepts "/a" [ "a" ];
  accepts "/var/lib/space/logs" [ "var"; "lib"; "space"; "logs" ];
  accepts "/a/..b/c." [ "a"; "..b"; "c." ];
  (* A backslash and a colon are ordinary characters in this language, whatever
     a host path module would make of them. *)
  accepts "/a\\b/c:d" [ "a\\b"; "c:d" ]

(* Every other spelling of the same object. Each names what the canonical one
   names and splits differently, which is the whole reason to refuse it. *)
let test_every_other_spelling_is_refused () =
  refuses P.Not_canonical "/a//b";
  refuses P.Not_canonical "//a/b";
  refuses P.Not_canonical "/a/b/";
  refuses P.Not_canonical "/a/b//";
  refuses P.Not_canonical "/a/./b";
  refuses P.Not_canonical "/a/../b";
  refuses P.Not_canonical "/..";
  refuses P.Not_canonical "/."

(* The three answers that are not about spelling at all. *)
let test_a_path_with_no_components_and_a_path_that_is_not_one () =
  refuses P.Relative "a/b";
  refuses P.Relative "./a";
  refuses P.Relative "..";
  refuses P.Root "/";
  refuses P.Root "//";
  refuses P.Not_a_path "";
  (* Every layer below takes a NUL for the end of the string, so the path
     acted on would be [/var/lib/space] and the refusal would name a prefix of
     what was declared. *)
  refuses P.Not_a_path "/var/lib/space\000/../../etc";
  refuses P.Not_a_path "\000"

(* {!P.canonical} answers about the string it was handed; {!P.components}
   answers about the object. Where the first says yes, the two agree, and that
   is what makes a canonical path safe to compare by either one. *)
let test_the_accepted_components_are_the_components () =
  List.iter
    (fun path ->
      match P.canonical path with
      | Error _ -> Alcotest.failf "%S is not canonical" path
      | Ok parts ->
          Alcotest.(check (list string))
            (Fmt.str "%S splits once" path)
            (P.components path) parts)
    [ "/a"; "/var/lib/space/logs"; "/a/..b/c." ]

(* [parent] is lexical: the directory holding a path, without resolving it.
   The empty path is the current directory, as Eio reads it, and [..] is held
   by [../..]. *)
let test_parent () =
  List.iter
    (fun (path, expected) ->
      Alcotest.(check string)
        (Fmt.str "parent %S" path) expected (P.parent path))
    [
      ("/", "/");
      ("/a", "/");
      ("/a/b", "/a/");
      ("/a/b/", "/a/");
      ("a", "./");
      ("a/b", "a/");
      ("", "../");
      (".", "../");
      ("a/.", "./");
      ("..", "../../");
      ("a/..", "a/../../");
      ("/..", "/");
    ]

(* [to_dir_path] adds a separator and changes nothing else. The empty path is
   the current directory, as Eio reads it, not the root. *)
let test_to_dir_path () =
  List.iter
    (fun (path, expected) ->
      Alcotest.(check string)
        (Fmt.str "to_dir_path %S" path)
        expected (P.to_dir_path path))
    [
      ("", "./");
      ("/", "/");
      (".", "./");
      ("a", "a/");
      ("a/", "a/");
      ("/a/b", "/a/b/");
    ]

let suite =
  ( "eio_fs_path",
    [
      Alcotest.test_case "to_dir_path keeps the directory" `Quick
        test_to_dir_path;
      Alcotest.test_case "parent is lexical" `Quick test_parent;
      Alcotest.test_case "a canonical path is its own spelling" `Quick
        test_a_canonical_path_is_its_own_spelling;
      Alcotest.test_case "every other spelling is refused" `Quick
        test_every_other_spelling_is_refused;
      Alcotest.test_case "no components, or not a path" `Quick
        test_a_path_with_no_components_and_a_path_that_is_not_one;
      Alcotest.test_case "the accepted components are the components" `Quick
        test_the_accepted_components_are_the_components;
    ] )

(* Ferrite's litmus tests (Bornholt et al., ASPLOS 2016, section 3, and the
   four in rosette/litmus/ of https://github.com/uwplse/ferrite), run under
   eio-fs's crash model and checked against the ported models. *)

open Litmus

let ext4 = Ferrite.ext4 ()
let envelope = Ferrite.envelope ()

(* {1 The ports reproduce the Rosette tests' outcome counts} *)

let chrome =
  {
    name = "append-chrome";
    setup = [ Creat "0"; Write ("0", String.make 2509 '1'); Fsync "0" ];
    body =
      [ Write ("0", String.make 13 '1'); Write ("0", String.make 2500 '1') ];
    observe = [ "0"; "1" ];
  }

let create_rename ~fsync =
  {
    name = (if fsync then "create-rename (fsync)" else "create-rename");
    setup = [];
    body =
      [ Creat "0"; Write ("0", "11") ]
      @ (if fsync then [ Fsync "0" ] else [])
      @ [ Rename ("0", "1") ];
    observe = [ "0"; "1" ];
  }

let replace ~fsync =
  {
    name = (if fsync then "replace (fsync)" else "replace");
    setup = [ Creat "0"; Write ("0", "1"); Fsync "0" ];
    body =
      [ Creat "1"; Write ("1", "11") ]
      @ (if fsync then [ Fsync "1" ] else [])
      @ [ Rename ("1", "0") ];
    observe = [ "0"; "1" ];
  }

let two_appends ~fsync =
  {
    name = (if fsync then "two-appends (fsync)" else "two-appends");
    setup = [];
    body =
      [ Creat "0"; Creat "1"; Write ("0", "11") ]
      @ (if fsync then [ Fsync "0" ] else [])
      @ [ Write ("1", "1"); Fsync "1" ];
    observe = [ "0"; "1" ];
  }

(* The [(check equal? (length all-states) n)] of each rosette/litmus test. *)
let rosette_counts =
  [
    (chrome, Ferrite.Seqfs, 3);
    (chrome, ext4, 7);
    (chrome, Ferrite.ext4 ~nodelalloc:true (), 4);
    (create_rename ~fsync:false, Seqfs, 4);
    (create_rename ~fsync:true, Seqfs, 4);
    (create_rename ~fsync:false, Advfs, 3);
    (create_rename ~fsync:true, Advfs, 4);
    (create_rename ~fsync:false, ext4, 5);
    (create_rename ~fsync:true, ext4, 4);
    (replace ~fsync:false, Seqfs, 4);
    (replace ~fsync:true, Seqfs, 4);
    (replace ~fsync:false, Advfs, 3);
    (replace ~fsync:true, Advfs, 4);
    (two_appends ~fsync:false, Seqfs, 5);
    (two_appends ~fsync:true, Seqfs, 5);
    (two_appends ~fsync:false, Advfs, 4);
    (two_appends ~fsync:true, Advfs, 5);
  ]

let test_rosette_counts () =
  List.iter
    (fun (t, model, n) ->
      Alcotest.(check int)
        (Fmt.str "%s on %a" t.name Ferrite.pp_model model)
        n
        (Set.cardinal (Ferrite.outcomes model t)))
    rosette_counts

(* {1 The paper's litmus tests} *)

let pa =
  {
    name = "prefix-append";
    setup = [ Creat "file"; Write ("file", String.make 2500 'a') ];
    body = [ Write ("file", String.make 2500 'b') ];
    observe = [ "file" ];
  }

let n_overwrite = 40 * 1024

let same_file =
  {
    name = "ordered-same-file-overwrites";
    setup = [ Creat "f"; Write ("f", String.make n_overwrite '0') ];
    body = [ Pwrite ("f", n_overwrite - 1, "1"); Pwrite ("f", 0, "1") ];
    observe = [ "f" ];
  }

let two_file =
  {
    name = "ordered-two-file-overwrites";
    setup = [ Creat "f"; Creat "g"; Write ("f", "0"); Write ("g", "0") ];
    body = [ Pwrite ("f", 0, "1"); Pwrite ("g", 0, "1") ];
    observe = [ "f"; "g" ];
  }

let implied_dir_fsync =
  {
    name = "implied-directory-fsync";
    setup = [];
    body = [ Creat "file"; Write ("file", "data"); Fsync "file"; Mark ];
    observe = [ "file" ];
  }

let arvr =
  {
    name = "atomic-replace-via-rename";
    setup = [ Creat "file"; Write ("file", "old") ];
    body =
      [
        Creat "file.tmp"; Write ("file.tmp", "new"); Rename ("file.tmp", "file");
      ];
    observe = [ "file"; "file.tmp" ];
  }

let acvr =
  {
    name = "atomic-create-via-rename";
    setup = [];
    body =
      [
        Creat "file.tmp"; Write ("file.tmp", "data"); Rename ("file.tmp", "file");
      ];
    observe = [ "file"; "file.tmp" ];
  }

let content name o = List.assoc name o.entries

(* {1 Cases} *)

type case = {
  test : Litmus.t;
  surprising : outcome -> bool;  (** The test's [exists?] predicate. *)
  paper : (string * bool) list;
      (** Figure 3: [true] where Ferrite found the surprising outcome on the
          real file system. Empty where the paper reports none. *)
  extra : outcome list;
  missing : outcome list;
  impossible : outcome list;
}

let case ?(paper = []) ?(extra = []) ?(missing = []) ?(impossible = []) test
    surprising =
  { test; surprising; paper; extra; missing; impossible }

let zeros n = String.make n '\000'
let ones n = String.make n '1'
let f ?marked l = Outcome.file ?marked (List.map (fun (n, c) -> (n, Some c)) l)
let o ?marked l = Outcome.file ?marked l

let allowed_two_appends o =
  List.mem
    (content "0" o, content "1" o)
    [
      (Absent, Absent);
      (File "", Absent);
      (File "", File "");
      (File "11", File "");
      (File "11", File "1");
    ]

(* Every [extra] below is one shape: a file has the length a write gave it
   but zeros where that write's bytes go. ext4's data=ordered forbids it
   (Definition 7, rule 4: a write and the extend that allocates its block
   stay ordered, and an extend sets data and size together). chaos.mli
   draws a file's length independently of its blocks ("The length is one a
   change left, or a block boundary between them"), which is data=writeback:
   the envelope's [writeback] variant. The one other kind is marked in
   implied-directory-fsync. *)
let cases =
  [
    case pa
      (fun o ->
        match content "file" o with
        | File s ->
            let full = String.make 2500 'a' ^ String.make 2500 'b' in
            let n = String.length s in
            not (n <= 5000 && String.equal s (String.sub full 0 n))
        | _ -> true)
      ~paper:[ ("ext4", true); ("xfs", false); ("btrfs", false) ]
      ~extra:
        (let a = String.make 2500 'a' in
         [
           f [ ("file", a ^ zeros 2500) ];
           f [ ("file", a ^ zeros 1596 ^ String.make 904 'b') ];
           f [ ("file", a ^ String.make 1596 'b' ^ zeros 904) ];
         ]);
    case same_file (fun o ->
        match content "f" o with
        | File s -> s.[0] = '1' && s.[n_overwrite - 1] = '0'
        | _ -> false);
    case two_file (fun o ->
        content "f" o = File "0" && content "g" o = File "1");
    case implied_dir_fsync
      (fun o -> o.marked && content "file" o = Absent)
      ~extra:
        [
          f [ ("file", zeros 4) ];
          (* Not writeback: syncing a file does not commit its creation
             (chaos.mli), where ext4 commits the creation with the inode
             and Ferrite's barrier orders every earlier operation. *)
          o ~marked:true [ ("file", None) ];
        ];
    case arvr
      (fun o ->
        content "file" o <> File "old" && content "file" o <> File "new")
      ~paper:[ ("ext4", true); ("xfs", true); ("btrfs", false) ]
      ~extra:
        [
          o [ ("file", Some (zeros 3)); ("file.tmp", None) ];
          f [ ("file", "old"); ("file.tmp", zeros 3) ];
        ];
    case acvr
      (fun o -> content "file" o <> Absent && content "file" o <> File "data")
      ~paper:[ ("ext4", true); ("xfs", true); ("btrfs", true) ]
      ~extra:
        [
          o [ ("file", None); ("file.tmp", Some (zeros 4)) ];
          o [ ("file", Some (zeros 4)); ("file.tmp", None) ];
        ];
    case
      (create_rename ~fsync:false)
      (fun o -> not (List.mem (content "1" o) [ Absent; File "11" ]))
      ~extra:
        [
          o [ ("0", None); ("1", Some (zeros 2)) ];
          o [ ("0", Some (zeros 2)); ("1", None) ];
        ];
    case
      (create_rename ~fsync:true)
      (fun o -> not (List.mem (content "1" o) [ Absent; File "11" ]))
      ~extra:[ o [ ("0", Some (zeros 2)); ("1", None) ] ];
    case (replace ~fsync:false)
      (fun o -> not (List.mem (content "0" o) [ File "1"; File "11" ]))
      ~extra:
        [
          o [ ("0", Some (zeros 2)); ("1", None) ];
          f [ ("0", "1"); ("1", zeros 2) ];
        ];
    case (replace ~fsync:true)
      (fun o -> not (List.mem (content "0" o) [ File "1"; File "11" ]))
      ~extra:[ f [ ("0", "1"); ("1", zeros 2) ] ];
    case (two_appends ~fsync:false)
      (fun o -> not (allowed_two_appends o))
      ~extra:
        [
          f [ ("0", ""); ("1", zeros 1) ];
          o [ ("0", Some (zeros 2)); ("1", None) ];
          f [ ("0", zeros 2); ("1", "") ];
          f [ ("0", zeros 2); ("1", zeros 1) ];
          f [ ("0", zeros 2); ("1", "1") ];
          f [ ("0", "11"); ("1", zeros 1) ];
        ];
    case (two_appends ~fsync:true)
      (fun o -> not (allowed_two_appends o))
      ~extra:
        [
          o [ ("0", Some (zeros 2)); ("1", None) ];
          f [ ("0", zeros 2); ("1", "") ];
          f [ ("0", "11"); ("1", zeros 1) ];
        ];
    case chrome
      (fun o ->
        match content "0" o with
        | File s -> String.exists (fun c -> c <> '1') s
        | _ -> true)
      ~extra:
        (List.map
           (fun s -> o [ ("0", Some s); ("1", None) ])
           [
             ones 2509 ^ zeros 2513;
             ones 2509 ^ zeros 1587 ^ ones 926;
             ones 2522 ^ zeros 2500;
             ones 2522 ^ zeros 1574 ^ ones 926;
             ones 4096 ^ zeros 926;
           ]);
  ]

let mark b = if b then "yes" else "no"

let run_case c () =
  let t = c.test in
  let r = reorder ~seeds:(seeds ()) t in
  let s = survive t in
  let ext4_set = Ferrite.outcomes ext4 t in
  let env_set = Ferrite.outcomes envelope t in
  let advfs = Ferrite.outcomes Advfs t in
  let seqfs = Ferrite.outcomes Seqfs t in
  let exists set = Set.exists c.surprising set in
  if report then
    Fmt.epr
      "| %s | %d (saturated by seed %d of %d) | survive %d | ext4 %d | \
       envelope %d | advfs %d (eio outside: %d) | seqfs %d | surprising: eio \
       %s, ext4-model %s, paper %a |@."
      t.name (Set.cardinal r.outcomes) r.last_new (seeds ())
      (Set.cardinal s.outcomes) (Set.cardinal ext4_set) (Set.cardinal env_set)
      (Set.cardinal advfs)
      (Set.cardinal (Set.diff r.outcomes advfs))
      (Set.cardinal seqfs)
      (mark (exists r.outcomes))
      (mark (exists ext4_set))
      Fmt.(list ~sep:(any " ") (pair ~sep:(any ":") string bool))
      c.paper;
  check ~name:t.name ~eio:r.outcomes ~envelope:env_set ~ext4:ext4_set
    ~extra:c.extra ~missing:c.missing ~impossible:c.impossible;
  Alcotest.check testable "sequential states are reachable" Set.empty
    (Set.diff seqfs r.outcomes);
  Alcotest.check testable "advfs states are reachable" Set.empty
    (Set.diff advfs r.outcomes);
  (* A real file system showing the surprising outcome means eio-fs must. *)
  if List.exists snd c.paper then
    Alcotest.(check bool)
      "eio-fs shows what Figure 3 saw" true (exists r.outcomes)

let suite =
  ( "ferrite",
    Alcotest.test_case "rosette outcome counts" `Quick test_rosette_counts
    :: List.map
         (fun c -> Alcotest.test_case c.test.name `Quick (run_case c))
         cases )

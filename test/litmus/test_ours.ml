(* Our own litmus tests, for the operations eio-fs models and Ferrite lacks:
   mkdir, unlink, symlink, truncate, a cross-directory rename and fsync after
   a failed fsync. No model enumerates these, so each test states two sets by
   hand:

   - [ext4]: what ext4 in its default data=ordered mode can leave, from its
     documentation (Documentation/filesystems/ext4 and the jbd2 journal):
     every metadata change goes through one journal, which commits whole
     transactions in order, and an fsync commits the transaction holding the
     file's inode, with every earlier change;
   - [envelope]: the same relaxed as Ferrite.envelope relaxes ext4, the
     weakest crash behaviour eio-fs claims: a file's length lands
     independently of its data (data=writeback), and an fsync covers the
     file's data, not its name nor any other entry, so directory changes
     become durable only on a directory barrier.

   Both keep what POSIX implies for a file system that survives a crash at
   all: an entry inside a directory is reachable only through that
   directory, a directory barrier makes that directory's entries durable, a
   truncation is one change of length that does not rewrite the bytes it
   keeps or cuts, and a failed fsync leaves the bytes it failed to write
   unwritten (Linux since 4.13 marks those pages clean, so a later fsync
   does not retry them).

   Each case then lists how eio-fs differs: [impossible], outside the
   envelope (known unsound); [extra], inside it but weaker than ext4;
   [missing], what ext4 leaves and eio-fs never does; and [not_produced],
   what the envelope allows and eio-fs never does (deliberately stronger). *)

open Litmus

let out ?(marked = false) entries = { marked; entries }

(* Each state, before the mark and after it. *)
let both l =
  List.map (fun e -> out e) l @ List.map (fun e -> out ~marked:true e) l

let unmarked l = List.map (fun e -> out e) l
let marked l = List.map (fun e -> out ~marked:true e) l
let zeros n = String.make n '\000'

type case = {
  test : Litmus.t;
  ext4 : outcome list;
  envelope : outcome list;
  extra : outcome list;
  missing : outcome list;
  impossible : outcome list;
  not_produced : outcome list;
}

let run_case c () =
  let r = reorder ~seeds:(seeds ()) c.test in
  let s = survive c.test in
  if report then
    Fmt.epr
      "| %s | %d (saturated by seed %d of %d) | survive %d | ext4 %d | \
       envelope %d |@."
      c.test.name (Set.cardinal r.outcomes) r.last_new (seeds ())
      (Set.cardinal s.outcomes) (List.length c.ext4) (List.length c.envelope);
  let ext4 = Set.of_list c.ext4 and envelope = Set.of_list c.envelope in
  Alcotest.check testable "ext4 stays inside the envelope" Set.empty
    (Set.diff ext4 envelope);
  check ~name:c.test.name ~eio:r.outcomes ~envelope ~ext4 ~extra:c.extra
    ~missing:c.missing ~impossible:c.impossible ~not_produced:c.not_produced

let case ?(extra = []) ?(missing = []) ?(impossible = []) ?(not_produced = [])
    test ~ext4 ~envelope =
  { test; ext4; envelope; extra; missing; impossible; not_produced }

(* {1 mkdir} *)

(* A file made in a new directory and synced, with no directory barrier.
   ext4: the fsync commits the transaction that holds the mkdir and the
   creation. The envelope: either may be lost, in order, and the file only
   with its directory; once synced, a file that survives holds its data. *)
let mkdir_file_sync =
  case
    {
      name = "mkdir, then a synced file in it";
      setup = [];
      body = [ Mkdir "d"; Creat "d/f"; Write ("d/f", "x"); Fsync "d/f"; Mark ];
      observe = [ "d"; "d/f" ];
    }
    ~ext4:
      (unmarked
         [
           [ ("d", Absent); ("d/f", Absent) ];
           [ ("d", Dir); ("d/f", Absent) ];
           [ ("d", Dir); ("d/f", File "") ];
           [ ("d", Dir); ("d/f", File "x") ];
         ]
      @ marked [ [ ("d", Dir); ("d/f", File "x") ] ])
    ~envelope:
      (unmarked
         [
           [ ("d", Absent); ("d/f", Absent) ];
           [ ("d", Dir); ("d/f", Absent) ];
           [ ("d", Dir); ("d/f", File "") ];
           [ ("d", Dir); ("d/f", File "x") ];
           [ ("d", Dir); ("d/f", File (zeros 1)) ];
         ]
      @ marked
          [
            [ ("d", Absent); ("d/f", Absent) ];
            [ ("d", Dir); ("d/f", Absent) ];
            [ ("d", Dir); ("d/f", File "x") ];
          ])
    ~extra:
      (unmarked [ [ ("d", Dir); ("d/f", File (zeros 1)) ] ]
      @ marked
          [
            [ ("d", Absent); ("d/f", Absent) ]; [ ("d", Dir); ("d/f", Absent) ];
          ])

(* The same with a barrier on the new directory, and none on its parent.
   fsync(2) on [d] makes [d]'s entries durable; [d]'s own entry in the root
   may still be lost. So after the barrier [d] either is gone, and [f] with
   it, or holds [f]: no crash leaves [d] without [f]. *)
let mkdir_dir_barrier =
  let pre =
    unmarked
      [
        [ ("d", Absent); ("d/f", Absent) ];
        [ ("d", Dir); ("d/f", Absent) ];
        [ ("d", Dir); ("d/f", File "") ];
        [ ("d", Dir); ("d/f", File "x") ];
      ]
  in
  case
    {
      name = "mkdir, then a directory barrier on it alone";
      setup = [];
      body =
        [
          Mkdir "d";
          Creat "d/f";
          Write ("d/f", "x");
          Fsync "d/f";
          Fsync_dir "d";
          Mark;
        ];
      observe = [ "d"; "d/f" ];
    }
    ~ext4:(pre @ marked [ [ ("d", Dir); ("d/f", File "x") ] ])
    ~envelope:
      (pre
      @ unmarked [ [ ("d", Dir); ("d/f", File (zeros 1)) ] ]
      @ marked
          [
            [ ("d", Absent); ("d/f", Absent) ];
            [ ("d", Dir); ("d/f", File "x") ];
          ])
    ~extra:
      (unmarked [ [ ("d", Dir); ("d/f", File (zeros 1)) ] ]
      @ marked [ [ ("d", Absent); ("d/f", Absent) ] ])

(* {1 unlink} *)

let unlink =
  case
    {
      name = "unlink";
      setup = [ Creat "f"; Write ("f", "A") ];
      body = [ Unlink "f"; Mark ];
      observe = [ "f" ];
    }
    ~ext4:(both [ [ ("f", File "A") ]; [ ("f", Absent) ] ])
    ~envelope:(both [ [ ("f", File "A") ]; [ ("f", Absent) ] ])

(* The name comes back for another inode. A crash that undoes the unlink
   must find the old bytes there, never the new file's: an outcome holding
   [B] without [m] would mean the old name kept the new inode's data. *)
let unlink_recreate =
  let states =
    [
      [ ("f", File "AAAA"); ("m", Absent) ];
      [ ("f", Absent); ("m", Absent) ];
      [ ("f", Absent); ("m", Dir) ];
      [ ("f", File "B"); ("m", Dir) ];
    ]
  in
  case
    {
      name = "unlink, then create the name again";
      setup = [ Creat "f"; Write ("f", "AAAA") ];
      body =
        [ Unlink "f"; Mkdir "m"; Creat "f"; Write ("f", "B"); Fsync "f"; Mark ];
      observe = [ "f"; "m" ];
    }
    ~ext4:
      (unmarked (states @ [ [ ("f", File ""); ("m", Dir) ] ])
      @ marked [ [ ("f", File "B"); ("m", Dir) ] ])
    ~envelope:
      (unmarked
         (states
         @ [
             [ ("f", File ""); ("m", Dir) ];
             [ ("f", File (zeros 1)); ("m", Dir) ];
           ])
      @ marked states)
    ~extra:
      (unmarked [ [ ("f", File (zeros 1)); ("m", Dir) ] ]
      @ marked
          [
            [ ("f", File "AAAA"); ("m", Absent) ];
            [ ("f", Absent); ("m", Absent) ];
            [ ("f", Absent); ("m", Dir) ];
          ])

(* {1 symlink} *)

(* Replacing a link by renaming a new one over it. ext4 keeps a short target
   in the inode itself (a fast symlink), so a link is never seen without its
   target, and the rename is one journalled change. *)
let symlink_swap =
  let states =
    [
      [ ("l", Link "v1"); ("l.tmp", Absent) ];
      [ ("l", Link "v1"); ("l.tmp", Link "v2") ];
      [ ("l", Link "v2"); ("l.tmp", Absent) ];
    ]
  in
  case
    {
      name = "symlink, renamed over another";
      setup = [ Symlink { target = "v1"; name = "l" } ];
      body =
        [
          Symlink { target = "v2"; name = "l.tmp" }; Rename ("l.tmp", "l"); Mark;
        ];
      observe = [ "l"; "l.tmp" ];
    }
    ~ext4:(both states) ~envelope:(both states)

let symlink_barrier =
  case
    {
      name = "symlink, then a barrier on its directory";
      setup = [];
      body = [ Symlink { target = "t"; name = "l" }; Fsync_dir ""; Mark ];
      observe = [ "l" ];
    }
    ~ext4:
      (unmarked [ [ ("l", Absent) ]; [ ("l", Link "t") ] ]
      @ marked [ [ ("l", Link "t") ] ])
    ~envelope:
      (unmarked [ [ ("l", Absent) ]; [ ("l", Link "t") ] ]
      @ marked [ [ ("l", Link "t") ] ])

(* {1 truncate} *)

(* ext4 journals a truncation as one change of [i_size] and frees the blocks
   it cuts when that change commits (its orphan list finishes a truncation
   a crash interrupted), so the file has its old length and every old byte,
   or its new length. *)
let truncate_shrink =
  let a = String.make 8192 'A' in
  case
    {
      name = "truncate down";
      setup = [ Creat "f"; Write ("f", a) ];
      body = [ Truncate ("f", 4096); Mark ];
      observe = [ "f" ];
    }
    ~ext4:(both [ [ ("f", File a) ]; [ ("f", File (String.make 4096 'A')) ] ])
    ~envelope:
      (both [ [ ("f", File a) ]; [ ("f", File (String.make 4096 'A')) ] ])

(* Growing by ftruncate(2) is one change of [i_size]: the file has either
   length, never one in between. *)
let truncate_grow =
  case
    {
      name = "truncate up";
      setup = [ Creat "f"; Write ("f", "AAAA") ];
      body = [ Truncate ("f", 8192); Mark ];
      observe = [ "f" ];
    }
    ~ext4:
      (both [ [ ("f", File "AAAA") ]; [ ("f", File ("AAAA" ^ zeros 8188)) ] ])
    ~envelope:
      (both [ [ ("f", File "AAAA") ]; [ ("f", File ("AAAA" ^ zeros 8188)) ] ])

(* {1 rename across directories} *)

(* ext4 journals a rename as one change of both directories, so a barrier
   on either commits it; chaos.mli claims the same. *)
let rename_across =
  case
    {
      name = "rename across directories, barrier on the source";
      setup = [ Mkdir "a"; Mkdir "b"; Creat "a/f"; Write ("a/f", "x") ];
      body = [ Rename ("a/f", "b/f"); Fsync_dir "a"; Mark ];
      observe = [ "a/f"; "b/f" ];
    }
    ~ext4:
      (unmarked
         [
           [ ("a/f", File "x"); ("b/f", Absent) ];
           [ ("a/f", Absent); ("b/f", File "x") ];
         ]
      @ marked [ [ ("a/f", Absent); ("b/f", File "x") ] ])
    ~envelope:
      (unmarked
         [
           [ ("a/f", File "x"); ("b/f", Absent) ];
           [ ("a/f", Absent); ("b/f", File "x") ];
         ]
      @ marked [ [ ("a/f", Absent); ("b/f", File "x") ] ])

(* {1 Directory barriers and renamed directories}

   ext4 journals every directory as an inode: fsync(2) on a directory makes
   that inode's entries durable, whatever name the directory has then or
   later, and its own name is an entry of its parent's. The envelope keeps
   exactly that much: a barrier on a directory commits the entries it holds,
   once the directory itself is durable, and nothing of its parent's. *)

(* A file made in [d] and synced, moved to the root, then barriers on the
   root and on [d]. The root's barrier commits the rename, [d]'s the
   creation; both landing leaves [b] alone, never [b] and [d/b] at once. *)
let rename_out_of_dir =
  let pre =
    [
      [ ("b", Absent); ("d/b", Absent) ];
      [ ("b", Absent); ("d/b", File "") ];
      [ ("b", Absent); ("d/b", File "x") ];
      [ ("b", File "x"); ("d/b", Absent) ];
    ]
  in
  case
    {
      name = "a synced file moved out of its directory, then both barriers";
      setup = [ Mkdir "d" ];
      body =
        [
          Creat "d/b";
          Write ("d/b", "x");
          Fsync "d/b";
          Rename ("d/b", "b");
          Fsync_dir "";
          Fsync_dir "d";
          Mark;
        ];
      observe = [ "b"; "d/b" ];
    }
    ~ext4:(unmarked pre @ marked [ [ ("b", File "x"); ("d/b", Absent) ] ])
    ~envelope:
      (unmarked (pre @ [ [ ("b", Absent); ("d/b", File (zeros 1)) ] ])
      @ marked [ [ ("b", File "x"); ("d/b", Absent) ] ])
    ~extra:(unmarked [ [ ("b", Absent); ("d/b", File (zeros 1)) ] ])

(* The old [d] moved to [x], a new [d] made in its place, a synced file in
   the new one and a barrier on it. The barrier holds for the new
   directory's inode: the file is in the new [d] whenever that survives, and
   never in the old directory, whatever its name. *)
let recreated_dir =
  let st o f xo =
    [ ("d/old", o); ("d/f", f); ("x/old", xo); ("x/f", Absent) ]
  in
  let o = File "O" in
  case
    {
      name = "a barrier on a directory made again under a moved one's name";
      setup = [ Mkdir "d"; Creat "d/old"; Write ("d/old", "O") ];
      body =
        [
          Rename ("d", "x");
          Mkdir "d";
          Creat "d/f";
          Write ("d/f", "F");
          Fsync "d/f";
          Fsync_dir "d";
          Mark;
        ];
      observe = [ "d/old"; "d/f"; "x/old"; "x/f" ];
    }
    ~ext4:
      (unmarked
         [
           st o Absent Absent;
           st Absent Absent o;
           st Absent (File "") o;
           st Absent (File "F") o;
         ]
      @ marked [ st Absent (File "F") o ])
    ~envelope:
      (unmarked
         [
           st o Absent Absent;
           st Absent Absent o;
           st Absent (File "") o;
           st Absent (File (zeros 1)) o;
           st Absent (File "F") o;
         ]
      @ marked
          [ st o Absent Absent; st Absent Absent o; st Absent (File "F") o ])
    ~extra:
      (unmarked [ st Absent (File (zeros 1)) o ]
      @ marked [ st o Absent Absent; st Absent Absent o ])

(* The states of a synced file [f] made in a new or a durable [d], which a
   rename takes to [e]. *)
let moved_dir_states =
  [
    [ ("d/f", Absent); ("e/f", Absent) ];
    [ ("d/f", File ""); ("e/f", Absent) ];
    [ ("d/f", File "F"); ("e/f", Absent) ];
    [ ("d/f", Absent); ("e/f", File "F") ];
  ]

let moved_dir_envelope =
  moved_dir_states @ [ [ ("d/f", File (zeros 1)); ("e/f", Absent) ] ]

(* A barrier on [d] before its own entry is durable, then [d] renamed and a
   barrier on the root. The barrier follows the directory: once the root's
   commits the mkdir and the rename, [e] holds [f]. *)
let barrier_then_rename_then_parent =
  case
    {
      name = "a deferred barrier, then the directory renamed and its parent's";
      setup = [];
      body =
        [
          Mkdir "d";
          Creat "d/f";
          Write ("d/f", "F");
          Fsync "d/f";
          Fsync_dir "d";
          Rename ("d", "e");
          Fsync_dir "";
          Mark;
        ];
      observe = [ "d/f"; "e/f" ];
    }
    ~ext4:
      (unmarked moved_dir_states
      @ marked [ [ ("d/f", Absent); ("e/f", File "F") ] ])
    ~envelope:
      (unmarked moved_dir_envelope
      @ marked [ [ ("d/f", Absent); ("e/f", File "F") ] ])
    ~extra:(unmarked [ [ ("d/f", File (zeros 1)); ("e/f", Absent) ] ])

(* mkdir d; a synced d/f; a barrier on d; rename d e. ext4: the barrier
   commits everything before it, the rename may be lost. The envelope: [d]'s
   own entry may be lost too, and [f] goes with whichever name survives. *)
let barrier_then_rename =
  case
    {
      name = "a barrier on a new directory, then the directory renamed";
      setup = [];
      body =
        [
          Mkdir "d";
          Creat "d/f";
          Write ("d/f", "F");
          Fsync "d/f";
          Fsync_dir "d";
          Rename ("d", "e");
          Mark;
        ];
      observe = [ "d/f"; "e/f" ];
    }
    ~ext4:
      (unmarked moved_dir_states
      @ marked
          [
            [ ("d/f", File "F"); ("e/f", Absent) ];
            [ ("d/f", Absent); ("e/f", File "F") ];
          ])
    ~envelope:
      (unmarked moved_dir_envelope
      @ marked
          [
            [ ("d/f", Absent); ("e/f", Absent) ];
            [ ("d/f", File "F"); ("e/f", Absent) ];
            [ ("d/f", Absent); ("e/f", File "F") ];
          ])
    ~extra:
      (unmarked [ [ ("d/f", File (zeros 1)); ("e/f", Absent) ] ]
      @ marked [ [ ("d/f", Absent); ("e/f", Absent) ] ])

(* A synced file in a durable [d], [d] renamed, a barrier on [e]: the barrier
   is on [d]'s inode, so [f] is durable under whichever name survives.
   ext4 commits the rename with it. *)
let rename_then_barrier =
  case
    {
      name = "a directory renamed, then a barrier on its new name";
      setup = [ Mkdir "d" ];
      body =
        [
          Creat "d/f";
          Write ("d/f", "F");
          Fsync "d/f";
          Rename ("d", "e");
          Fsync_dir "e";
          Mark;
        ];
      observe = [ "d/f"; "e/f" ];
    }
    ~ext4:
      (unmarked moved_dir_states
      @ marked [ [ ("d/f", Absent); ("e/f", File "F") ] ])
    ~envelope:
      (unmarked moved_dir_envelope
      @ marked
          [
            [ ("d/f", File "F"); ("e/f", Absent) ];
            [ ("d/f", Absent); ("e/f", File "F") ];
          ])
    ~extra:
      (unmarked [ [ ("d/f", File (zeros 1)); ("e/f", Absent) ] ]
      @ marked [ [ ("d/f", File "F"); ("e/f", Absent) ] ])

(* {1 fsync after a failed fsync} *)

(* The failed fsync loses the write; the next one succeeds and writes
   nothing (fsyncgate). *)
let fsync_after_failure =
  let states = unmarked [ [ ("f", File "old") ]; [ ("f", File "new") ] ] in
  case
    {
      name = "fsync after a failed fsync";
      setup = [ Creat "f"; Write ("f", "old") ];
      body = [ Pwrite ("f", 0, "new"); Fsync_fails "f"; Fsync "f"; Mark ];
      observe = [ "f" ];
    }
    ~ext4:(states @ marked [ [ ("f", File "old") ] ])
    ~envelope:(states @ marked [ [ ("f", File "old") ] ])

(* Writing the data again from the application's copy is the only recovery. *)
let fsync_after_rewrite =
  let states = unmarked [ [ ("f", File "old") ]; [ ("f", File "new") ] ] in
  case
    {
      name = "fsync after a failed fsync and a rewrite";
      setup = [ Creat "f"; Write ("f", "old") ];
      body =
        [
          Pwrite ("f", 0, "new");
          Fsync_fails "f";
          Pwrite ("f", 0, "new");
          Fsync "f";
          Mark;
        ];
      observe = [ "f" ];
    }
    ~ext4:(states @ marked [ [ ("f", File "new") ] ])
    ~envelope:(states @ marked [ [ ("f", File "new") ] ])

(* {1 A rename into a new directory, a rename after a create, a regrown
   truncation} *)

(* A file moved into a new directory, then a barrier on that directory
   alone. The rename changes both directories in one step, so once the
   barrier returns it is durable: [a/f] is gone. [b/d] may still be lost
   with it (the envelope) or not (ext4). *)
let rename_into_new_dir =
  let pre =
    [
      [ ("a/f", File "x"); ("b/d", Absent); ("b/d/f", Absent) ];
      [ ("a/f", File "x"); ("b/d", Dir); ("b/d/f", Absent) ];
      [ ("a/f", Absent); ("b/d", Dir); ("b/d/f", File "x") ];
    ]
  in
  let lost = [ ("a/f", Absent); ("b/d", Absent); ("b/d/f", Absent) ] in
  case
    {
      name = "rename into a new directory, barrier on it alone";
      setup = [ Mkdir "a"; Mkdir "b"; Creat "a/f"; Write ("a/f", "x") ];
      body = [ Mkdir "b/d"; Rename ("a/f", "b/d/f"); Fsync_dir "b/d"; Mark ];
      observe = [ "a/f"; "b/d"; "b/d/f" ];
    }
    ~ext4:(unmarked pre @ marked [ List.nth pre 2 ])
    ~envelope:(unmarked (pre @ [ lost ]) @ marked [ List.nth pre 2; lost ])
    ~not_produced:(both [ lost ])

(* A file made in [a], then another moved from [a] to [b], then a barrier
   on [b]. Both change [a], so they stay in order (Ferrite's same-directory
   rule): the barrier that makes the rename durable makes [a/g] durable. *)
let rename_after_create_in_source =
  let pre =
    [
      [ ("a/f", File "x"); ("a/g", Absent); ("b/f", Absent) ];
      [ ("a/f", File "x"); ("a/g", File ""); ("b/f", Absent) ];
      [ ("a/f", Absent); ("a/g", File ""); ("b/f", File "x") ];
    ]
  in
  case
    {
      name = "rename after a create in its source, barrier on the target";
      setup = [ Mkdir "a"; Mkdir "b"; Creat "a/f"; Write ("a/f", "x") ];
      body = [ Creat "a/g"; Rename ("a/f", "b/f"); Fsync_dir "b"; Mark ];
      observe = [ "a/f"; "a/g"; "b/f" ];
    }
    ~ext4:(unmarked pre @ marked [ List.nth pre 2 ])
    ~envelope:(unmarked pre @ marked [ List.nth pre 2 ])

(* A truncation, then an overwrite that grows the file again inside the cut
   block. data=ordered writes the zeroed tail before the truncation commits;
   data=writeback may commit the new length over the old block, so the
   file's own cut byte "B" comes back. *)
let truncate_then_regrow =
  let ordered =
    [ "ABC"; "A"; "A\000"; "AZ"; "A\000\000"; "AZ\000" ]
    |> List.map (fun c -> [ ("f", File c) ])
  in
  case
    {
      name = "truncate, then regrow inside the cut block";
      setup = [ Creat "f"; Write ("f", "ABC") ];
      body = [ Truncate ("f", 1); Pwrite ("f", 1, "Z"); Mark ];
      observe = [ "f" ];
    }
    ~ext4:(both ordered)
    ~envelope:(both (ordered @ [ [ ("f", File "AB") ] ]))
    ~not_produced:(both [ [ ("f", File "AB") ] ])

let cases =
  [
    mkdir_file_sync;
    mkdir_dir_barrier;
    unlink;
    unlink_recreate;
    symlink_swap;
    symlink_barrier;
    truncate_shrink;
    truncate_grow;
    rename_across;
    rename_out_of_dir;
    recreated_dir;
    barrier_then_rename_then_parent;
    barrier_then_rename;
    rename_then_barrier;
    fsync_after_failure;
    fsync_after_rewrite;
    rename_into_new_dir;
    rename_after_create_in_source;
    truncate_then_regrow;
  ]

let suite =
  ( "ours",
    List.map (fun c -> Alcotest.test_case c.test.name `Quick (run_case c)) cases
  )

(* Tests for the critical section a caller supplies: that [none] gets out of
   the way, that a packed lock keeps two fibers out of one section, that a
   raise in the body leaves the section usable, and that a runner which cannot
   enter says so in the section's own vocabulary. *)

module Lock = Eio_fs_sync.Lock

(* [none] adds no exclusion, so what it owes the caller is not to get in the
   way: run the section and answer what the section answered. *)
let test_none_runs_the_section () =
  Alcotest.(check int)
    "the section's answer" 42
    (Lock.use Lock.none (fun () -> 42));
  (* One lock value serves sections at every result type: a section is
     exclusion, not a channel for one kind of answer. A [t] that let the first
     use fix the type would not typecheck past this line. *)
  Alcotest.(check string)
    "and at another result type" "answer"
    (Lock.use Lock.none (fun () -> "answer"))

module Fiber_lock = struct
  type t = Eio.Mutex.t

  let use mutex f = Eio.Mutex.use_ro mutex f
end

module Section = Lock.Make (Fiber_lock)

(* What a packed lock is for: two fibers are never inside one section at once,
   however long the body suspends. The count is raised on the way in and
   dropped on the way out with a yield between them, so a section that let the
   second fiber in while the first was suspended is seen rather than assumed
   away. *)
let test_make_excludes_fibers () =
  Eio_mock.Backend.run @@ fun () ->
  let section = Section.v (Eio.Mutex.create ()) in
  let inside = ref 0 and most = ref 0 in
  let enter () =
    Lock.use section (fun () ->
        incr inside;
        most := max !most !inside;
        Eio.Fiber.yield ();
        decr inside)
  in
  Eio.Fiber.both enter enter;
  Alcotest.(check int) "never two fibers in one section" 1 !most;
  Alcotest.(check int) "and both left it" 0 !inside

(* A section is exclusion and nothing more: it does not swallow what the body
   raised, and it is entered again afterwards. A lock that a raise disabled
   would end the component that runs on it. *)
let test_a_raise_leaves_the_section_usable () =
  Eio_mock.Backend.run @@ fun () ->
  let section = Section.v (Eio.Mutex.create ()) in
  (match Lock.use section (fun () -> failwith "the body's own") with
  | () -> Alcotest.fail "the section swallowed a raise"
  | exception Failure m ->
      Alcotest.(check string) "raised as thrown" "the body's own" m);
  Alcotest.(check int)
    "the section is still there" 7
    (Lock.use section (fun () -> 7))

(* A runner that cannot enter has no answer to give at the section's result
   type, so it raises -- and it raises [Unavailable], the one exception a
   backend-free component on the other side can name. A component whose
   interface promises a result catches this and reports it; anything else
   would travel out through that promise. *)
module Refusing = struct
  type t = string

  let use held _f = raise (Lock.Unavailable held)
end

module Refused = Lock.Make (Refusing)

let test_a_refusal_is_unavailable () =
  let section = Refused.v "/state/floor.lock is held by another process" in
  let ran = ref false in
  match Lock.use section (fun () -> ran := true) with
  | () -> Alcotest.fail "a refused section ran its body"
  | exception Lock.Unavailable what ->
      Alcotest.(check string)
        "the refusal says what refused it"
        "/state/floor.lock is held by another process" what;
      Alcotest.(check bool) "and the body never ran" false !ran

(* [none] is the section that never refuses: a component holding one can still
   fail, but never for want of the section. *)
let test_none_never_refuses () =
  Alcotest.(check int)
    "entered" 1
    (match Lock.use Lock.none (fun () -> 1) with
    | n -> n
    | exception Lock.Unavailable _ -> 0)

let suite =
  ( "lock",
    [
      Alcotest.test_case "none runs the section" `Quick
        test_none_runs_the_section;
      Alcotest.test_case "a packed lock excludes fibers" `Quick
        test_make_excludes_fibers;
      Alcotest.test_case "a raise leaves the section usable" `Quick
        test_a_raise_leaves_the_section_usable;
      Alcotest.test_case "a refusal is Unavailable" `Quick
        test_a_refusal_is_unavailable;
      Alcotest.test_case "none never refuses" `Quick test_none_never_refuses;
    ] )

(* Two programs whose bug needs two faults: the first sends the program
   down another path, and the second lands on it. *)

module P = Eio.Path

let pwrite f off s =
  Eio.File.pwrite_all f ~file_offset:(Optint.Int63.of_int off)
    [ Cstruct.of_string s ]

let pread f off len =
  let b = Cstruct.create len in
  Eio.File.pread_exact f ~file_offset:(Optint.Int63.of_int off) [ b ];
  Cstruct.to_string b

let hash s = String.sub (Digest.to_hex (Digest.string s)) 0 8

(* An A/B updater: slot A and slot B hold images, flag names the slot that
   boots. An update writes the new image into the other slot, then flips
   the flag. With [`Default_slot], a flag it fails to read names slot A, so
   it may overwrite the slot that boots. *)
module Ab = struct
  let image v =
    let body = String.init 3000 (fun i -> Char.chr (97 + (i * v mod 26))) in
    Fmt.str "%d %s\n%s" v (hash body) body

  let verify s =
    match String.index_opt s '\n' with
    | None -> None
    | Some i ->
        let body = String.sub s (i + 1) (String.length s - i - 1) in
        Scanf.sscanf_opt (String.sub s 0 i) "%d %s" (fun v h ->
            if h = hash body then Some v else None)
        |> Option.join

  let slot c = Fmt.str "slot%c" c

  let flag fs =
    Scanf.sscanf (P.load P.(fs / "flag")) "%c %d" (fun c v -> (c, v))

  let boot fs =
    let c, v = flag fs in
    if verify (P.load P.(fs / slot c)) <> Some v then
      Fmt.failwith "flag names slot %c v%d, which does not verify" c v

  let install ~bug fs v =
    let active =
      match flag fs with
      | c, _ -> c
      | exception Eio.Io _ when bug = `Default_slot -> 'A'
    in
    let target = if active = 'A' then 'B' else 'A' in
    let img = image v in
    P.with_open_out ~create:(`Or_truncate 0o644)
      P.(fs / slot target)
      (fun f ->
        let n = String.length img in
        for c = 0 to (n - 1) / 200 do
          let off = c * 200 in
          pwrite f off (String.sub img off (min 200 (n - off)));
          Eio.Fiber.yield ()
        done;
        Eio.File.sync f);
    if verify (P.load P.(fs / slot target)) = Some v then begin
      P.save ~create:(`Or_truncate 0o644)
        P.(fs / "flag.tmp")
        (Fmt.str "%c %d" target v);
      P.rename P.(fs / "flag.tmp") P.(fs / "flag")
    end

  (* Updates every 30 s, and a monitor that boots every 10 s. *)
  let test ?budget ?(bug = `None) () =
    Eio_fs.Chaos.check ?budget
      ~setup:(fun env ->
        P.save ~create:(`Exclusive 0o644) P.(env#fs / "slotA") (image 1);
        P.save ~create:(`Exclusive 0o644) P.(env#fs / "flag") "A 1")
      (fun env ->
        let stop = ref false in
        Eio.Fiber.both
          (fun () ->
            for v = 2 to 9 do
              (try install ~bug env#fs v with Eio.Io _ -> ());
              Eio.Time.sleep env#clock 30.
            done;
            stop := true)
          (fun () ->
            while not !stop do
              Eio.Time.sleep env#clock 10.;
              try boot env#fs with Eio.Io _ -> ()
            done))
end

(* A double-buffered state file: two slots, each a generation in a header,
   a body and a trailer, the header odd while a write is under way. A
   writer fills the slot the committed generation is not in. With
   [`Advance_on_failure], a failed write still moves it on to the other
   slot, which holds the committed generation. *)
module Seqlock = struct
  let size = 256
  let body k = String.make (size - 32) (Char.chr (97 + (k mod 26)))
  let field g = Fmt.str "%015d\n" g

  let write f ~slot k =
    let base = slot * size in
    pwrite f base (field ((2 * k) - 1));
    pwrite f (base + 16) (String.sub (body k) 0 112);
    pwrite f (base + 128) (String.sub (body k) 112 112);
    pwrite f (base + 240) (field (2 * k));
    pwrite f base (field (2 * k));
    Eio.File.sync f

  let read f slot =
    let base = slot * size in
    let g = int_of_string_opt (String.trim (pread f base 16)) in
    let b = pread f (base + 16) (size - 32) in
    let g' = int_of_string_opt (String.trim (pread f (base + 240) 16)) in
    match (g, g') with
    | Some g, Some g' when g = g' && g mod 2 = 0 && b = body (g / 2) ->
        Some (g / 2)
    | _ -> None

  let latest f = max (read f 0) (read f 1)

  (* A writer every second, and three readers. *)
  let test ?(bug = `None) () =
    Eio_fs.Chaos.check
      ~setup:(fun env ->
        P.with_open_out ~create:(`Exclusive 0o644) P.(env#fs / "state")
        @@ fun f ->
        pwrite f 0 (field 0 ^ String.make (size - 16) ' ');
        write f ~slot:1 1)
      (fun env ->
        try
          P.with_open_out ~create:`Never P.(env#fs / "state") @@ fun f ->
          let committed = ref 1 and attempt = ref 1 and stop = ref false in
          let writer () =
            for _ = 1 to 30 do
              Eio.Time.sleep env#clock 1.;
              let k = !committed + 1 in
              if bug = `Advance_on_failure then incr attempt else attempt := k;
              try
                write f ~slot:(!attempt mod 2) k;
                committed := k
              with Eio.Io _ -> ()
            done;
            stop := true
          in
          let reader delay () =
            while not !stop do
              Eio.Time.sleep env#clock delay;
              let floor = !committed in
              match latest f with
              | Some g when g >= floor -> ()
              | Some g -> Fmt.failwith "reads %d, %d was committed" g floor
              | None -> Fmt.failwith "no valid slot, %d was committed" floor
              | exception Eio.Io _ -> ()
            done
          in
          Eio.Fiber.all [ writer; reader 0.3; reader 0.7; reader 1.1 ]
        with Eio.Io _ -> ())
end

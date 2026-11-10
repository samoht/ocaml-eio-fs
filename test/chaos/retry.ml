(* A save that retries a failed write once, in place, with positioned writes.
   With [`Unchecked_short] the retry ignores a short count. *)

module P = Eio.Path

let save ?(bug = `None) dir v =
  P.with_open_out ~create:(`Or_truncate 0o644)
    P.(dir / "data")
    (fun f ->
      try Eio.Flow.copy_string v f
      with Eio.Io _ ->
        let len = String.length v in
        let rec go off =
          if off < len then
            let n =
              Eio.File.pwrite_single f ~file_offset:(Optint.Int63.of_int off)
                [ Cstruct.of_string ~off ~len:(len - off) v ]
            in
            if bug <> `Unchecked_short then go (off + n)
        in
        go 0)

let load dir = P.load P.(dir / "data")

open! Hermetic

type (_, _, _) Eio.Resource.pi +=
  | Base_dir :
      ('t, 't -> Eio.Fs.dir_ty Eio.Resource.t, [> `Base_dir ]) Eio.Resource.pi
  | Base_file :
      ('t, 't -> Eio.File.ro_ty Eio.Resource.t, [> `Base_file ]) Eio.Resource.pi

let base_dir (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Base_dir)

let base_file (Eio.Resource.T (t, ops)) =
  Option.map (fun get -> get t) (Eio.Resource.get_opt ops Base_file)

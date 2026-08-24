(* The critical section a component runs its file operations inside. *)

(* One lock value serves sections at every result type -- a save ends in
   [unit], a listing in the ids it collected -- so the representation quantifies
   the section's result inside the field rather than at the type. The explicit
   [ 'a. ] is what stops the first use fixing the type for the rest. *)
type t = { run : 'a. (unit -> 'a) -> 'a }

(* The section's own vocabulary for a refusal. [use] answers at the section's
   result type, so a runner that cannot enter has nothing to answer with and
   must raise -- and the component on the other side is backend-free, so it
   cannot name the exceptions of whatever lock the deployment picked. One
   exception declared here is what both sides can name. *)
exception Unavailable of string

let use t f = t.run f
let none = { run = (fun f -> f ()) }

module type S = sig
  type t

  val use : t -> (unit -> 'a) -> 'a
end

(* [L.use] is polymorphic in the section's result by its signature, so the
   eta-expanded [fun f -> L.use l f] is what checks against the quantified
   field: the packed lock inherits the caller's polymorphism instead of being
   asked to promise it. *)
module Make (L : S) = struct
  let v l = { run = (fun f -> L.use l f) }
end

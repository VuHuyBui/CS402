(* James Bui *)

type randstate = int ref * int

let init (m: int) (seed: int) : randstate = 
  (ref seed, m)

let next (r: randstate) : int =
  let (a_ref, m) = r in
  let () = a_ref := (!a_ref) * (!a_ref) mod m in
  !a_ref

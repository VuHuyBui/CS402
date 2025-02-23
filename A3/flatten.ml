(* James Bui *)

(* 
* flatten : int list list -> int list
* REQUIRES: true
* ENSURES: flatten lsts |-*-> flat such that such that flat is flattened with respect to lsts
*)
let rec flatten lsts = 
  match lsts with
  | [] -> []
  | []::lst' ->  flatten lst'
  | (y::ys)::lst' -> y::(flatten (ys::lst'))
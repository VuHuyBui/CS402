(* pi : float *)
let pi : float = 3.14159265358979
(*
* area : float -> float
* REQUIRES: r >= 0
* ENSURES: evaluates to the area of a circle with radius r
*)
let area (r : float) : float = pi *. r *. r
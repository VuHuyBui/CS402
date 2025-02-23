(* pi : float *)
let pi : float = 3.14159265358979
(*
* vol : float -> float
* REQUIRES: r >= 0
* ENSURES: evaluates to the volume of a sphere with radius r
*)
let vol (r : float) : float = 4.0 /. 3.0 *. pi *. r *. r *. r
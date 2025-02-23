(* James Bui *)
(*
* add : int * int -> int
* REQUIRES: m >= 0, n >= 0
* ENSURES: add (m , n ) -*-> m + n 
*)
let rec add (( m :int) , ( n :int) ) : int =
  match m with
  | 0 -> n
  | _ -> 1 + add ( m - 1 , n )

(* 
* mult : int * int -> int
* REQUIRES: m >= 0, n >= 0
* ENSURES: mult (m, n) |--*--> m * n  
*)
let rec mult (m, n) = match n with
| 0 -> 0
| _ -> add(m, mult(m, n - 1))
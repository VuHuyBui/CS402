(* e : float *)
let e : float = 2.71828182845905
(*
* isOne : int -> bool
* REQUIRES: true
* ENSURES: evaluates to true if the input is 1 and false otherwise
*)
let isOne (x : int) =
match x with
 | 1 -> true
 |_ -> false
(*
* isPositive : int -> bool
* REQUIRES: true
* ENSURES: evaluates to true if the input is positive and false otherwise
*)
let isPositive (n : int) =
if n > 0
then true
else false

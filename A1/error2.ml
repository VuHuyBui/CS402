(*
* fact : int -> int
* REQUIRES: n >= 0
* ENSURES: fact n evaluates to n factorial
*)
let rec fact (n : int) : int =
  match n with
  | 0 -> 1
  | _ -> n * fact (n - 1)
  let result = fact 5
  
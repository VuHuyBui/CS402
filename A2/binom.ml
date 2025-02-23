(* James Bui *)

(*
* binom : int * int -> int
* REQUIRES: n >= 0 and k >= 0
* ENSURES: binom (n , k ) is existentially equivalent to nCk
*)

let rec binom (n, k) = match (n, k) with 
| (0, 0) -> 1
| (a, 0) -> 1
| _ -> 
  begin 
    if k > n
      then 0
    else binom(n - 1, k - 1) + binom(n - 1, k) 
  end
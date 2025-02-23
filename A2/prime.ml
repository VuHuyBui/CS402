(* James Bui *)

(*
* isPrimeHelper : int * int -> bool
* REQUIRES: n > 1
* ENSURES: isPrimeHelper (n, current_div) -*-> true if n is prime and false otherwise
*)
let rec isPrimeHelper (n, div) = match div with
| 1 -> true
| _ -> 
  begin
    if n mod div = 0
    then false
  else isPrimeHelper(n, div - 1)
  end

(*
* isPrime : int -> bool
* REQUIRES: n > 1
* ENSURES: isPrime n -*-> true if n is prime and false otherwise
*)
let isPrime x = 
  isPrimeHelper(x, x - 1)
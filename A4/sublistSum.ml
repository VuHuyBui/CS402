(* James Bui *)

(*  
* sum : int list -> int 
* REQUIRES: true
* ENSURES: sum lst |-*-> n where n is the sum of the elements in lst
*)
let rec sum (lst: int list): int =
  match lst with
  | []-> 0
  | x::xs -> x + sum xs

(*  
* sublistSum : int list * int-> int list option
* REQUIRES: true
* ENSURES: sublistSum (lst, n) |-*-> Some lst' where lst' is a sublist of lst and sum
*     lst' ~= n. sublistSum(lst, n) |-*-> None if there is no such sublist lst'. 
*)
let sublistSum (lst, n): int list option = 
  match (lst, n) with
  | ([], 0) -> Some []
  | ([], a) -> None
  | (x::lst', b) -> Some []
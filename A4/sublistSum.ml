(* James Bui *)
(* 
* prepend : int * int list list -> int list list
* REQUIRES: true
* ENSURES: prepend (x, lsts) |-*-> res such that 
*          res contains all lists in lsts, and for each list l in lsts, 
*          (x :: l) is also included in res.
*)
let rec prepend (x, lst) = 
  match lst with
  | [] -> []
  | y::ys -> (x::y)::prepend (x, ys)

(* 
* allSublists : int list -> int list list
* REQUIRES: true
* ENSURES: allSublists lst |-*-> sls such that sls contains all sublists of sls
*)
let rec allSublists (lst : int list ) : int list list = 
  match lst with
  | [] -> [[]]
  | x::lst' -> 
      let sublist = allSublists lst' in
      prepend (x, sublist) @ sublist
(*  
* sum : int list -> int 
* REQUIRES: true
* ENSURES: sum lst |-*-> n where n is the sum of the elements in lst
*)
let rec sum (lst: int list): int =
  match lst with
  | []-> 0
  | x::xs -> x + sum xs


let rec findSublists (sublists, n): int list option = 
  match (sublists, n) with
  | ([], 0) -> Some []
  | ([], a) -> None
  | (x::xs, b) -> if sum x = b
    then Some x
else findSublists (xs, n)

(*  
* sublistSum : int list * int-> int list option
* REQUIRES: true
* ENSURES: sublistSum (lst, n) |-*-> Some lst' where lst' is a sublist of lst and sum
*     lst' ~= n. sublistSum(lst, n) |-*-> None if there is no such sublist lst'. 
*)
let sublistSum (lst, n): int list option = 
  let sublists = allSublists lst in findSublists (sublists, n)

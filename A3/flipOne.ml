(* James Bui *)


let rec flipOneHelp (lst, curr, acc) = 
  match curr with
  | [] -> acc
  | x::curr' -> 
    if x
      then let flipped = lst @ [not x] @ curr' in
          flipOneHelp (lst @ [x], curr', flipped::acc)
        else 
          flipOneHelp (lst @ [x], curr', acc)

(* 
* flipOne : bool list -> bool list list
* REQUIRES: true
* ENSURES: flipOne lst |-*-> a list with all ways to flip exactly one true in lst to false
*)
let rec flipOne lst = 
  flipOneHelp ([], lst, [])

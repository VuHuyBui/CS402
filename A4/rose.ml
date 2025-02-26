(* James Bui *)

type rose = Rose of int * rose list

(* type rose = Rose of int * roseList
and roseList = Nil | Cons of rose * roseList *)

(*  
* sumUp : rose-> int
* REQUIRES: true
* ENSURES: sumUp t |-*-> n where n is the total of all numbers held in t 
*)
let rec sumUp (t: rose): int = 
  match t with 
  | Rose (b, ys) -> b + sumUpList ys

and sumUpList (rl: rose list) :int =
  match rl with 
  | [] -> 0
  | x::lst -> sumUp x + sumUpList lst 

 
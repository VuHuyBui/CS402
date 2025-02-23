type myList = Nil | Cons of int * myList
(* [1;2;3;4] *)
let lst : myList = Cons (1, Cons (2, Cons (3, Cons (4, Nil))))
(*
* ofList : int list -> myList
* REQUIRES: true
* ENSURES: ofList xs |-*-> xs'
* such that xs' represents the same
* list as xs, but as a myList
*)
let rec ofList (xs:int list) : myList =
  match xs with
  | [] -> Nil
  | x::xs' -> Cons (x, ofList xs') 

(*
* toList : myList -> int list
* REQUIRES: true
* ENSURES: toList xs |-*-> xs'
* such that xs' represents the same
* list as xs, but as an int list
*)
let rec toList (xs:myList) : int list =
  match xs with
  | Nil -> []
  | Cons (x, xs') -> x::toList xs'


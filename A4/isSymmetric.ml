(* James Bui *)

type tree = Empty | Node of tree * int * tree

let rec isSame (t1, t2) : bool = 
  match (t1, t2) with
  | (Empty, Empty) -> true
  | (Node (tl1, a, tr1), Node (tl2, b, tr2)) -> isSame (tl1, tl2) && isSame(tr1, tr2)
  | _ -> false
  


(* 
* isSymmetric : tree-> bool
* : true
* ENSURES: isSymmetric t |-*-> true iff invert t can be obtained from t by a series
* of changes to node values, and false otherwise
*)
let isSymmetric (t: tree): bool = 
  match t with
  | Empty -> true
  | Node (tl, x, tr) -> isSame(tl, tr)
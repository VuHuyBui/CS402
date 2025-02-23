type myTree = Leaf | Node of myTree * int * myTree
(*
* 1
* / \
* 3 2
* / \ \
* 0 5 7
* / \
* 8 9
*)
let t : myTree =
Node (
Node (
Node (Leaf, 0, Leaf),
3,
Node (Leaf, 5, Leaf)
),
1,
Node (
Leaf,
2,
Node (
Node (Leaf, 8, Leaf),
7,
Node (Leaf, 9, Leaf)
)
)
)
(*
* sumUp : myTree -> int
* REQUIRES: true
* ENSURES: sumUp t |-*-> the sum of
* all nodes in t
*)
let rec sumUp (t:myTree) : int =
match t with
| Leaf ->  0 
| Node (child1, x, child2) -> x + sumUp child1 + sumUp child2  
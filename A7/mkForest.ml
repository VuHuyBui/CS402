module type TREE = 
sig
  type order = Less | Equal | Greater
  type 'a tree

  exception NotFound

  val empty: unit -> 'a tree
  val insert: ('a * 'a -> order) -> 'a tree -> 'a -> 'a tree
  val remove: ('a * 'a -> order) -> 'a tree -> 'a -> 'a tree
  val isIn: ('a * 'a -> order) -> 'a tree -> 'a -> bool
  val findRoot: 'a tree -> 'a
end

module type FOREST =
  sig
    module T: TREE
    type 'a forest

  
    exception Full
    exception NotInForest
  
    val limit: int
    val empty: unit -> 'a forest
    val addToForest: 'a forest -> 'a T.tree -> 'a forest
    val addToTree: ('a * 'a -> T.order) -> 'a forest -> 'a -> 'a -> 'a forest
    val findRoot: ('a * 'a -> T.order) -> 'a forest -> 'a -> 'a
    val removeTree: ('a * 'a -> T.order) -> 'a forest -> 'a -> 'a forest
  end

module MkForest (T : TREE) (L : sig val lim : int end) : FOREST =
struct

  type 'a forest = 'a T.tree list * int 

  module T = T

  exception Full
  exception NotInForest
  exception Empty

  let limit = L.lim

  let empty () = ([], 0)

  let addToForest (ts, i) tree =
    if i >= L.lim then raise Full else (tree::ts, i + 1)

  let rec accForest g f lst key x = 
    match lst with 
    | [] -> raise Empty
    | t::ts ->
      if g t key 
      then (f t x)::ts 
      else t::(accForest g f ts key x)

  let rec iterateForest g f lst x = 
    match lst with 
    | [] -> raise Empty
    | t::ts ->
      if g t x 
      then f t x 
      else iterateForest g f ts x

  let rec addToTree cmp (ts, i) key x = 
    match ts with 
    | [] -> raise NotInForest
    | _ ->
        try 
          let nts = accForest (T.isIn cmp) (T.insert cmp) ts key x in 
          (nts, i)
        with Empty -> raise NotInForest

  let getRoot t x = T.findRoot t

  let findRoot cmp (ts, i) x = 
    match ts with 
    | [] -> raise NotInForest
    | _ ->
      try
        (iterateForest (T.isIn cmp) getRoot ts x)
      with 
      | Empty -> raise NotInForest
      | T.NotFound -> raise NotInForest

  let removeTree cmp (ts, i) x = 
    match ts with 
    | [] -> raise NotInForest
    | _ ->
      try
        let nts = accForest (T.isIn cmp) (T.remove cmp) ts x x in 
        (nts, i)
      with Empty -> raise NotInForest

end

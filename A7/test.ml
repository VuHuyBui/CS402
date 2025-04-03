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
module BinaryTree : TREE =
struct

  type order = Less | Equal | Greater

  type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  exception NotFound

  let empty () = Empty

  let rec insert (cmp : 'a * 'a -> order) t x = 
    match t with 
    | Empty -> Node (Empty, x, Empty)
    | Node (l, v, r) ->
      match cmp (x, v) with
      | Less -> Node (insert cmp l x, v, r)
      | _    -> Node (l, v, insert cmp r x)

  let rec remove (cmp : 'a * 'a -> order) t x = 
    match t with 
    | Empty -> Empty
    | Node (l, v, r) ->
      match cmp (x, v) with
      | Equal -> Empty
      | Less  -> Node (remove cmp l x, v, r)
      | _     -> Node (l, v, remove cmp r x)

  let rec isIn (cmp: 'a * 'a -> order) t x = 
    match t with 
    | Empty -> false
    | Node (l, v, r) ->
      cmp (x, v) = Equal || isIn cmp l x || isIn cmp r x

  let rec find (cmp: 'a * 'a -> order) t x = 
    match t with 
    | Empty -> raise NotFound
    | Node (l, v, r) ->
      match cmp (x, v) with
      | Equal -> Node (l, v, r)
      | Less  -> find cmp l x
      | _     -> find cmp r x

  let findRoot t = 
    match t with 
    | Empty -> raise NotFound
    | Node (l, v, r) -> v

end

module Test =
  struct
    module Forest = MkForest (BinaryTree) (struct let lim = 4 end)
    let cmp (x,y) = 
      match x < y, x > y with 
      | true, _ -> Forest.T.Less 
      | _, true -> Forest.T.Greater 
      | _ -> Forest.T.Equal


    let t0 = List.fold_left 
      (fun t x -> Forest.T.insert cmp t x)
      (Forest.T.empty ())
      [4; 1; 65; 3; 8; 7; 2]
    let t1 = List.fold_left 
      (fun t x -> Forest.T.insert cmp t x)
      (Forest.T.empty ())
      [10; 9; 11; 12; 13]

    let lim = Forest.limit

    let fe : int Forest.forest = Forest.empty ()
    let f1 = Forest.addToForest fe t0
    let f2 = Forest.addToForest f1 t1
    let f3 = 
      try 
        Forest.addToTree cmp f2 3 15
      with Forest.Full -> f2
    let f4 = Forest.addToTree cmp f3 9 20
    let _ = 
      try 
        Some (Forest.findRoot cmp f4 20)
      with Forest.NotInForest -> None
    let _ = 
      try 
        Some (Forest.removeTree cmp f4 10)
      with Forest.NotInForest -> None
  end

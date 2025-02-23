(* James Bui *)

(* 
* pivotSplit : int * int list -> int list * int list
* REQUIRES: true
* ENSURES: pivotSplit (p , xs ) |-*-> ( ys , zs ) such that
* * ys@zs is a permutation of xs
* * For all x that are in ys, x < p
* * For all x that are in zs, x ≥ p
* * ys is a sublist of xs and zs is a sublist xs
*)
let rec pivotSplit ((p, xs) : int * int list) : int list * int list = 
  match xs with
  | [] -> ([], [])
  | x::xs' -> let (ys, zs) = pivotSplit(p, xs') in 
    begin
      if x < p
        then (x::ys, zs)
    else (ys, x::zs)
    end

(* 
* quicksort : int list -> int list
* REQUIRES: true
* ENSURES: quicksort lst |-*-> lst' s.t. lst' is a permutation of lst and lst' is sorted ascendingly
*)
let rec quicksort (lst: int list) : int list = 
  match lst with
  | [] -> []
  | x::xs -> let (ys, zs) = pivotSplit(x, xs) in quicksort(ys) @ [x] @ quicksort(zs)
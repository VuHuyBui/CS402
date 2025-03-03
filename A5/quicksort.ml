(* James Bui *)
type order = Greater | Less | Equal
type 'a ord = 'a * 'a -> order

(* 
* pivotSplit : 'a ord * 'a * 'a list -> 'a list * 'a list
* REQUIRES: cmp is comparison function
* ENSURES: pivotSplit ( cmp , p , lst ) |-*-> (a , b ) such that
*   * a @ b is a permutation of lst
*   * For all x that are cmp-in a, cmp (x , p ) ~= Less
*   * For all x that are cmp-in b, cmp (x , p ) ~= Greater or cmp (x , p ) ~= Equal
*   * a is a sublist of lst and b is a sublist of lst
*)

let rec pivotSplit  ( cmp , p , lst ) = 
  match lst with
  | [] -> ([], [])
  | x::xs -> let (a', b') = pivotSplit(cmp, p, xs) in 
  begin
    match cmp (x, p) with
    | Equal | Greater -> (a', x::b')
    | Less -> (x::a', b')
  end

(* 
* quicksort : 'a ord * 'a list -> 'a list
* REQUIRES: cmp is a comparison function
* ENSURES: quicksort (cmp, xs) |-*->  ys s.t. ys is a permutation of xs and ys is cmp-sorted.
 *)
let rec quicksort (cmp, xs) = 
  match xs with
  | [] -> []
  | x::xs -> let (ys, zs) = pivotSplit(cmp, x, xs) in
            quicksort (cmp, ys) @ [x] @ quicksort (cmp, zs)

            
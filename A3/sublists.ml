(* James Bui *)

(* 
* isSublist : int list * int list -> bool
* REQUIRES: true
* ENSURES: isSublist ( lst1 , lst2 ) |-*-> true if lst1 is a sublist of lst2 and false if otherwise
*)
let rec isSublist (( lst1 , lst2 ) : int list * int list) : bool = 
  match lst1 with
  | [] -> true
  | x::lst1' -> 
    begin
      match lst2 with
      | [] -> false
      | y::lst2' -> 
        if x = y
          then isSublist(lst1', lst2')  
      else isSublist(lst1, lst2')
    end

    
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
* isAscending : int list -> bool
* REQUIRES: true
* ENSURES: isAscending xs |-*-> true if all elements of xs are at least as large as any
*           previous, and isAscending xs |-*-> false otherwise 
*)
let rec isAscending ( xs : int list ) : bool =
match xs with
| [] -> true
| _ ::[] -> true
| x1 :: x2 :: xs' -> x1 <= x2 && isAscending ( x2 :: xs')

(* 
 * length : 'a list -> int
 * REQUIRES: true
 * ENSURES: length xs |-*-> n such that n is the number of elements in xs
 *)
 let rec length xs =
  match xs with
  | [] -> 0
  | _::xs' -> 1 + length xs'

(* 
 * maxAcending : (int list list * int * int list) -> int list
 * REQUIRES: true
 * ENSURES: maxAcending (sublists, maxLen, maxSublist) |-*-> ys such that
 *  * ys is the longest ascending sublist found in sublists
 *  * if multiple sublists have the same max length, the first one encountered is chosen
 *)
let rec maxAcending (sublists, maxLen, maxSublist) = 
  match sublists with
  | [] -> maxSublist
  | x::xs -> let len = length x in
          if len > maxLen && isAscending x
            then maxAcending (xs, len, x)
        else maxAcending (xs, maxLen, maxSublist)

(* 
 * ascension : int list -> int list
 * REQUIRES: true
 * ENSURES: ascension xs |-*-> ys such that
 *  * ys is a sublist of xs
 *  * isAscending ys |-*-> true
 *  * there is no longer list with these properties
 *)
let ascension (lst : int list) : int list = 
  let sublists = allSublists lst in maxAcending (sublists, 0, [])


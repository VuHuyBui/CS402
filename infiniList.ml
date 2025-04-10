type 'a infiniList = More of (unit -> 'a * 'a infiniList)
(*
* intsFrom : int -> int infiniList
* REQUIRES: true
* ENSURES: intsFrom n |-*-> an infiniList of increasing ints starting at n
*)
let rec intsFrom n = More (fun () -> n, intsFrom (n+1))
(*
* nth : 'a infiniList -> int -> 'a
* REQUIRES: n >= 0
* ENSURES: nth ilst n |-*-> the nth element of ilst
*)
let rec nth (More th) n =
match n, th () with
| 0, (m, _) -> m
| _, (_, ilst) -> nth ilst (n-1)


(*
 * map: ('a -> 'b) -> 'a infiniList -> 'b infiniList
 *)
 let rec map f (More th) = More (fun () -> match th () with
 | (m, ilst') -> (f m, map f ilst'))

(*
 * filter: ('a -> bool) -> 'a infiniList -> 'a infiniList 
 *)

let rec filter f (More th) () = More ()
type comparison = Less | Equal | Greater
(*
* compare : int -> int -> comparison
* REQUIRES: true
* ENSURES: compare x y |-*-> Less if x < y,
* Equal if x = y, and Greater if x > y
*)
let comp (x:int) (y:int) : comparison =
match (x < y), (x = y) with
| true, _ -> Less
| _, true -> Equal
| _ -> Greater

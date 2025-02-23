type intOption = None | Some of int

(*
* indexOf : int -> int list -> intOption
* REQUIRES: true
* ENSURES: intOption x xs |-*-> Some n
* if x is the nth element of xs
* and None if x is not in xs
*)
let rec find (x:int) (xs:int list) =
match xs with
| [] -> None
| y::ys ->
if y = x
then Some 0
else
match find x ys with
| None -> None
| Some n -> Some (n+1)


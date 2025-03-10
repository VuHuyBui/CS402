(* James Bui *)

type 'a rose = Rose of 'a * 'a rose list

(*
 * roseMap : ( 'a -> 'b ) -> 'a rose -> 'b rose
 * REQUIRES: true
 * ENSURES: rosePreord ( roseMap f r ) ~= List.map f ( rosePreord r )  
 *)
let rec roseMap f (Rose (x, rs)) = Rose (f x, List.map (roseMap f) rs)


(* 
 * roseFoldLeft : ( 'acc -> 'a -> 'acc ) -> 'acc -> 'a rose -> 'acc
 * REQUIRES: true
 * ENSURES: roseFoldLeft f z r ~= List.fold_left f z ( rosePreord r )
 *)
let rec roseFoldLeft f z (Rose (x, rs)) =
  List.fold_left (roseFoldLeft f) (f z x) rs

(* 
 * roseFoldRight : ( 'a -> 'acc -> 'acc ) -> 'a rose -> 'acc  -> 'acc
 * REQUIRES: true
 * ENSURES: roseFoldRight f r ~= List.fold_right f z ( rosePreord r )
 *)
 let rec roseFoldRight f (Rose (x, rs)) z =
  List.fold_right (roseFoldRight f) rs (f x z)  


(*
 * roseFilter : ( 'a -> bool ) -> 'a rose -> 'a rose option
 * REQUIRES: true
 * ENSURES: if roseFilter p r ~= None then List.filter p (rosePreord r) ~= [] and if roseFilter p r ~= Some r' then
 * rosePreord r' ~= List.filter p (rosePreord r) 
 *)
let roseFilter p r =
  roseFoldLeft 
  (
    fun acc x -> 
      match acc with
      | None -> if p x then Some (Rose (x, [])) else None
      | Some Rose(a, xs) -> if p x then Some (Rose (a, xs @ [Rose (x, [])])) else acc
  ) 
  None 
  r
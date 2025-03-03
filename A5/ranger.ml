(* James Bui *)

type ( 'a , 'b , 'c ) ranger =
| Red of 'a
| Blue of 'b
| Yellow of 'c

type ( 'a , 'b , 'c ) mech =
( 'a , 'b , 'c ) ranger
* ( 'a , 'b , 'c ) ranger
* ( 'a , 'b , 'c ) ranger

(* 
* clobber : ( 'a , 'b , ' c ) mech list -> (rs, bs, ys)'a list * 'b list * 'c list
* REQUIRES: true
* ENSURES: clobber ms |-*-> (rs, bs, ys)( xs , ys , zs ) where
*       * xs contains x for each Red x in a tuple of ms
*       * ys contains y for each Blue y in a tuple of ms
*       * zs contains z for each Yellow z in a tuple of ms
*       * none of xs , ys , zs contain any other elements
*)
let rec clobber (ms: ( 'a , 'b , ' c ) mech list) : 'a list * 'b list * 'c list  = match ms with
| [] -> ([], [], [])
| x::xs -> let (rs, bs, ys) = clobber xs in
          match x with
          | (Red r1, Red r2, Red r3) -> (r1::r2::r3::rs, bs, ys)
          | (Red r1, Red r2, Blue b) -> (r1::r2::rs, b::bs, ys)
          | (Red r1, Red r2, Yellow y) -> (r1::r2::rs, bs, y::ys)
          | (Red r1, Blue b, Red r2) -> (r1::r2::rs, b::bs, ys)
          | (Red r, Blue b1, Blue b2) -> (r::rs, b1::b2::bs, ys)
          | (Red r, Blue b, Yellow y) -> (r::rs, b::bs, y::ys)
          | (Red r1, Yellow y, Red r2) -> (r1::r2::rs, bs, y::ys)
          | (Red r, Yellow y, Blue b) -> (r::rs, b::bs, y::ys)
          | (Red r, Yellow y, Yellow y2) -> (r::rs, bs, y::y2::ys)
          | (Blue b, Red r1, Red r2) -> (r1::r2::rs, b::bs, ys)
          | (Blue b1, Red r, Blue b2) -> (r::rs, b1::b2::bs, ys)
          | (Blue b, Red r, Yellow y) -> (r::rs, b::bs, y::ys)
          | (Blue b1, Blue b2, Red r) -> (r::rs, b1::b2::bs, ys)
          | (Blue b1, Blue b2, Blue b3) -> (rs, b1::b2::b3::bs, ys)
          | (Blue b1, Blue b2, Yellow y) -> (rs, b1::b2::bs, y::ys)
          | (Blue b, Yellow y, Red r) -> (r::rs, b::bs, y::ys)
          | (Blue b, Yellow y, Blue b2) -> (rs, b::b2::bs, y::ys)
          | (Blue b, Yellow y, Yellow y2) -> (rs, b::bs, y::y2::ys)
          | (Yellow y1, Red r1, Red r2) -> (r1::r2::rs, bs, y1::ys)
          | (Yellow y, Red r, Blue b) -> (r::rs, b::bs, y::ys)
          | (Yellow y1, Red r, Yellow y2) -> (r::rs, bs, y1::y2::ys)
          | (Yellow y, Blue b, Red r) -> (r::rs, b::bs, y::ys)
          | (Yellow y, Blue b1, Blue b2) -> (rs, b1::b2::bs, y::ys)
          | (Yellow y1, Blue b, Yellow y2) -> (rs, b::bs, y1::y2::ys)
          | (Yellow y1, Yellow y2, Red r) -> (r::rs, bs, y1::y2::ys)
          | (Yellow y1, Yellow y2, Blue b) -> (rs, b::bs, y1::y2::ys)
          | (Yellow y1, Yellow y2, Yellow y3) -> (rs, bs, y1::y2::y3::ys)



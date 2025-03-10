(* James Bui *)
type poly = int -> float
(* 
 * polynomialEqual : poly * poly * int -> bool
 * REQUIRES: p and q are valid polynomials and d is at least as big as the maximum degree
 * across p and q (i.e., at least as big as the largest argument for which p or q returns a
 * nonzero result)
 * ENSURES: polynomialEqual (p , q , d ) |-*-> true iff p and q are equal as polynomials (within error), and otherwise polynomialEqual (p , q , d ) |-*-> false
 *)
let rec polynomialEqual (( p : poly ) , ( q : poly ) , ( d :int) ) : bool =
  abs_float ( p d -. q d ) < 0.0001 &&
  match d with
  | 0 -> true
  | _ -> polynomialEqual (p , q , d - 1)


(* 
 * differentiate : poly -> poly
 * REQUIRES: p is a valid polynomial (i.e. p n ~= 0.0 for n < 0 and p n ~= 0.0 for all
 * but finitely many n)
 * ENSURES: differentiate p ~= p', where p' is the derivative of p
 *)
let differentiate (p: poly) x =
  float_of_int (x + 1) *. p (x + 1)

(*
 * integrate : poly -> ( float -> poly )
 * REQUIRES: p is a valid polynomial (i.e. p n ~= 0.0 for n < 0 and p n ~= 0.0 for all
 * but finitely many n)
 * ENSURES: integrate p ~= q, where q c is the antiderivative of p with constant of integration c.
 * Alternatively, you could say that differentiate (( integrate p ) c ) ~= p for all c : float. 
 *)
let integrate (p: poly) (c: float) (d: int) =
  p (d - 1) /. float_of_int (d)
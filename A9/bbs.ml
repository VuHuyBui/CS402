(* James Bui *)


(*
 * bbs : int-> int-> int Seq.t
 * REQUIRES: m is the product of two primes
 * ENSURES: bbs m s ~= a stream of integers representing a Blum-Blum-Shub PRNG with
 * modulus m and seed s. The first element in the stream is x1 as defined above. 
 *)

 let rec bbs (m: int) (s: int) : int Seq.t = 
  fun () -> Seq.Cons((s * s) mod m, bbs m ((s * s) mod m)) 
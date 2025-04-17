(* James Bui *)

(*  
 * concat : 'a Seq.t Seq.t -> 'a Seq.t
 * REQUIRES: s is productive
 * ENSURES:
 * * concat s |-*-> s', a stream containing the elements of s concatenated together.
 * * concat s is maximally lazy (evaluating concat s never exposes any elements of s). 
 *)
let rec concat s () =
  match s () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, s') -> 
    begin
      match x () with
      | Seq.Nil -> concat s' ()
      | Seq.Cons (y, ys) -> Seq.Cons(y, concat (fun () -> Seq.Cons(ys, s')))
    end


(* 
 * cycle : 'a Seq.t-> 'a Seq.t
 * REQUIRES: s is productive
 * ENSURES:
 * * cycle s is productive
 * * cycle s |-*-> s' a stream formed by infinitely repeating the elements of s
 * * If s ~= Seq.Nil, then cycle s ~= Seq.empty
 * * cycle s is maximally lazy (evaluating cycle s never exposes any elements of s). 
 *)
let rec cycle s () = 
  match s () with
  | Seq.Nil -> Seq.Nil
  | _ -> concat (fun () -> Seq.Cons(s, fun () -> Seq.Cons(cycle s, fun () -> Seq.Nil))) ()


(*
  interleave : 'a Seq.t -> 'a Seq.t -> 'a Seq.t

  REQUIRES: s1 and s2 are productive

  ENSURES:
    - interleave s1 s2 is productive
    - interleave s1 s2 |-*-> s, where s contains alternating elements 
      from s1 and s2, starting with the first element of s1
    - If one stream ends early, the rest of the other stream is 
      appended in its entirety
    - interleave s1 s2 is maximally lazy (evaluating interleave s1 s2 
      never exposes any elements of s1 or s2)
*)

let rec interleave s1 s2 () =
  match s1 (), s2 () with
  | Seq.Nil, _ -> s2 ()
  | _, Seq.Nil -> s1 ()
  | Seq.Cons(x1, xs1),  Seq.Cons(x2, xs2) -> Seq.Cons(x1, fun () -> Seq.Cons(x2, interleave xs1 xs2))


(*
  double : 'a Seq.t -> 'a Seq.t

  REQUIRES: s is productive

  ENSURES:
    - double s is productive
    - double s |-*-> s', a stream containing the same elements as s,
      but with each element appearing twice in a row
    - double s is maximally lazy (evaluating double s never exposes
      any elements of s)
*)

let rec double s () = 
  match s () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, xs) -> Seq.Cons(x, fun () -> Seq.Cons(x, double xs))


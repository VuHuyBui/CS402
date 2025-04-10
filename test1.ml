let rec pointwise (f: 'a -> 'b -> 'c) (l1: 'a Seq.t) (l2: 'b Seq.t) () = 
  match l1 (), l2 () with
  | Seq.Nil, _ | _, Seq.Nil-> Seq.Nil
  | Seq.Cons (x, l1'), Seq.Cons (y, l2') -> Seq.Cons (f x y, pointwise f l1' l2')

let swap x y l1 (): int-> int -> 'a Seq.t->'a Seq.t = 

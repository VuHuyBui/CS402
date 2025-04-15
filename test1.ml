let rec pointwise (f: 'a -> 'b -> 'c) (l1: 'a Seq.t) (l2: 'b Seq.t) () = 
  match l1 (), l2 () with
  | Seq.Nil, _ | _, Seq.Nil-> Seq.Nil
  | Seq.Cons (x, l1'), Seq.Cons (y, l2') -> Seq.Cons (f x y, pointwise f l1' l2')

(* let swap x y l1 (): int-> int -> 'a Seq.t->'a Seq.t =  *)



module type BINOM = sig
  val binom: int -> int -> int
end


module Binom: BINOM = struct
  let saved = Hashtbl.create 100
  let memoize x y th = 
    match Hashtbl.find_opt saved (x, y) with
    | Some v -> v
    | None -> 
      let res = th () in 
      let () = Hashtbl.add saved (x, y) res in res
  
  let rec binom n k = 
    memoize n k (fun () -> 
      match k with
      | 0 -> 1
      | _ -> if n < k then 0 else  binom (n - 1) k + binom (n - 1) (k - 1)
      )
end
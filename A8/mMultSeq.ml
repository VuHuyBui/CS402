
module type MMULTSEQ = sig 

  include Sequence.SEQUENCE

  type 'a matrix = 'a seq seq

  val mmult : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a matrix -> 'a matrix -> 'a matrix

end


module MkMMult (S : Sequence.SEQUENCE) : MMULTSEQ = struct 

  include S

  type 'a matrix = 'a seq seq

  (* YOUR CODE GOES BELOW THIS LINE *)

  let dot add mul c1 c2 = reduce1 (fun (x, y) ->  add x y ) (zipWith (fun (x, y) ->  mul x y) (c1, c2))
  let get_col j s2 = map (fun row -> nth row j) s2

  let mmult add mul s1 s2 = 
    if null s1 then empty ()
    else
      let nrow, ncol = length s1, length (nth s2 0) in
      tabulate (fun i ->
        tabulate (fun j ->
          dot add mul (nth s1 i) (get_col j s2)
        ) ncol
      ) nrow

  (* YOUR CODE GOES ABOVE THIS LINE *)

end
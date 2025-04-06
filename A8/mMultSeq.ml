
module type MMULTSEQ = sig 

  include Sequence.SEQUENCE

  type 'a matrix = 'a seq seq

  val mmult : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a matrix -> 'a matrix -> 'a matrix

end


module MkMMult (S : Sequence.SEQUENCE) : MMULTSEQ = struct 

  include S

  type 'a matrix = 'a seq seq

  (* YOUR CODE GOES BELOW THIS LINE *)

  let mmult = failwith "TODO"

  (* YOUR CODE GOES ABOVE THIS LINE *)

end
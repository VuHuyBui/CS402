
module type TRANSPOSESEQ = sig 

  include Sequence.SEQUENCE

  type 'a matrix = 'a seq seq

  val transpose : 'a matrix -> 'a matrix

end


module MkTranspose (S : Sequence.SEQUENCE) : TRANSPOSESEQ = struct 

  include S

  type 'a matrix = 'a seq seq

  (* YOUR CODE GOES BELOW THIS LINE *)

  let transpose = failwith "TODO"

  (* YOUR CODE GOES ABOVE THIS LINE *)

end
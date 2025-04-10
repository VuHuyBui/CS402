module type TRANSPOSESEQ = sig 

  include Sequence.SEQUENCE

  type 'a matrix = 'a seq seq

  val transpose : 'a matrix -> 'a matrix

end


module MkTranspose (S : Sequence.SEQUENCE) : TRANSPOSESEQ = struct 

  include S

  type 'a matrix = 'a seq seq

  (* YOUR CODE GOES BELOW THIS LINE *)

  let rec transpose s = 
    if null s then empty ()
    else
    let row, col = length s, length (nth s 0) in 
    tabulate (fun i ->
      tabulate (fun j -> nth (nth s j) i) row
    ) col
  (* YOUR CODE GOES ABOVE THIS LINE *)

end
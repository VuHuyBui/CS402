
module type EXISTSSEQ = sig 

  include Sequence.SEQUENCE

  val exists : ('a -> bool) -> 'a seq -> bool

end


module MkExists (S : Sequence.SEQUENCE) : EXISTSSEQ = struct 

  include S

  (* YOUR CODE GOES BELOW THIS LINE *)

  let exists p s = mapreduce p false (fun (x, y) -> x || y) s

  (* YOUR CODE GOES ABOVE THIS LINE *)
  
end


module type ARRAY = sig
  type 'a arr
  
  val make: int -> 'a -> 'a arr

  val length: 'a arr -> int

  val get: 'a arr -> int -> 'a

  val set: 'a arr -> int -> 'a -> unit
end

module Arr: ARRAY = struct
  module M = Map.Make(Int)
  type 'a arr = int * 'a M.t ref

  let make n x = 
    let m = ref M.empty
    for i = 0 to n - 1 do
      m := M.add i x !m 
    done in (n, m)

  let length (a, m) = a

  let get (n, m) i = 
    M.find 
end

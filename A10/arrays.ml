(* James Bui *)

(* 
 * monotonize : ( 'a -> 'a -> bool ) -> 'a array -> unit
 * REQUIRES: cmp x y |-*-> true if x <= y in some ordering and cmp x y |-*-> false otherwise
 * ENSURES: monotonize cmp a mutates a so that, if ever it would be the case that cmp a .(i) a .(i + 1) |-*-> false, 
 * instead a.(i + 1) is mutated to be equal to a .(i)
 *)
let monotonize cmp a = 
  let n = Array.length a in
  for i = 0 to n - 2 do 
    if not (cmp a.(i) a.(i + 1))
      then a.(i + 1) <- a.(i)
  done

(*
 * swapBigSmall : ( 'a -> 'a -> bool ) -> 'a array -> unit
 * REQUIRES: cmp x y |-*-> true if x <= y in some ordering and cmp x y |-*-> false otherwise
 * ENSURES: swapBigSmall a mutates a so that all the the indices of a that originally
 * contained the smallest element in a according to cmp now contain the biggest element
 * originally in a according to cmp, and vice versa. 
 *)
let swapBigSmall (cmp: 'a -> 'a -> bool) (a: 'a array): unit = 
  if Array.length a > 0 then
    let min = Array.fold_left (fun acc x -> if cmp x acc then x else acc) a.(0) a in
    let max = Array.fold_left (fun acc x -> if cmp acc x then x else acc) a.(0) a in
    Array.map_inplace (fun x ->
      if x = max then min
      else if x = min then max
      else x
    ) a


let merge cmp a l m r = 
  let l2 = ref (m + 1)in
  let l1 = ref l in
  let mid = ref m in
  if cmp a.(!mid) a.(!l2) then ()
  else 
    while (!l1 <= !mid) && !l2 <= r do 
      if cmp a.(!l1) a.(!l2) then l1 := !l1 + 1 
      else let value = a.(!l2) in
      for i = !l2 - 1 downto !l1 do 
        a.(i + 1) <- a.(i)
      done; a.(!l1) <- value;
      l1 := !l1 + 1;
      mid := !mid + 1;
      l2 := !l2 + 1;

    done

let rec mergesort cmp a low high = 
  if low < high then 
    let mid = (low + high) / 2 in
    mergesort cmp a low mid; mergesort cmp a (mid + 1) high;
    merge cmp a low mid high


(*
 * sort : ( 'a -> 'a -> bool ) -> 'a array -> unit
 * REQUIRES: cmp x y |-*-> true if x <= y in some ordering and cmp x y |-*-> false otherwise
 * ENSURES: sort cmp a mutates a to be ascendingly sorted according to cmp 
 *)
let sort cmp a = 
  let n = Array.length a in
  mergesort cmp a 0 (n - 1)



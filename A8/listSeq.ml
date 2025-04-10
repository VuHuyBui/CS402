
(*
 * The interface for parallel collections
 *
 * work and span bounds are provided assuming any arguments
 * of function type operate in O(1) work and span
 *)
 module type SEQUENCE = sig

  (* the type of sequences *)
  type 'a seq 

  (* an exception for when indices are out of range *)
  exception Range of string 


  (* CONSTRUCTING SEQUENCES *)

  (* 
   * empty : unit -> 'a seq
   * REQUIRES: true
   * ENSURES: empty () evaluates to the empty sequence of length 0
   * work: O(1)
   * span: O(1)
   *)
  val empty : unit -> 'a seq

  (* 
   * singleton : 'a -> 'a seq
   * REQUIRES: true
   * ENSURES: singleton x evaluates to the sequence 
   *  of length 1 containing only x
   * work: O(1)
   * span: O(1)
   *)
  val singleton : 'a -> 'a seq

  (* 
   * tabulate : (int -> 'a) -> int -> 'a seq
   * REQUIRES: n >= 0 and f i is valuable for all 0 <= i <= n
   * ENSURES: tabulate f n evaluates to a sequence s of length n, 
   *  where the ith element of s is equal to f i
   * work: O(n), or more generally O(sum from i=0 to n-1 of the work of f applied to i)
   * span: O(1), or more generally O(max from i=0 to n-1 of the span of f applied to i)
   *)
  val tabulate : (int -> 'a) -> int -> 'a seq

  (* 
   * fromList : 'a list -> 'a seq
   * REQUIRES: true
   * ENSURES: fromList lst evaluates to a sequence consisting of the elements of lst, preserving order
   * work: O(|lst|)
   * span: O(|lst|)
   *)
  val fromList : 'a list -> 'a seq


  (* DECONSTRUCTING SEQUENCES *)

  (* 
   * nth : 'a seq -> int -> 'a
   * REQUIRES: 0 <= i < |s|
   * ENSURES: nth s i evaluates to s[i], the ith element (zero-indexed) of s
   * work: O(1)
   * span: O(1)
   *)
  val nth : 'a seq -> int -> 'a

  (*
   * null : 'a seq -> bool
   * REQUIRES: true
   * ENSURES: null s evaluates to true if s is an empty sequence, and false otherwise
   * work: O(1) 
   * span: O(1)
   *)
  val null : 'a seq -> bool

  (*
   * length : 'a seq -> int
   * REQUIRES: true
   * ENSURES: length s evaluates to |s|, the number of items in s
   * work: O(1) 
   * span: O(1)
   *)
  val length : 'a seq -> int

  (*
   * toList : 'a seq -> 'a list
   * REQUIRES: true
   * ENSURES: toList s returns a list consisting of the elements of s, preserving order
   * work: O(|s|) 
   * span: O(|s|)
   *)
  val toList : 'a seq -> 'a list

  (*
   * toString : ('a -> string ) -> 'a seq -> string
   * REQUIRES: true
   * ENSURES: toString ts s evaluates to a string representation of s, 
   *  using the function ts to convert each element of s into a string
   * work: O(|s|) 
   * span: O(log(|s|))
   *)
  val toString : ('a -> string) -> 'a seq -> string

  (*
   * equal : ('a * 'a -> bool ) -> 'a seq * 'a seq -> bool
   * REQUIRES: true
   * ENSURES: equal eq (s1, s2) returns whether or not the two 
   *  sequences are equal according to the equality function eq
   * work: O(min(|s1|,|s2|)) 
   * span: O(log(min(|s1|,|s2|)))
   *)
  val equal : ('a * 'a -> bool) -> 'a seq * 'a seq -> bool


  (* SIMPLE TRANSFORMATIONS *)

  (*
   * rev : 'a seq -> 'a seq
   * REQUIRES: true
   * ENSURES: rev S returns the sequence containing the elements of S in reverse order
   * work: O(|s|) 
   * span: O(1)
   *)
  val rev : 'a seq -> 'a seq

  (*
   * append : 'a seq * 'a seq -> 'a seq
   * REQUIRES: true
   * ENSURES: append (s1, s2) evaluates to a sequence of length |s1| + |s2| 
   *  whose first |s1| elements are the sequence s1 and
   *  whose last |s2| elements are the sequence s2
   * work: O(|s|) 
   * span: O(1)
   *)
  val append : 'a seq * 'a seq -> 'a seq

  (*
   * flatten : 'a seq seq -> 'a seq
   * REQUIRES: true
   * ENSURES: flatten s flattens a sequence of sequences down to a single sequence
   *  maintaining relative order
   * work: O(|s| + sum over x in s of |x|) 
   * span: O(log(|s|))
   *)
  val flatten : 'a seq seq -> 'a seq

  (*
   * cons : 'a -> 'a seq -> 'a seq
   * REQUIRES: true
   * ENSURES: If the length of s is n, cons x s evaluates to a sequence of length n+1 whose
   *  first item is x and whose remaining n items are exactly the sequence s
   * work: O(|s|) 
   * span: O(1)
   *)
  val cons : 'a -> 'a seq -> 'a seq


  (* COMBINATORS AND HIGHER-ORDER FUNCTIONS *)

  (*
   * filter : ('a -> bool) -> 'a seq -> 'a seq
   * REQUIRES: true
   * ENSURES: filter p s evaluates to a sequence containing all of the elements x of s such
   *  that p x |-*-> true, preserving element order.
   * work: O(|s|), or more generally O(sum over x in s of the work of p applied to x)
   * span: O(log(|s|)), or more generally O(log(|s|) + max over x in s of the span of p applied to x)
   *)
  val filter : ('a -> bool) -> 'a seq -> 'a seq

  (*
   * map : ('a -> 'b) -> 'a seq -> 'b seq
   * REQUIRES: true
   * ENSURES: map f s |-*-> s' such that |s| = |s'| and for all 0 <= i < |s'|, nth s' i ∼= f (nth s i)
   * work: O(|s|), or more generally O(sum over x in s of the work of f applied to x)
   * span: O(1), or more generally O(max over x in s of the span of f applied to x)
   *)
  val map : ('a -> 'b) -> 'a seq -> 'b seq

  (*
   * reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
   * REQUIRES: g is associative and z is the identity element for g
   * ENSURES: reduce g z s uses the function g to combine the elements of s using z as a base case
   * work: O(|s|)
   * span: O(log(|s|))
   *)
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a

  (*
   * reduce1 : ('a * 'a -> 'a) -> 'a seq -> 'a
   * REQUIRES: g is associative and s nonempty
   * ENSURES: reduce1 g s uses the function g to combine the elements of s; 
   *  if S is a singleton sequence, the sequence element is returned
   * work: O(|s|)
   * span: O(log(|s|))
   *)
  val reduce1 : ('a * 'a -> 'a) -> 'a seq -> 'a

  (*
   * mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b
   * REQUIRES: g is associative and z is the identity element for g
   * ENSURES: mapreduce f z g s ∼= reduce g z (map f s)
   * work: O(|s|)
   * span: O(log(|s|))
   *)
  val mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b

  (*
   * zip : ('a seq * 'b seq) -> ('a * 'b) seq
   * REQUIRES: true
   * ENSURES: zip (s1, s2) evaluates to a sequence of length min(|s1|, |s2|)
   *  whose ith element is (s1[i], s2[i])
   * work: O(min(|s1|,|s2|))
   * span: O(1)
   *)
  val zip : ('a seq * 'b seq) -> ('a * 'b) seq

  (*
   * zipWith : ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq
   * REQUIRES: true
   * ENSURES: zipWith f (s1, s2) ∼= map f (zip (s1, s2))
   * work: O(min(|s1|,|s2|)), or more generally O(sum from i=0 to min(|s1|,|s2|)-1 of the work of f applied to (s1[i], s2[i]))
   * span: O(1), or more generally O(max from i=0 to min(|s1|,|s2|)-1 of the span of f applied to (s1[i], s2[i]))
   *)
  val zipWith : ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq


  (* INDEXING-RELATED FUNCTIONS *)

  (*
   * enum : 'a seq -> (int * 'a) seq
   * REQUIRES: true
   * ENSURES: enum s evaluates to a sequence such that for each index 0 <= i < |s|,
   *  the ith index of the result is (i , s[i]).
   * work: O(|s|)
   * span: O(1)
   *)
  val enum : 'a seq -> (int * 'a) seq

  (*
   * mapIdx : (int * 'a -> 'b) -> 'a seq -> 'b seq
   * REQUIRES: true
   * ENSURES: mapIdx f s ∼= map f (enum s)
   * work: O(|s|), or more generally O(sum over x in s of the work of f applied to x)
   * span: O(1), or more generally O(max over x in s of the span of f applied to x)
   *)
  val mapIdx : (int * 'a -> 'b) -> 'a seq -> 'b seq

  (*
   * update : ('a seq * (int * 'a)) -> 'a seq
   * REQUIRES: 0 <= i < |s|
   * ENSURES: update (s, (i,x)) evaluates to a sequence identical to s but with the ith element (0-indexed) now x
   * work: O(|s|)
   * span: O(1)
   *)
  val update : ('a seq * (int * 'a)) -> 'a seq

  (*
   * inject : 'a seq * (int * 'a ) seq -> 'a seq
   * REQUIRES: for all (i, x) in u, 0 <= i < |s|
   * ENSURES: inject (s, u) evaluates to a sequence where for each (i, x) in u, the ith
   *  element of s is replaced with x; if there are multiple elements at the same index, 
   *  one is chosen nondeterministically
   * work: O(|s| + |u|)
   * span: O(1)
   *)
  val inject : 'a seq * (int * 'a ) seq -> 'a seq

  (*
   * subseq : 'a seq -> int * int -> 'a seq
   * REQUIRES: 0 <= i < |s|, and i <= i + l < |s|
   * ENSURES: subseq s (i, l) evaluates to the subsequence of s with length l starting at index i
   * work: O(1)
   * span: O(1)
   *)
  val subseq : 'a seq -> int * int -> 'a seq

  (*
   * take : 'a seq -> int -> 'a seq
   * REQUIRES: 0 <= i <= |s|
   * ENSURES: take s i evaluates to the sequence containing exactly the first i elements of s
   * work: O(1)
   * span: O(1)
   *)
  val take : 'a seq -> int -> 'a seq

  (*
   * drop : 'a seq -> int -> 'a seq
   * REQUIRES: 0 <= i <= |s|
   * ENSURES: take s i evaluates to the sequence containing all except for the first i elements of s
   * work: O(1)
   * span: O(1)
   *)
  val drop : 'a seq -> int -> 'a seq

  (*
   * split : 'a seq -> int -> 'a seq * 'a seq
   * REQUIRES: 0 <= i <= |s|
   * ENSURES: split s i evaluates to a pair of sequences (s1, s2) 
   *  such that s1 has length i and append (s1, s2) ∼= s
   * work: O(1)
   * span: O(1)
   *)
  val split : 'a seq -> int -> 'a seq * 'a seq


  (* SORTING AND SEARCHING *)

  (* a type for comparisons *)
  type order = Less | Equal | Greater

  (*
   * sort : ('a * 'a -> order) -> 'a seq -> 'a seq
   * REQUIRES: cmp is a valid comparison function
   * ENSURES: sort cmp s evaluates to a permutation of s that is sorted according to cmp.
   *  The sort is stable: elements that are considered equal by cmp
   *  remain in the same order they were in s.
   * work: O(|s| * log(|s|))
   * span: O(log(|s|)^2)
   *)
  val sort : ('a * 'a -> order) -> 'a seq -> 'a seq

  (*
   * merge : ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
   * REQUIRES: cmp is a valid comparison function, s1 and s2 are both cmp-sorted
   * ENSURES: merge cmp (s1, s2) evaluates to a sorted permutation of append (s1, s2)
   * work: O(|s1| + |s2|)
   * span: O(log(|s1| + |s2|))
   *)
  val merge : ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq

  (*
   * search : ('a * 'a -> order) -> 'a -> 'a seq -> int option
   * REQUIRES: cmp is a valid comparison function, s is cmp-sorted
   * ENSURES:  search cmp x s |-*-> Some i where i is the first index in s satisfying
   *  cmp (s[i], x) ∼= Equal or None if no such index exists
   * work: O(log(|s|))
   * span: O(log(|s|))
   *)
  val search : ('a * 'a -> order) -> 'a -> 'a seq -> int option


  (* VIEWS *)

  (* a convenience type for pattern matching sequences like lists *)
  type 'a lview = Nil | Cons of 'a * 'a seq

  (*
   * showl : 'a seq -> 'a lview
   * REQUIRES: true
   * ENSURES: showl s |-*-> Nil if s is empty, otherwise to Cons (s[0], drop 1 s)
   * work: O(1)
   * span: O(1)
   *)
  val showl : 'a seq -> 'a lview

  (* 
   * hidel : 'a lview -> 'a seq
   * REQUIRES: true 
   * ENSURES: hidel Nil ~= an empty sequence
   *  and hidel Cons (x, s) ~= cons x s
   * work: O(1)
   * span: O(1)
   *)
  val hidel : 'a lview -> 'a seq


  (* a convenience type for pattern matching sequences like trees *)
  type 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  (*
   * showt : 'a seq -> 'a tview
   * REQUIRES: true
   * ENSURES: showt s |-*-> Empty if s is empty, 
   *  showt s |-*-> Leaf x if s contains only the single element x
   *  showt s |-*-> Node (s1, s2) otherwise where append (s1, s2) ~= s
   * work: O(1)
   * span: O(1)
   *)
  val showt : 'a seq -> 'a tview

  (*
   * hidet : 'a tview -> 'a seq
   * REQUIRES: true
   * ENSURES: hidet Empty ~= an empty sequence,
   *  hidet (Leaf x) ~= a sequence containing only the single element x,
   *  and hidet (Node (s1, s2)) ~= append (s1, s2)
   * work: O(1)
   * span: O(1)
   *)
  val hidet : 'a tview -> 'a seq

end 

module type TRANSPOSESEQ = sig 

  include SEQUENCE

  type 'a matrix = 'a seq seq

  val transpose : 'a matrix -> 'a matrix

end


module MkTranspose (S : SEQUENCE) : TRANSPOSESEQ = struct 

  include S

  type 'a matrix = 'a seq seq

  (* YOUR CODE GOES BELOW THIS LINE *)

  let rec transpose s = 
    let row, col = length s, length (nth s 0) in 
    tabulate (fun i ->
      tabulate (fun j -> nth (nth s j) i) row
    ) col
  (* YOUR CODE GOES ABOVE THIS LINE *)

end

module type MMULTSEQ = sig 

  include SEQUENCE

  type 'a matrix = 'a seq seq

  val mmult : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a matrix -> 'a matrix -> 'a matrix

end


module MkMMult (S : SEQUENCE) : MMULTSEQ = struct 

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
(* a dummy implementation of SEQUENCE; does not satisfy asymptotic bounds *)
module ListSeq = struct 

  type 'a seq = 'a list

  exception Range of string 


  (* Constructing a Sequence *)

  let empty () = []
  let singleton x = [x]
  let tabulate f n = 
    if n < 0 
    then raise (Range ("tabulate given negative length " ^ (string_of_int n)))
    else List.init n f
  let fromList lst = lst


  (* Deconstructing a Sequence *)

  let nth lst n = 
    if n < 0 
    then raise (Range ("nth given negative index " ^ (string_of_int n)))
    else 
      let len = List.length lst in 
      if n > len
      then raise (Range ("nth given index " ^ (string_of_int n) ^ " which is out of range for seq of length " ^ (string_of_int len)))
      else List.nth lst n
  let null = List.is_empty
  let length = List.length
  let toList lst = lst
  let toString f lst = "<" ^ (String.concat ";" (List.map f lst)) ^ ">"
  let equal eq (s1,s2) = try List.for_all2 (fun x y -> eq (x,y)) s1 s2 with Invalid_argument _ -> false 
  
  
  (* Simple Transformations *)

  let rev = List.rev
  let append (s1,s2) = List.append s1 s2
  let flatten = List.flatten
  let cons = List.cons


  (* Combinators and Higher-Order Functions *)

  let filter = List.filter
  let map = List.map
  let reduce f z lst = List.fold_left (fun a b -> f (a,b)) z lst
  let rec reduce1 f lst = 
    match lst with 
    | x::[] -> x 
    | x::xs -> f (x, (reduce1 f xs))
    | _ -> raise (Range "empty seq given to reduce1")

  let mapreduce f z g lst = reduce g z (map f lst)
  let zip (lst1,lst2) = List.combine lst1 lst2
  let zipWith f (lst1,lst2) = map f (zip (lst1, lst2))


 (* Indexing-Related Functions *)

  let enum lst = List.mapi (fun i x -> (i,x)) lst
  let mapIdx f lst = map f (enum lst)

  let update (lst, (i, x)) = 
    let len = List.length lst in
    if i >= len
    then raise (Range ("mapIdx given index " ^ (string_of_int i) ^ " which is out of range for seq of length " ^ (string_of_int len)))
    else List.mapi (fun j y -> if i = j then x else y) lst

  let inject (lst1, lst2) =
    let len = List.length lst1 in
    let maxElt = List.fold_left (fun acc (i,_) -> max acc i) (-1) lst2 in
    if maxElt >= len
    then raise (Range ("inject given index " ^ (string_of_int maxElt) ^ " which is out of range for seq of length " ^ (string_of_int len)))
    else List.fold_left (fun acc (i,x) -> update (acc, (i, x))) lst1 lst2

  let subseq lst (start, len) = 
    if start + len > List.length lst 
    then raise (Range ("subseq given (" ^ (string_of_int start) ^ "," ^ (string_of_int len) ^ ") goes out of range for seq of length " ^ (string_of_int len)))
    else
      lst 
      |> (try List.drop start with Invalid_argument _ -> raise (Range ("subseq given negative starting index " ^ (string_of_int start))))
      |> (try List.take len with Invalid_argument _ -> raise (Range ("subseq given negative length " ^ (string_of_int len))))
    

  let take lst n = 
    let len = List.length lst in
    if n > len || n < 0
    then raise (Range ("take given index " ^ (string_of_int n) ^ " which is out of range for seq of length " ^ (string_of_int len)))
    else List.take n lst

  let drop lst n = 
    let len = List.length lst in
    if n > len || n < 0
    then raise (Range ("drop given index " ^ (string_of_int n) ^ " which is out of range for seq of length " ^ (string_of_int len)))
    else List.drop n lst

  let split lst n = 
    let len = List.length lst in
    if n > len || n < 0 
    then raise (Range ("split given index " ^ (string_of_int n) ^ " which is out of range for seq of length " ^ (string_of_int len)))
    else (List.take n lst, List.drop n lst)


  (* Sorting and Searching *)

  type order = Less | Equal | Greater

  let sort cmp = List.stable_sort (fun a b -> match cmp (a, b) with Less -> -1 | Equal -> 0 | Greater -> 1)

  let merge cmp (lst1, lst2) = List.merge (fun a b -> match cmp (a, b) with Less -> -1 | Equal -> 0 | Greater -> 1) lst1 lst2
  let search cmp x = List.find_index (fun y -> cmp (x,y) = Equal)


  (* Views *)

  type 'a lview = Nil | Cons of 'a * 'a seq

  let showl lst = match lst with [] -> Nil | x::xs -> Cons (x,xs)
  let hidel lst = match lst with Nil -> [] | Cons (x,xs) -> x::xs

  type 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  let showt lst = 
    match lst with 
    | [] -> Empty 
    | x::[] -> Leaf x 
    | _ -> 
      let len = List.length lst in 
      Node (List.take (len/2) lst, List.drop (len/2) lst)

  let rec hidet t = 
    match t with 
    | Empty -> [] 
    | Leaf x -> [x]
    | Node (t1, t2) -> t1 @ t2

end


module LM = MkMMult(ListSeq)
module LT = MkTranspose(ListSeq)

let create_2d_list rows cols f =
  LT.tabulate (fun i ->
    LT.tabulate (fun j -> f i j) cols
  ) rows

let l1 = create_2d_list 4 5 (fun x y ->  x + y)
let l2 = create_2d_list 5 3 (fun x y ->  x + y)

let l3 = LT.transpose l1

let sl1 = LT.showl (LT.map LT.showl l1)
let sl2 = LT.showl (LT.map LT.showl l2)
let sl3 = LT.showl (LT.map LT.showl l3)

(* Function to print a single lview element (row in the 2D sequence) *)
let rec print_lview f lview =
  match lview with
  | LT.Nil -> print_endline ""  (* End of a row, print a newline *)
  | LT.Cons (x, xs) ->
      f x;  (* Print the element using the function f *)
      print_lview f (LT.showl xs)  (* Recursively print the rest of the sequence *)

(* Function to print a 2D lview sequence *)
let print_2d_lview f seq =
  let rec aux s =
    match s with
    | LT.Nil -> ()  (* End of the 2D sequence *)
    | LT.Cons (row, rest) ->
        print_lview f row;  (* Print the current row *)
        aux (LT.showl rest)   (* Print the next row *)
  in
  aux seq

(* Example usage *)

let () =
  (* Print the 2D sequence where each element is an integer *)
  print_2d_lview (fun x -> Printf.printf "%d " x) sl1;
  print_2d_lview (fun x -> Printf.printf "%d " x) sl2;
  print_2d_lview (fun x -> Printf.printf "%d " x) sl3;


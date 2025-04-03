

  module type NAT = 
    sig 

      (* the type representing naturals *)
      type nat 

      (* the representation of 0 *)
      val zero : nat 


      (* 
      * succ : nat -> nat 
      * REQUIRES: true
      * ENSURES: succ n |-*-> m s.t. if 
      *  n represents the natural x 
      *  them m represents x + 1
      *)
      val succ : nat -> nat 

      (* 
      * fold : ('a -> 'a) -> 'a -> nat -> 'a
      * REQUIRES: true
      * ENSURES: fold f s z n ~= f (f ... (f z) ... )
      *  where, if n represents the natural x,
      *  there are x applications of f
      *)
      val fold : ('a -> 'a) -> 'a -> nat -> 'a

    end


  module IntNat : NAT = 
    struct

      type nat = int 

      let zero = 0

      let succ n = n + 1 

      let rec fold s z n = 
        match n with 
        | 0 -> z 
        | _ -> fold s (s z) (n - 1)

    end


  module ListNat : NAT = 
    struct 

      type nat = unit list 

      let zero = [] 

      let succ lst = ()::lst 

      let fold s z n = List.fold_left (fun acc _ -> s acc) z n

    end


  module UnNat : NAT = struct 

    type nat = Z | S of nat 

    let zero = Z 

    let succ n = S n 

    let rec fold s z n = 
      match n with 
      | Z -> z
      | S n' -> fold s (s z) n'

  end


  module BinNat : NAT = struct 

    type nat = End | Zero of nat | One of nat

    let zero = End

    let rec succ n = 
      match n with 
      | End -> One End
      | Zero m -> One m 
      | One m -> Zero (succ m)


    let rec fold s z n = 
      match n with 
      | End -> z 
      | Zero m -> fold s (fold s z m) m
      | One m -> fold s (fold s z m) m |> s

  end 
    

  module type OPNAT = 
    sig 

      include NAT 

      (* 
      * add : nat -> nat -> nat 
      * REQUIRES: true
      * ENSURES: add m n |-*-> p s.t.
      *  if m represents the natural x 
      *  and n represents the natural y
      *  then p represents x + y
      *)
      val add : nat -> nat -> nat 

      (* 
      * mul : nat -> nat -> nat 
      * REQUIRES: true
      * ENSURES: mul m n |-*-> p s.t.
      *  if m represents the natural x 
      *  and n represents the natural y
      *  then p represents x * y
      *)
      val mul : nat -> nat -> nat 

      (* 
      * eq : nat -> nat -> bool
      * REQUIRES: true
      * ENSURES: eq m n |-*-> true if 
      *  m and n represent the same natural,
      *  eq m n |-*-> false otherwise 
      *)
      val eq : nat -> nat -> bool

      (*
      * nat_of_int : int -> nat
      * REQUIRES: z >= 0
      * ENSURES: nat_of_int z |-*-> n s.t.
      *  n represents z
      *)
      val nat_of_int : int -> nat

    end 


  module type INT = sig 

    (* the type representing integers *)
    type int'

    (* the type representing the naturals that int' is built out of *)
    type nat

    (* 
    * add : int' -> int' -> int'
    * REQUIRES: true
    * ENSURES: add m n |-*-> p s.t.
    *  if m represents the integer x 
    *  and n represents the integer y
    *  then p represents x + y
    *)
    val add : int' -> int' -> int'

    (* 
    * mul : int' -> int' -> int'
    * REQUIRES: true
    * ENSURES: mul m n |-*-> p s.t.
    *  if m represents the integer x 
    *  and n represents the integer y
    *  then p represents x * y
    *)
    val mul : int' -> int' -> int'

    (* 
    * neg : int' -> int'
    * REQUIRES: true
    * ENSURES: neg n |-*-> p s.t.
    *  if n represents the integer x
    *  then p represents -x 
    *)
    val neg : int' -> int'

    (* 
    * eq : int' -> int' -> bool
    * REQUIRES: true
    * ENSURES: eq m n |-*-> true if 
    *  m and n represent the same integer,
    *  eq m n |-*-> false otherwise 
    *)
    val eq : int' -> int' -> bool

    (*
    * nat_of_int : nat -> int'
    * REQUIRES: true
    * ENSURES: int'_of_nat m |-*-> n s.t.
    *  n represents the same number as m
    *)
    val int'_of_nat : nat -> int'

  end

  module type RAT = sig 

    (* the type representing rationals *)
    type rat

    (* the type representing the integers that rat is built out of *)
    type int'

    (* 
    * add : rat -> rat -> rat
    * REQUIRES: true
    * ENSURES: add m n |-*-> p s.t.
    *  if m represents the rational x 
    *  and n represents the rational y
    *  then p represents x + y
    *)
    val add : rat -> rat -> rat 

    (* 
    * mul : rat -> rat -> rat
    * REQUIRES: true
    * ENSURES: mul m n |-*-> p s.t.
    *  if m represents the rational x 
    *  and n represents the rational y
    *  then p represents x * y
    *)
    val mul : rat -> rat -> rat

    (* 
    * inv : rat -> rat
    * REQUIRES: n does not represent 0
    * ENSURES: inv n |-*-> p s.t.
    *  if n represents the rational x
    *  then p represents 1/x 
    *)
    val inv : rat -> rat

    (* 
    * eq : rat -> rat -> bool
    * REQUIRES: true
    * ENSURES: eq m n |-*-> true if 
    *  m and n represent the same rational,
    *  eq m n |-*-> false otherwise 
    *)
    val eq : rat -> rat -> bool

    (*
    * nat_of_int : int' -> rat
    * REQUIRES: true
    * ENSURES: rat_of_int' m |-*-> n s.t.
    *  n represents the same number as m
    *)
    val rat_of_int' : int' -> rat

  end

  module AddOps (N : NAT) : OPNAT with type nat = N.nat = struct
    type nat = N.nat

    let zero = N.zero
    let succ = N.succ
    let fold = N.fold

    let rec add x y = N.fold N.succ x y

    let mul x y = N.fold (add x) N.zero y

    let rec eq (x: nat) (y: nat) =
      let map_int z = fold ((+) 1) 0 z in
      map_int x = map_int y 
    let rec nat_of_int z =
      if z = 0 then N.zero else N.succ (nat_of_int (z - 1))

  end


  module Int (N : NAT) : INT with type nat = N.nat = struct
    type nat = N.nat

    module NOps = AddOps(N)
    
    (* Representing integers as Zero, Positive, or Negative *)
    type int' = N.nat * N.nat  (* (p, n) represents p - n *)
    
    
    (* Addition *)
    let add (p1, n1) (p2, n2) =
      let pos = NOps.add p1 p2 in
      let neg = NOps.add n1 n2 in
      (pos, neg)
    
    
    (* Multiplication *)
    let mul (p1, n1) (p2, n2) =
      let pp = NOps.mul p1 p2 in
      let nn = NOps.mul n1 n2 in
      let pn = NOps.mul p1 n2 in
      let np = NOps.mul n1 p2 in
      (NOps.add pp nn, NOps.add pn np)
    
    
    (* Negation *)
    let neg (p, n) = (n, p)

    
    (* Equality Check *)
    let eq (p1, n1) (p2, n2) =
      NOps.eq p1 p2 && NOps.eq n1 n2
    
    
    (* Convert nat to int' *)
    let int'_of_nat n = (n, NOps.zero)

  end


  module Rational (N: NAT) : RAT with type int' = Int(N).int' = struct
    module I = Int(N)

    (* the type representing the integers that rat is built out of *)
    type int' = I.int'

    (* the type representing rationals *)
    type rat = Int(N).int' * Int(N).int'  

    let add x y= match x, y with (p1, q1), (p2, q2) -> (I.add (I.mul p1 q2) (I.mul p2 q1), (I.mul q1 q2))

    let mul x y= match x, y with (p1, q1), (p2, q2) -> (I.mul p1 p2, I.mul q1 q2)

    let inv x = match x with (p1, q1) -> (q1, p1)

    let eq x y= match x, y with (p1, q1), (p2, q2) -> (I.eq p1 p2) && (I.eq q1 q2)

    let rat_of_int' x = (x, I.int'_of_nat (N.succ N.zero))

  end
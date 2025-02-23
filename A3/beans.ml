(* James Bui *)

(* 
* beanCountHelp : (( bool * int) list) * int * int -> int * int
* REQUIRES: true
* ENSURES: beanCountHelp (bs, accTrue, accFalse) |-*-> (j , k ) where j is the sum of the ints that are
* paired with true in lst and k is the sum of the ints that are paired with false
*)
let rec beanCountHelp ((bs, accTrue, accFalse) : (bool * int) list * int * int) : int * int = 
  match bs with
  | [] -> (accTrue, accFalse)
  | (t, n)::bs' ->
    begin
      if t
        then beanCountHelp(bs', accTrue + n, accFalse)
    else beanCountHelp(bs', accTrue , accFalse + n)
    end

(* 
* beanCountTail : ( bool * int) list -> int * int
* REQUIRES: true
* ENSURES: beanCountTail bs |-*-> (j , k ) where j is the sum of the ints that are
* paired with true in lst and k is the sum of the ints that are paired with false
*)
let beanCountTail ( bs : ( bool * int) list ) : int * int = 
  beanCountHelp(bs, 0, 0)
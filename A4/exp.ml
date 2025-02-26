(* James Bui *)

type exp =
 | Var of string
 | Int of int
 | Add of exp * exp
 | Mul of exp * exp
 | Not of exp
 | IfThenElse of exp * exp * exp

type environ = (string * int) list


(* 
* eval : environ * exp-> int
* REQUIRES: Each variable in e has one entry in env
* ENSURES: eval (env,e) |-*-> n, where n is the integer representing the value of e given environment env
*)
let rec eval (env, e): int = 
  match e with
  | Var x -> let value = List.assoc x env in value
  | Int y-> y
  | Add (a,b) -> eval (env, a) + eval (env, b) 
  | Mul (a,b)-> eval (env, a) * eval (env, b) 
  | Not a-> if eval (env, a) = 0 then 1 else 0
  | IfThenElse (a,b,c)-> if eval (env, a) = 0 then eval (env, c) else eval (env, b)


(*  
* backport : exp-> exp
* REQUIRES: true
* ENSURES:
* - For any (env,e) satisfying the REQUIRES of eval,
*                           eval (env,e) ~= eval (env,backport e)
* - backport e does not contain any IfThenElse constructors
*)
let rec backport (e: exp): exp = 
  match e with
  | IfThenElse (a,b,c)-> Add (Mul (Not (Not (backport a)), backport b), Mul (Not (backport a), backport c))
  | Var _ | Int _ -> e 
  | Add (e1, e2) -> Add (backport e1, backport e2)  
  | Mul (e1, e2) -> Mul (backport e1, backport e2)  
  | Not e1 -> Not (backport e1)  


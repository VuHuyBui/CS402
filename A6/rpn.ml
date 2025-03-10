(* James Bui *)

type token =
| Integer of int
| Multiply
| Add
| Subtract
| Divide
| SumAll

exception InvalidExpression

let tokenize ( t : string ) : token =
  match t with
  | "*" -> Multiply
  | "+" -> Add
  | "-" -> Subtract
  | "/" -> Divide
  | "sum" -> SumAll
  | _ -> match int_of_string_opt t with
  | Some n -> Integer n
  | None -> failwith (" invalid token : " ^ t )

let (%) ( f : 'a -> 'b ) ( g : 'c -> 'a ) ( x : 'c ) : 'b = f ( g x )

(*
 * parse : string -> token list
 * REQUIRES: s is a space-separated string of RPN tokens
 * ENSURES: parse s |-*-> an ordered list of the tokens that appear in s 
 *)
 let parse s = 
  let str_list = String.split_on_char ' ' s in
    str_list 
    |> List.filter ((<>) "") 
    |> List.map tokenize


let match_exp acc a = 
  match (acc, a) with
  | (_, Integer x) -> x::acc
  | (acc, SumAll) -> [List.fold_left (+) 0 acc] 
  | (_::[], _) |([], _) -> raise InvalidExpression
  | (x::y::acc', Multiply) -> (x * y)::acc'
  | (x::y::acc', Add) -> (x + y)::acc'
  | (x::y::acc', Subtract) -> (x - y)::acc'
  | (x::y::acc', Divide) -> (x / y)::acc'

(* 
 * eval : token list -> int list
 * REQUIRES: true
 * ENSURES: If tokens is a valid list of tokens, eval tokens |-*-> the resulting stack after
 * evaluating all of the tokens in tokens on an empty stack or it raises Division_by_zero
 * exception if the input's indicated computation would divide by 0. If tokens is not a valid
 * list of tokens, eval tokens raises an InvalidExpression exception.
 *)
let eval tokens = 
  tokens
  |> List.fold_left match_exp []


(*
 * rpn : string -> int list
 * REQUIRES: s is a space-separated string of RPN tokens
 * ENSURES: rpn |-*-> the resulting stack after evaluating RPN expression s or it raises
 * Division_by_zero exception if the input's indicated computation would divide by 0.
 * If s is not a valid RPN expression, rpn s raises an InvalidExpression exception
 *)
let rpn s = 
  s 
  |> parse 
  |> eval

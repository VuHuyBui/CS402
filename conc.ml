let rec conc x x' = 
  match x with
  | [] -> x'
  | a::xs -> a::conc xs x' 

let x1 = [1;2;3]
let x2 = [4;5;6]

let out = conc x1 x2


let rec sumUp x = 
  match x with
  | [] -> 0
  | _::xs -> 1 + sumUp xs
  
let rec rev x = 
  match x with
  | [] -> []
  | x::xs -> rev xs @ [x]


let rec sumHelp (x, acc) = 
  match x with
  | [] -> acc
  | x::xs -> sumHelp(xs, acc + x)

let sumUpT x = 
  sumHelp (x, 0)


let rec filterNil (lst : int list list): int list list = 
  match lst with
  | [] -> []
  | []::xs -> filterNil xs
  | y::ys -> y :: filterNil ys
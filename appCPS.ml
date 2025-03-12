(*
* app : 'a list -> 'a list -> 'a list
* REQUIRES: true
* ENSURES: app a b ~= a @ b
*)
let rec app a b =
  match a with
  | [] -> b
  | x::xs -> x :: (app xs b)
(*
* appCPS : 'a list -> 'a list -> ('a list -> 'b) -> 'b
* REQUIRES: true
* ENSURES: appCPS a b k ~= k (app a b)
*)
let rec appCPS a b k =
  match a with
  | [] -> k b
  | x::xs -> appCPS xs b (fun ret -> k (x :: ret))


(*
* app' : 'a list -> 'a list -> 'a list
* REQUIRES: true
* ENSURES: app' ~= app
*)
let app' a b = appCPS a b (fun x -> x)
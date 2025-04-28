(* James Bui *)

let saved : (int, int) Hashtbl.t = Hashtbl.create 10
let memoize saved x th =
  match Hashtbl.find_opt saved x with
  | Some y -> y
  | None ->
  let res = th () in
  let () = Hashtbl.add saved x res in
  res

(* catalan : int -> int
 * REQUIRES: n >= 0 
 * ENSURES: catalan n |-*-> the nth Catalan number.
 *)
let rec catalan n =  
  memoize saved n 
  begin
    fun () -> match n with
    | 0 -> 1
    | _ -> let res = List.init n (fun i -> i + 1)
    |> List.map (fun x -> catalan (x - 1) * catalan (n - x))
    |> List.fold_left (+) 0
    in res
  end

let saved2 : (int*int, int) Hashtbl.t = Hashtbl.create 10

let rec binom (n, k) = 
  memoize saved2 (n, k)
  begin
    fun () -> 
      if k > n then 0
      else 
        if k = 0 then 1
        else binom (n - 1, k) + binom (n - 1, k - 1)
  end
     
let saved3 : (int, int) Hashtbl.t = Hashtbl.create 10

(* bell : int -> int
 * REQUIRES: n ≥ 0
 * ENSURES: bell n |-*-> the nth Bell number 
 *)
let rec bell n = 
  memoize saved3 n
  begin
    fun () -> match n with
    | 0 -> 1
    | _ -> let res = List.init n (fun i -> i)
    |> List.map (fun x -> binom (n - 1, x) * bell x)
    |> List.fold_left (+) 0
    in res
  end

let counter = ref 0

let tick () = counter := !counter + 1

let read () = !counter

let reset () = counter := 0

(* catalanNaive : int -> int
 * REQUIRES: n >= 0
 * ENSURES: catalanNaive n |-*-> the nth Catalan numb;;er and calls tick once before
 * every recursive call to catalanNaive.
 *)
let rec catalanNaive n = 
  match n with
  | 0 -> 1
  | _ -> 
    let res = List.init n (fun i -> i + 1)
    |> List.map (fun x -> 
      tick(); let left = catalanNaive (x - 1) in
      tick(); let right = catalanNaive (n - x) in
      left * right)
    |> List.fold_left (+) 0
    in res



let rec binomNaive (n, k) = 
  if k > n then 0
  else if k = 0 then 1
  else (
    tick(); let left = binomNaive (n - 1, k) in
    tick(); let right = binomNaive (n - 1, k - 1) in
    left + right
  )

(* bellNaive : int -> int
 * REQUIRES: n >= 0
 * ENSURES: bellNaive n |-*-> the nth Bell number and calls tick once before every
 * recursive call to bellNaive and any call to a function computing binomial coefficients.
 *)

let rec bellNaive n = 
match n with
| 0 -> 1
| _ -> 
  let res = List.init n (fun i -> i)
  |> List.map (fun x -> 
      tick(); let bin = binomNaive (n - 1, x) in
      tick(); let bel = bellNaive x in
      bin * bel)
  |> List.fold_left (+) 0
  in res
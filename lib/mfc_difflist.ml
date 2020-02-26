
type 'a dlist = DList of ('a list -> 'a list)

let dzero = DList(fun xs -> xs)
let dsnoc x = DList(fun xs -> x::xs)
let dappend df dg =
  match (df, dg) with
  | (DList f, DList g) -> DList(fun xs -> f (g xs))
let rec dconst =
  function
  | [] -> dzero
  | x::xs -> dappend (dsnoc x) (dconst xs)

let dmake =
  function
  | DList f -> f []

let (++) a b = dappend a b
let (|-) d x = d ++ dsnoc x
let (|+) a b = a ++ dconst b

let rec range s e = if s < e then (dsnoc s ++ range (s+1) e) else dsnoc e

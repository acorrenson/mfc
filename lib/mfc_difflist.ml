
type 'a dlist = DList of ('a list -> 'a list)

let dzero = DList(fun xs -> xs)
let dsnoc x = DList(fun xs -> x::xs)
let dsnoc_map f x = DList(fun xs -> (f x)::xs)
let dappend df dg =
  match (df, dg) with
  | (DList f, DList g) -> DList(fun xs -> f (g xs))
let rec dconst =
  function
  | [] -> dzero
  | x::xs -> dappend (dsnoc x) (dconst xs)
let rec dconst_map f l =
  match List.rev l with
  | [] -> dzero
  | x::xs -> dappend (dconst_map f xs) (dsnoc_map f x)
let rec dconcat l =
  match List.rev l with
  | [] -> dzero
  | x::xs -> dappend (dconcat xs) x
let rec dconcat_map f l =
  match List.rev l with
  | [] -> dzero
  | x::xs -> dappend (dconcat_map f xs) (f x)

let dmake =
  function
  | DList f -> f []

let (++) a b = dappend a b
let (|-) d x = d ++ dsnoc x
let (|+) a b = a ++ dconst b
let (>+) a l =
  match a with
  | DList f -> f l

let dlist_of_list =
  function
  | [] -> dzero
  | x::xs -> List.fold_left (|-) (dsnoc x) xs

let (|<) = dlist_of_list

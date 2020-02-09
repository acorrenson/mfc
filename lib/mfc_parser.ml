exception ParseException of string

type 'a parse = ('a * string) option

let is_done p =
  match p with
  | Some(_, "") | None -> true
  | Some(_, _) -> false

let option_of_parse p = match p with
  | Some(c, _) -> Some(c)
  | None -> None

let char c input =
  match input with
  | "" -> None
  | x -> match String.get x 0 with
    | x when x == c -> Some(String.make 1 c, String.sub input 1 ((String.length input)-1))
    | _ -> None

let por a b =
  let parser input =
    match a input with
    | Some(_,_) as x -> x
    | None -> b input
  in
  parser

let many p =
  let rec parser ct input =
    match p input with
    | Some(c, rest) -> parser (ct @ [c]) rest
    | None -> match ct with
      | [] -> None
      | x -> Some(x, input)
  in
  parser []

let manystr p =
  let rec parser ct input =
    match p input with
    | Some(c, rest) -> parser (ct^c) rest
    | None -> match ct with
      | "" -> None
      | x -> Some(x, input)
  in
  parser ""

let ignore (p:string -> 'a parse) (next:string -> 'b parse) input =
  match p input with
  | Some(_, rest) -> next rest
  | None -> next input
let skip p next (input:string) =
  match p input with
  | Some(_, rest) -> next rest
  | None -> None

let seqstr ps =
  let rec parser ps ct input =
    match ps with
    | p::ps -> (match p input with
        | Some(c, rest) -> parser ps (ct ^ c) rest
        | None -> match ct with
          | "" -> None
          | x -> Some(x, input))
    | [] -> (match ct with
        | "" -> None
        | x -> Some(x, input))
  in
  parser ps ""

let anychar_in s =
  let rec parser ct n input =
    if n == (String.length s) then
      match ct with
      | "" -> None
      | _ -> Some(ct, input)
    else match char (s.[n]) input with
      | Some(c, rest) -> Some(c, rest)
      | None -> parser ct (n+1) input
  in
  parser "" 0

let explode s =
  let rec step s l i =
    if i == String.length s then l
    else step s (l @ [s.[i]]) (i+1)
  in
  step s [] 0

let rec combine_seq ps input =
  match ps with
  | [] -> Some([], input)
  | v::vs -> match v input with
    | None -> None
    | Some(c, rest) -> match combine_seq vs rest with
      | Some(cs, rest) -> Some(c::cs, rest)
      | None -> None

let delim d p input =
  match p input with
  | None -> None
  | Some(c, rest) -> match skip d p rest with
    | Some(d, rest) -> Some((c,d), rest)
    | None -> None

let wrap l r p input =
  match l input with
  | None -> None
  | Some(_, rest) -> match p rest with
    | None -> None
    | Some(c, rest) -> match r rest with
      | None -> None
      | Some(_, rest) -> Some(c, rest)

(* Parse digit *)
let pdigit = anychar_in "0123456789"

(* Parse nat *)
let pnat input =
  input
  |> manystr pdigit
  |> Option.map (fun (s,r) -> int_of_string s, r)

(* Parse int *)
let pint input =
  match char '-' input with
  | Some(_, rest) -> pnat rest |> Option.map (fun (i,r) -> (-i, r))
  | None -> pnat input

(* Parse float *)
let pfloat input =
  input
  |> seqstr [manystr pdigit; char '.'; manystr pdigit]
  |> Option.map (fun (s,r) -> float_of_string s, r)

type expr =
  | Plus of expr * expr
  | Mult of expr * expr
  | Value of float

let make_plus ((l,r): expr*expr) = Plus (l, r)
let make_mult ((l,r): expr*expr) = Mult (l,r)
let make_value v = Value v
let tuple2_of_list l = (List.nth l 0, List.nth l 1)
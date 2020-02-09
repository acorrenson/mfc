exception ParseException of string

type 'a parse = ('a * string) option

let is_done p =
  match p with
  | Some(_, "") | None -> true
  | Some(_, _) -> false

let option_of_parse p = match p with
  | Some(c, _) -> Some(c)
  | None -> None

let parse_char c input =
  match input with
  | "" -> None
  | x -> match String.get x 0 with
    | x when x == c -> Some(String.make 1 c, String.sub input 1 ((String.length input)-1))
    | _ -> None

let parse_or a b =
  let parser input =
    match a input with
    | Some(_,_) as x -> x
    | None -> b input
  in
  parser

let pmap f = Option.map(fun (v,rest) -> (f v, rest))

let pand f p =
  match p with
  | Some(c, rest) -> f c rest
  | None -> None

let parse_many p =
  let rec parser ct input =
    match p input with
    | Some(c, rest) -> parser (ct @ [c]) rest
    | None -> match ct with
      | [] -> None
      | x -> Some(x, input)
  in
  parser []

let parse_concat_many p =
  let rec parser ct input =
    match p input with
    | Some(c, rest) -> parser (ct^c) rest
    | None -> match ct with
      | "" -> None
      | x -> Some(x, input)
  in
  parser ""

let parse_ignore (p:string -> 'a parse) (next:string -> 'b parse) input =
  match p input with
  | Some(_, rest) -> next rest
  | None -> next input
let parse_skip p next (input:string) =
  match p input with
  | Some(_, rest) -> next rest
  | None -> None

let parse_concat_seq ps =
  let rec parser ps ct input =
    match ps with
    | p::ps ->
      begin
        match p input with
        | Some(c, rest) -> parser ps (ct ^ c) rest
        | None -> match ct with
          | "" -> None
          | x -> Some(x, input)
      end
    | [] ->
      begin
        match ct with
        | "" -> None
        | x -> Some(x, input)
      end
  in
  parser ps ""

let parse_anychar_in s =
  let rec parser ct n input =
    if n == (String.length s) then
      match ct with
      | "" -> None
      | _ -> Some(ct, input)
    else match parse_char (s.[n]) input with
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

let rec parse_combine_seq ps input =
  match ps with
  | [] -> Some([], input)
  | v::vs ->
    match v input with
    | None -> None
    | Some(c, rest) ->
      match parse_combine_seq vs rest with
      | Some(cs, rest) -> Some(c::cs, rest)
      | None -> None

let parse_delim d p input =
  match p input with
  | None -> None
  | Some(c, rest) -> match parse_skip d p rest with
    | Some(d, rest) -> Some((c,d), rest)
    | None -> None

let parse_wrap l r p input =
  match l input with
  | None -> None
  | Some(_, rest) -> match p rest with
    | None -> None
    | Some(c, rest) -> match r rest with
      | None -> None
      | Some(_, rest) -> Some(c, rest)

(* Parse digit *)
let pdigit = parse_anychar_in "0123456789"

(* Parse nat *)
let pnat input =
  input
  |> parse_concat_many pdigit
  |> pmap int_of_string

(* Parse int *)
let pint input =
  match parse_char '-' input with
  | Some(_, rest) -> pnat rest |> pmap (fun i -> -i)
  | None -> pnat input

(* Parse float *)
let pfloat input =
  let positive input =
    input
    |> parse_concat_seq [parse_concat_many pdigit; parse_char '.'; parse_concat_many pdigit]
    |> pmap float_of_string
  in match parse_char '-' input with
  | Some(_, rest) -> pmap (fun i -> -. i) (positive rest)
  | None -> positive input

(* Example parser *)
type expr =
  | Plus of expr * expr
  | Mult of expr * expr
  | Value of float

let make_plus ((l,r): expr*expr) = Plus (l, r)
let make_mult ((l,r): expr*expr) = Mult (l,r)
let make_value v = Value v
let tuple2_of_list l = (List.nth l 0, List.nth l 1)
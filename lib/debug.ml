module List =
    struct
        include List

        let rec make n v = match n with
        | 0 -> []
        | 1 -> [v]
        | _ -> v::(make (n-1) v)
        let first l = List.nth l 0
        let flat_first l = l |> List.flatten |> (fun l -> List.nth l 0)
    end

open Mfc.Mfc_parsing


type expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | Val of float

let _digit = anychar_in "0123456789"
let _floatpart = many _digit ||> String.combine
let _nat = many _digit ||> String.combine ||> int_of_string
let _int =
    P begin
        fun inp -> match parse (pchar '-') inp with
        | Some(_, rest) -> parse (_nat ||> (-) 0) rest
        | None -> parse _nat inp
    end
let _float =
    let _float_full = begin
        let* i = _int in
        let* d = pchar '.' in
        let* f = _floatpart in
        P (
            fun inp -> Some((i,(String.make 1 d)^f), inp))
            ||> begin
                fun (i,f) -> float_of_int i
            |> (+.) (float_of_string f)
        end
    end in
    _float_full <|> (_int ||> float_of_int)
let _array_of v = [v]
let _wrap l r p =
  (*sequence [ignore l; p; ignore r] ||> List.flat_first*)
    let* _ = l in
    let* e = p in
    let* _ = r in
    P(fun inp -> Some(e, inp))

let _infix a i b =
  let* x = a in
  let* _ = literal i in
  let* y = b in
  P(fun inp -> Some((x,y), inp))

let make_value v = Val v
let make_add (l,r) = Add (l,r)
let make_sub (l,r) = Sub (l,r)
let make_mult (l,r) = Mult (l,r)
let make_div (l,r) = Div (l,r)
let make_pow (l,r) = Pow (l,r)

let _value = _float ||> make_value
let rec _expr inp =
  parse ( _infix (parser _sub) "+" (parser _expr) ||> make_add <|> parser _sub  ) inp
and _sub inp = parse (_infix (parser _term) "-" (parser _expr) ||> make_sub <|> parser _term) inp
and _term inp =
  parse (_infix (parser _div) "*" (parser _term) ||> make_mult <|> parser _div) inp
and _div inp =
  parse (_infix (parser _pow) "/" (parser _term) ||> make_div <|> parser _pow) inp
and _pow inp =
    parse (_infix (parser _atom) "**" (parser _pow) ||> make_pow <|> parser _atom) inp
and _atom inp =
  parse (_wrap (pchar '(') (pchar ')') (parser _expr) <|> _value) inp

let rec eval =
  function
  | Val v -> v
  | Add (l,r) -> (eval l) +. (eval r)
  | Sub (l,r) -> (eval l) -. (eval r)
  | Mult (l,r) -> (eval l) *. (eval r)
  | Div (l,r) -> (eval l) /. (eval r)
  | Pow (l,r) -> Float.pow (eval l) (eval r)

let rec pprint =
    function
    | Val v -> string_of_float v
    | Add (l,r) -> Printf.sprintf "(%s + %s)" (pprint l) (pprint r)
    | Sub (l,r) -> Printf.sprintf "(%s - %s)" (pprint l) (pprint r)
    | Mult (l,r) -> Printf.sprintf "(%s * %s)" (pprint l) (pprint r)
    | Div (l,r) -> Printf.sprintf "(%s / %s)" (pprint l) (pprint r)
    | Pow (l,r) -> Printf.sprintf "(%s ** %s)" (pprint l) (pprint r)

let expr_eval = parser _expr ||> eval

let str_remove s input =
    Str.global_replace (Str.regexp_string s) "" input

let _ = print_string "Input computation: ";
    let e = _expr (str_remove " " (read_line())) in
    pleak e
    |> pprint
    |> print_endline;
    pleak e
    |> eval
    |> string_of_float
    |> print_endline



open Mfc.Mfc_parsing

(* Parse digit *)
let pdigit = parse_anychar_in "0123456789"

(* Parse nat *)
let pnat input = input |> parse_concat_many pdigit |> pmap int_of_string

(* Example parser *)

(* Parse int *)
let pint input =
  match (parse_char '-' input) with
  | Some (_, rest) -> pnat rest |> pmap (fun i -> -i)
  | None -> pnat input

(* Parse float *)
let pfloat input =
  let positive input =
    input
    |> parse_concat_seq
      [ parse_concat_many pdigit; parse_char '.'; parse_concat_many pdigit ]
    |> pmap float_of_string
  in
  match parse_char '-' input with
  | Some (_, rest) -> pmap (fun i -> -.i) (positive rest)
  | None -> positive input

type expr = Plus of expr * expr | Mult of expr * expr | Value of int

let rec print_expr e =
  match e with
  | Plus (e1, e2) ->
    print_string "Plus (";
    print_expr e1;
    print_string ", ";
    print_expr e2;
    print_string ")"
  | Mult (e1, e2) ->
    print_string "Mult (";
    print_expr e1;
    print_string ", ";
    print_expr e2;
    print_string ")"
  | Value i -> print_int i

let tuple2_of_list l = (List.nth l 0, List.nth l 1)

let make_plus l = l |> tuple2_of_list |> fun (l, r) -> Plus (l, r)

let make_mult l = l |> tuple2_of_list |> fun (l, r) -> Mult (l, r)

let make_value v = Value v

(*
Grammar:
-- expr     ::= term "+" expr | term | sym | call
-- term     ::= factor "*" term | factor
-- factor   ::= "(" expr ")" | int
-- int      ::= ('0'..'9')+
-- ifexpr   ::= "if" "(" condexpr ")" block "else" block
-- block    ::= "{" stmt* "}"
-- stmt     ::= call | assign
-- call     ::= sym "(" args ")" | sym "(" ")"
-- args     ::= expr | expr "," args
-- defun    ::= "fun" sym "(" params ")" block | "fun" sym "(" ")" block
-- params   ::= sym | sym "," sym
-- assign   ::= sym "=" expr
-- prog     ::= extern | defun*
-- sym      ::= ('a'..'z' | 'A'..'Z')+ ('0'..'9')* ('a'..'z' | 'A'..'Z')*
?? extern   ::= "extern" "fun" "(" params ")" | "extern" "fun" "(" ")"

TODO :
  on peut séparer les procédure des fonctions ! (et donc gagner en collecte d'informations dés le parsing)
*)

let rec factor input = input |> parse_or parse_expr_parent parse_int

and parse_int input = input |> pint |> pmap make_value

and parse_expr_parent input =
  parse_wrap (parse_char '(') (parse_char ')') parse_expr input

and parse_expr input = input |> parse_or term_expr term

and term input = input |> parse_or factor_term factor

and factor_term input =
  input
  |> parse_combine_seq [ factor; parse_skip (parse_char '*') term ]
  |> pmap make_mult

and term_expr input =
  input
  |> parse_combine_seq [ term; parse_skip (parse_char '+') parse_expr ]
  |> pmap make_plus

and parse_ifexpr input =
  input
  |> parse_skip (parse_literal "if")
    (parse_combine_seq
       [
         parse_expr_parent;
         parse_block;
         parse_skip (parse_literal "else") parse_block;
       ])

and (parse_block: stmt t) input =
  input
  |> parse_wrap (parse_char '{') (parse_char '}') parse_sequence

and parse_sequence input = input |> parse_many (parse_stmt)
and parse_stmt input = parse_or parse_call parse_assign input
and parse_call input =
  input
  |> parse_or
    (parse_all [parse_sym; parse_wrap (parse_char '(') (parse_char ')') parse_args])
    (parse_all [parse_sym; parse_char '('; parse_char ')'])
and parse_args input =
  input
  |> parse_or
    (parse_combine_seq [parse_expr; skip (parse_char ',') parse_args])
    (parse_expr)
(* and parse_defun input =
   let inner input =
    input
    |> parse_or
      (parse_wrap (parse_char '(') (parse_char ')') parse_params)
      (parse_all [parse_char '('; parse_char ')'])
    |> pmap (fun )
      input
    |> parse_or
      (parse_all [skip (parse_literal "fun") parse_sym; parse_wrap (parse_char '(') (parse_char ')') parse_params; parse_block])
      (parse_all [])
*)
let _ =
  parse_expr "3+2*4" |> Option.get |> function
  | _, s when s <> "" -> failwith ("parse error " ^ s)
  | x, _ -> print_expr x

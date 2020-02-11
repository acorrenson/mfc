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
  | Mult of expr * expr
  | Val of int

let _digit = anychar_in "0123456789"
let _nat = many _digit ||> String.combine ||> (fun i -> Val (int_of_string i))
let _array_of v = [v]
let _wrap l r p =
  sequence [ignore l; p; ignore r] ||> List.flat_first

let _infix a i b =
  let* x = a in
  let* _ = literal i in
  let* y = b in
  P(fun inp -> Some((x,y), inp))

let make_add (l,r) = Add (l,r)
let make_mult (l,r) = Mult (l,r)

let rec _expr inp =
  parse ( _infix (parser _term) "+" (parser _expr) ||> make_add <|> parser _term  ) inp
and _term inp =
  parse (_infix (parser _factor) "*" (parser _term) ||> make_mult <|> parser _factor) inp
and _factor inp =
  parse (_wrap (pchar '(') (pchar ')') (parser _expr ||> List.make 1) <|> _nat) inp

let rec eval =
  function
  | Val v -> v
  | Add (l,r) -> (eval l) + (eval r)
  | Mult (l,r) -> (eval l) * (eval r)

let expr_eval = parser _expr ||> eval

let _ = print_string "Input computation: "; parse expr_eval (read_line()) |> pmap string_of_int |> pmap print_endline

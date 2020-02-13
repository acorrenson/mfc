open Mfc_parsing
open Mfc_ast

let _sym = many (anychar_in "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ") |> fmap combine
let _digit = anychar_in "0123456789"
let _nat =
  many _digit
  |> fmap (fun lc -> List.fold_left (^) "" (List.map (String.make 1) lc))
  |> fmap (fun i -> Cst (int_of_string i))


let rec _expr inp =
  parse (
    (
      let* n1 = parser _term |> trim in
      let* _ = pchar '+' |> trim in
      let* n2 = parser _expr in
      P (fun inp -> Some (Binop (Add, n1, n2), inp))
    ) <|> parser _term
  ) inp
and _term inp =
  parse (
    (
      let* f = parser _factor |> trim in
      let* _ = pchar '*' in
      let* t = parser _term |> trim in
      P (fun inp -> Some (Binop (Mult, f, t), inp))
    ) <|> parser _factor
  ) inp
and _factor inp =
  parse (
    (
      let* _ = pchar '(' |> trim in
      let* e = parser _expr in
      let* _ = pchar ')' |> trim in
      P (fun inp -> Some (e, inp))
    ) <|> _nat
  ) inp

let _comp_c c =
  let* l = parser _expr |> trim in
  let* _ = literal (cstr c) |> trim in
  let* r = parser _expr |> trim in
  P (fun inp -> Some (Cmp (c, l, r), inp))

let _comp =
  _comp_c Lt
  <|> _comp_c Le
  <|> _comp_c Gt
  <|> _comp_c Ge
  <|> _comp_c Ne
  <|> _comp_c Eq

let rec _cond inp =
  parse (
    (
      let* c1 = parser _cterm in
      let* _ = literal "or" |> trim in
      let* c2 = parser _cond in
      P (fun inp -> Some (Or (c1, c2), inp))
    )
    <|> parser _cterm
  ) inp
and _cterm inp =
  parse (
    (
      let* c1 = parser _cfactor in
      let* _ = literal "and" |> trim in
      let* c2 = parser _cterm in
      P (fun inp -> Some (And (c1, c2), inp))
    )
    <|> parser _cfactor
  ) inp
and _cfactor inp =
  parse (
    (
      let* _ = literal "not" |> trim in
      let* c2 = parser _cfactor in
      P (fun inp -> Some (Not (c2), inp))
    )
    <|> 
    (
      let* _ = pchar '(' |> trim in
      let* c = parser _cond in
      let* _ = pchar ')' |> trim in
      P (fun inp -> Some (c, inp))
    )
    <|> _comp
  ) inp

let _arglist =
  some (
    let* e = parser _expr |> trim in
    let* _ = pchar ',' in
    P (fun inp -> Some (e, inp))
  )

let _args =
  (
    let* a = _arglist in
    let* e = parser _expr |> trim in
    P (fun inp -> Some (a @ [e], inp))
  )
  <|>
  (optional (parser _expr))


let rec _stmt inp =
  parse (
    (
      let* _ = literal "if" |> trim in
      let* _ = (pchar '(') |> trim in
      let* c = parser _cond in
      let* _ = pchar ')' |> trim in
      let* _ = pchar '{' |> trim in
      let* s1 = parser _stmt in
      let* _ = pchar '}' |> trim in
      let* _ = literal "else" in
      let* _ = pchar '{' |> trim in
      let* s2 = parser _stmt in
      let* _ = pchar '}' |> trim in
      P (fun inp -> Some (If (c, s1, s2), inp))
    )
    <|>
    (
      let* _ = literal "var" |> trim in
      let* s = _sym in
      P (fun inp -> Some (Declare s, inp))
    )
    <|>
    (
      let* s = _sym |> trim in
      let* _ = pchar '=' in
      let* e = parser _expr |> trim in
      P (fun inp -> Some (Set(Id s, e), inp))
    )
    <|>
    (
      let* fn = _sym |> trim in
      let* _ = pchar '(' |> trim in
      let* a = _args in
      let* _ = pchar ')' |> trim in
      P (fun inp -> Some (Call (Id fn, a), inp))
    )
    <|>
    (
      let* _ = literal "while" |> trim in
      let* _ = pchar '(' |> trim in
      let* c = parser _cond in
      let* _ = pchar ')' |> trim in
      let* _ = pchar '{' |> trim in
      let* s = parser _stmt |> trim in
      let* _ = pchar '}' |> trim in
      P (fun inp -> Some (While (c, s), inp))
    )
  ) inp
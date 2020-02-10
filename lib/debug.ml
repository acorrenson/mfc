open Mfc.Mfc_parsing

type expr =
  | Add of expr * expr
  | Mult of expr * expr
  | Val of int

let _digit = anychar_in "0123456789"
let _nat =
  many _digit
  |> fmap (fun lc -> List.fold_left (^) "" (List.map (String.make 1) lc))
  |> fmap (fun i -> Val (int_of_string i))


let rec _expr inp =
  parse (
    (
      let* n1 = parser _term in
      let* _ = some (pchar ' ') in
      let* _ = pchar '+' in
      let* _ = some (pchar ' ') in
      let* n2 = parser _expr in
      P (fun inp -> Some (Add (n1, n2), inp))
    ) <|> parser _term
  ) inp
and _term inp =
  parse (
    (
      let* f = parser _factor in
      let* _ = some (pchar ' ') in
      let* _ = pchar '*' in
      let* _ = some (pchar ' ') in
      let* t = parser _term in
      P (fun inp -> Some (Mult (f, t), inp))
    ) <|> parser _factor
  ) inp
and _factor inp =
  parse (
    (
      let* _ = some (pchar ' ') in
      let* _ = pchar '(' in
      let* _ = some (pchar ' ') in
      let* e = parser _expr in
      let* _ = some (pchar ' ') in
      let* _ = pchar ')' in
      let* _ = some (pchar ' ') in
      P (fun inp -> Some (e, inp))
    ) <|> _nat
  ) inp

let rec eval =
  function
  | Val v -> v
  | Add (l,r) -> (eval l) + (eval r)
  | Mult (l,r) -> (eval l) * (eval r)

let expr_eval = parser _expr |> fmap eval

let _ = _expr "2 + 3 * (4 * 5)"

type cond = Eq of expr * expr | Ne of expr * expr
type stmt = If of cond * stmt * stmt | Do of string

let _cond_eq =
  let* l = parser _expr in
  let* _ = some (pchar ' ') in
  let* _ = literal "==" in
  let* _ = some (pchar ' ') in
  let* r = parser _expr in
  P (fun inp -> Some (Eq (l, r), inp))

let trim p =
  let* _ = some (pchar ' ' <|> pchar '\n') in
  let* e = p in
  let* _ = some (pchar ' ' <|> pchar '\n') in
  P (fun inp -> Some(e, inp))

let _cond_ne =
  let* l = parser _expr |> trim in
  let* _ = literal "!=" |> trim in
  let* r = parser _expr |> trim in
  P (fun inp -> Some (Ne (l, r), inp))

let rec _stmt inp =
  parse (
    (
      let* _ = literal "if" |> trim in
      let* _ = (pchar '(') |> trim in
      let* c = _cond_eq <|> _cond_ne in
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
      let* _ = literal "do" |> trim in
      let* s = many (anychar_in "abcdefghijklmnopqrstuvwxyz") |> fmap combine in
      P (fun inp -> Some (Do s, inp))
    )
  ) inp




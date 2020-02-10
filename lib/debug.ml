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
      let* _ = pchar '+' in
      let* n2 = parser _expr in
      P (fun inp -> Some (Add (n1, n2), inp))
    ) <|> parser _term
  ) inp
and _term inp =
  parse (
    (
      let* f = parser _factor in
      let* _ = pchar '*' in
      let* t = parser _term in
      P (fun inp -> Some (Mult (f, t), inp))
    ) <|> parser _factor
  ) inp
and _factor inp =
  parse (
    (
      let* _ = pchar '(' in
      let* e = parser _expr in
      let* _ = pchar ')' in
      P (fun inp -> Some (e, inp))
    ) <|> _nat
  ) inp

let rec eval =
  function
  | Val v -> v
  | Add (l,r) -> (eval l) + (eval r)
  | Mult (l,r) -> (eval l) * (eval r)

let expr_eval = parser _expr |> fmap eval

let _ = parse expr_eval "2+3*(4*5)"

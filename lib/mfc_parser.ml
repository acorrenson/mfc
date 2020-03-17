(**************************************************************************)
(*                                                                        *)
(*                      This file is part of MFC                          *)
(*                  it is released under MIT license.                     *)
(*                https://opensource.org/licenses/MIT                     *)
(*                                                                        *)
(*          Copyright (c) 2020 Arthur Correnson, Nathan Graule            *)
(**************************************************************************)

open Libnacc.Parsers
open Mfc_ast

open StringParser

let _sym = String.join <$> some (one_in "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ")
let _digit = one_in "0123456789"
let _nat = integer
let spaced p = trim [' ';'\t';'\n'] p


let rec _expr inp =
  let add a b = Binop(Add, a, b) in
  let sub a b = Binop(Sub, a, b) in
  inp --> (
    ebinop2 add '+' ~~_term ~~_expr
    <|>
    ebinop2 sub '-' ~~_term ~~_expr
    <|> ~~_term
  )
and _term inp =
  let mul a b = Binop(Mult, a, b) in
  inp --> (
    ebinop2 mul '*' ~~_factor ~~_term <|> ~~_factor
  )
and _factor inp =
  let idref x = Ref(Id x) in
  let astval a = Cst a in
  inp --> (
    eparenthesized '(' ~~_expr ')' <|> (astval <$> _nat) <|> (idref <$> spaced _sym)
  )

let _comp_c c =
  let astcmp l r = Cmp (c,l,r) in
  binop astcmp (csym c |> literal) (spaced ~~_expr)
let _comp =
  _comp_c Lt
  <|> _comp_c Le
  <|> _comp_c Gt
  <|> _comp_c Ge
  <|> _comp_c Ne
  <|> _comp_c Eq

let rec _cond inp =
  let astor l r = Or (l,r) in
  inp --> (binop2 astor (literal "or" |> spaced) ~~_cterm ~~_cond
           <|> ~~_cterm)
and _cterm inp =
  let astand l r = And (l,r) in
  inp --> (binop2 astand (literal "and" |> spaced) ~~_cfactor ~~_cterm <|> ~~_cfactor)
and _cfactor inp =
  let astnot c = Not c in
  inp --> (astnot <$> (literal "not" |> spaced) *> ~~_cfactor <|> eparenthesized '(' ~~_cond ')')

let _arglist =
  let inner = (~~_expr |> spaced) <* elem ',' in
  some inner
let _args =
  let append a e = a @ [e] in
  append <$> _arglist <*> (~~_expr |> spaced)
  <|>
  (optional (~~_expr))


let rec _stmt inp: s_ast state =
  inp --> begin
    let astif c i e = If(c, i, e) in
    astif
    <$> (literal "if" |> spaced) *> eparenthesized '(' (~~_cond |> spaced) ')'
    <*> eparenthesized '{' ~~_stmt '}'
    <*> (literal "else" |> spaced) *> eparenthesized '{' ~~_stmt '}'
    <|>
    let astdeclare v = Declare v in
    astdeclare <$> (literal "var" |> spaced) *> _sym
    <|>
    let astassign i v = Set(Id i, v) in
    astassign <$> spaced _sym <*> spaced (elem '=') *> (~~_expr |> spaced)
    <|>
    let astcall f a = Call(Id f, a) in
    astcall <$> spaced _sym <*> eparenthesized '(' _args ')'
    <|>
    let astwhile c s = While(c, Block s) in
    astwhile <$> (literal "while" |> spaced) *> eparenthesized '(' ~~_cond ')' <*> eparenthesized '{' (many ~~_stmt) '}'
  end

(* let _prog =
   let* def = many (parser _stmt) in
   P (fun inp -> Some (Block (def), inp)) *)

let _prog =
  let astblock b = Block b in
  astblock <$> some ~~_stmt

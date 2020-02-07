
type binop =
  | Add
  | Sub

type compare =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Ne

type unop = Neg

type ctype =
  | T_int
  | T_pint
  | T_fun of ctype * ctype list

type r_ast =
  | Id of string

type e_ast =
  | Cst of int
  | Ref of r_ast
  | Ecall of r_ast * e_ast list
  | Unop of unop * e_ast
  | Binop of binop * e_ast * e_ast

type c_ast =
  | And of  c_ast * c_ast
  | Or of c_ast * c_ast
  | Not of c_ast
  | Cmp of compare * e_ast * e_ast

type s_ast =
  | Set of r_ast * e_ast
  | Seq of s_ast * s_ast
  | If of c_ast * s_ast * s_ast
  | While of c_ast * s_ast
  | Call of r_ast * e_ast list
  | Ret of e_ast
  | Declare of string * ctype
  | Block of s_ast

let rec str_of_type t =
  match t with
  | T_int -> "int"
  | T_pint -> "pint"
  | T_fun (e, l) ->
    (List.fold_left (fun a s -> a ^ s ^ " -> " ) "" (List.map str_of_type l))
    ^ (str_of_type e)
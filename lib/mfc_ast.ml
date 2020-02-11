type binop = Add | Sub | Mult

type compare = Ge | Gt | Le | Lt | Eq | Ne

type unop = Neg

type r_ast = Id of string

type e_ast =
  | Cst of int
  | Ref of r_ast
  | Ecall of r_ast * e_ast list
  | Unop of unop * e_ast
  | Binop of binop * e_ast * e_ast

type c_ast =
  | And of c_ast * c_ast
  | Or of c_ast * c_ast
  | Not of c_ast
  | Cmp of compare * e_ast * e_ast

type s_ast =
  | Set of r_ast * e_ast
  | If of c_ast * s_ast * s_ast
  | While of c_ast * s_ast
  | Call of r_ast * e_ast list
  | Ret of e_ast
  | Declare of string
  | DeclareFun of string * int * int
  | Block of s_ast list

let cstr c =
  match c with
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Ne -> "!="
  | Eq -> "=="
(**************************************************************************)
(*                                                                        *)
(*                      This file is part of MFC                          *)
(*                  it is released under MIT license.                     *)
(*                https://opensource.org/licenses/MIT                     *)
(*                                                                        *)
(*          Copyright (c) 2020 Arthur Correnson, Nathan Graule            *)
(**************************************************************************)

(** Binary operator *)
type binop = Add | Sub | Mult

(** Integer comparison *)
type compare = Ge | Gt | Le | Lt | Eq | Ne

(** Boolean negation *)
type unop = Neg

(** Reference AST *)
type r_ast = Id of string

(** Expression AST *)
type e_ast =
  | Cst of int
  | Ref of r_ast
  | Ecall of r_ast * e_ast list
  | Unop of unop * e_ast
  | Binop of binop * e_ast * e_ast

(** Test/condition AST *)
type c_ast =
  | And of c_ast * c_ast
  | Or of c_ast * c_ast
  | Not of c_ast
  | Cmp of compare * e_ast * e_ast

(** Statement AST *)
type s_ast =
  | Set of r_ast * e_ast
  | If of c_ast * s_ast * s_ast
  | While of c_ast * s_ast
  | Call of r_ast * e_ast list
  | Ret of e_ast
  | Declare of string
  | DeclareFun of string * int * int
  | Block of s_ast list

(** Comparison to math symbol conversion *)
let csym c =
  match c with
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Ne -> "!="
  | Eq -> "=="

(** Comparison to string conversion *)
let cstr c =
  match c with
  | Lt -> "lt"
  | Le -> "le"
  | Gt -> "gt"
  | Ge -> "ge"
  | Ne -> "ne"
  | Eq -> "eq"

(** Binop to string conversion *)
let bstr b =
  match b with
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "mul"
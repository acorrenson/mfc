(**************************************************************************)
(*                                                                        *)
(*                      This file is part of MFC                          *)
(*                  it is released under MIT license.                     *)
(*                https://opensource.org/licenses/MIT                     *)
(*                                                                        *)
(*          Copyright (c) 2020 Arthur Correnson, Nathan Graule            *)
(**************************************************************************)

open Mfc_ast

exception CallError of string

let check_function_call s =
  let rec get_functions_type s t =
    match s with
    | Block ls ->
      List.fold_left (fun t s -> t @ (get_functions_type s t)) [] ls
    | DeclareFun (id, ret, args) -> (id, (ret, args)) :: t
    | While (_, s) -> get_functions_type s t
    | If (_, s1, s2) -> get_functions_type s1 t @ get_functions_type s2 t
    | _ -> []
  in
  let rec check s t =
    match s with
    | Call (Id x, args) -> (
        match List.assoc_opt x t with
        | None -> raise (CallError ("Unknow function " ^ x))
        | Some (0, n) when n = List.length args -> ()
        | Some (_, n) ->
          let m = Printf.sprintf "function %s expect %d arguments" x n in
          raise (CallError m) )
    | Block ls -> List.iter (fun s -> check s t) ls
    | Set (_, Ecall (Id x, args)) -> (
        match List.assoc_opt x t with
        | None -> raise (CallError ("Unknow function " ^ x))
        | Some (0, _) ->
          raise (CallError "Results of a procedure cant be assigned")
        | Some (_, n) when n <> List.length args ->
          let m = Printf.sprintf "function %s expect %d arguments" x n in
          raise (CallError m)
        | Some (_, _) -> () )
    | Ret (Ecall (Id x, args)) -> (
        match List.assoc_opt x t with
        | None -> raise (CallError ("Unknow function " ^ x))
        | Some (0, _) ->
          raise (CallError "Result of a procedure cant be assigned")
        | Some (_, n) when n <> List.length args ->
          let m = Printf.sprintf "function %s expect %d arguments" x n in
          raise (CallError m)
        | Some (_, _) -> () )
    | _ -> ()
  in
  let types = get_functions_type s [] in
  check s types

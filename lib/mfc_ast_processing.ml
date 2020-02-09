open Mfc_ast

exception CallError of string

let check_function_call s =
  let rec get_functions_type s t =
    match s with
    | Block b ->
      get_functions_type b t
    | Seq (s1, s2) ->
      (get_functions_type s1 t) @ (get_functions_type s2 t)
    | DeclareFun (id, ret, args) ->
      (id, (ret, args))::t
    | While (_, s) ->
      get_functions_type s t
    | If (_, s1, s2) ->
      (get_functions_type s1 t) @ (get_functions_type s2 t)
    | _ -> []
  in
  let rec check s t =
    match s with
    | Call (Id x, args) ->
      begin
        match List.assoc_opt x t with
        | None -> raise (CallError ("Unknow function " ^ x))
        | Some (0, n) when n = List.length args -> ()
        | Some (_, n) ->
          let m = Printf.sprintf "function %s expect %d arguments" x n in
          raise (CallError m)
      end
    | Seq (s1, s2) ->
      check s1 t;
      check s2 t
    | Block s ->
      check s t
    | Set (_, Ecall (Id x, args)) ->
      begin
        match List.assoc_opt x t with
        | None -> raise (CallError ("Unknow function " ^ x))
        | Some (0, _) -> raise (CallError "Results of a procedure cant be assigned")
        | Some (_, n) when n <> List.length args ->
          let m = Printf.sprintf "function %s expect %d arguments" x n in
          raise (CallError m)
        | Some (_, _) -> ()
      end
    | Ret (Ecall (Id x, args)) ->
      begin
        match List.assoc_opt x t with
        | None -> raise (CallError ("Unknow function " ^ x))
        | Some (0, _) -> raise (CallError "Results of a procedure cant be assigned")
        | Some (_, n) when n <> List.length args ->
          let m = Printf.sprintf "function %s expect %d arguments" x n in
          raise (CallError m)
        | Some (_, _) -> ()
      end
    | _ -> ()
  in
  let types = get_functions_type s [] in
  check s types
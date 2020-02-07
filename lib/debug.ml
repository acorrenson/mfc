open Mfc.Mfc_env
open Mfc.Mfc_ast

let _ =
  let e = new_env () in
  push_frame e;
  new_local e "z" T_int;
  new_local e "g" T_int;
  match lookup_opt_offset e "g" with
  | None -> print_endline "error"
  | Some i -> print_int i |> print_newline;
    str_of_type (T_fun (T_int, [T_int; T_int]))
    |> print_endline
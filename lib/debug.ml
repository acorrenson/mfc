
open Mfc
open Mfc_generate
open Mfc_parsing
open Mfc_parser
open Mfc_env
open Mfc_quad
open Mfc_reg_alloc

let _ =
  let ic = open_in "examples/test.gen" in
  generate ic;
  close_in ic;
  let ic2 = open_in "examples/test.gen" in
  let r = read_all ic2 in
  match parse _prog r with
  | Some (ast, "") ->
    let env = new_env () in
    push_frame env;
    new_function env "print" 0 1;
    let ql = quad_s ast env in
    let rc = env.label_counter+1 in
    let arr = get_lifes ql rc in
    let p = fun s -> IntSet.iter (fun i -> print_int i; print_char ' ') s; print_newline () in
    Array.iter p arr;
    inter_mat arr
  | _ -> failwith "parse error"

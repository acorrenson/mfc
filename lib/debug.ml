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
    let rc = env.tmp_counter+1 in
    get_lifes ql rc (* Register lifes *)
    |> inter_mat    (* Interference matrix *)
    |> inter_graph  (* Interference graph *)
    |> dot_output_color "test.dot"
  | _ -> failwith "parse error"

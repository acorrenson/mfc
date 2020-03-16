(**************************************************************************)
(*                                                                        *)
(*                      This file is part of MFC                          *)
(*                  it is released under MIT license.                     *)
(*                https://opensource.org/licenses/MIT                     *)
(*                                                                        *)
(*          Copyright (c) 2020 Arthur Correnson, Nathan Graule            *)
(**************************************************************************)

open Mfc
open Mfc_generate
open Libnacc.Parsers
open Mfc_parser
open Mfc_env
open Mfc_quad
open Mfc_reg_alloc

module P = StringParser

let rec linecol_of_offset' =
  let const x _ = x
  in function
    | [] -> const (1, 1)
    | '\n'::cl -> (fun o ->
        let (line, col) = linecol_of_offset' cl o in
        (line + 1, col))
    | _::cl ->
      function
      | _ as o when o <= 0 -> (1, 1)
      | _ as o ->
        let (line, col) = linecol_of_offset' cl (o - 1) in
        (line, col + 1)
let linecol_of_offset s o = linecol_of_offset' (P.String.explode s) o
let _ =
  (* generate asm *)
  let ic = open_in "examples/fact.gen" in
  let oc = open_out "examples/fact.s" in
  generate ic oc;
  close_in ic;
  close_out oc;
  (* generate colored graph *)
  let ic2 = open_in "examples/fact.gen" in
  let r = read_all ic2 in
  let open StringParser in
  let open Mfc_difflist in
  try
    let ast = do_parse _prog r in
    let env = new_env () in
    push_frame env;
    new_function env "print" 0 1;
    let ql = quad_s ast env |> dmake in
    let rc = env.tmp_counter in
    print_quads stdout ql;
    get_lifes ql rc
    |> inter_mat
    |> inter_graph
    |> dot_output_color "examples/fact.dot"
  with
  | ParseException (o, _) -> print_endline ("Parse error at offset "^string_of_int o)

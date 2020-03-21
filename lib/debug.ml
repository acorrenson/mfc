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

let linecol_of_offset s o =
  let substr = String.sub s 0 o in
  match String.rindex_opt substr '\n' with
  | None -> (1, o+1)
  | Some nl ->
    let col = o - nl in
    let line = String.split_on_char '\n' substr |> List.length in
    (line, col)

let pp_error s o =
  let (l,c) = linecol_of_offset s o in
  let const x _ = x in
  let flip f a b = f b a in
  let get_line s l = String.split_on_char '\n' s |> flip List.nth l in
  print_endline ("Parse error at line "^string_of_int l^":");
  print_endline ("\t"^get_line s (l-1));
  print_endline ("\t"^String.init (c-1) (const ' ')^"^")

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
  let open Difflist in
  try
    let ast = do_parse _prog r in
    let env = new_env () in
    push_frame env;
    new_function env "print" 0 1;
    let ql = quad_s ast env |> to_list in
    let rc = env.tmp_counter in
    print_quads stdout ql;
    get_lifes ql rc
    |> inter_mat
    |> inter_graph
    |> dot_output_color "examples/fact.dot"
  with
  | ParseException (o, _) -> pp_error r o

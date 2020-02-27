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
open Mfc_parsing
open Mfc_parser
open Mfc_env
open Mfc_quad
open Mfc_reg_alloc

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
  match parse _prog r with
  | Some (ast, "") ->
    let env = new_env () in
    push_frame env;
    new_function env "print" 0 1;
    let ql = quad_s ast env |> Mfc_difflist.dmake in
    let rc = env.tmp_counter in
    get_lifes ql rc (* Register lifes *)
    |> inter_mat    (* Interference matrix *)
    |> inter_graph  (* Interference graph *)
    |> dot_output_color "examples/fact.dot"
  | _ -> failwith "parse error"

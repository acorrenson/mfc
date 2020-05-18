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

let input_file = ref ""
let output_file = ref ""
let reg_alloc_graph = ref false

let set_input f =
  if Sys.file_exists f then (
    if Filename.check_suffix f "gen"
    then input_file := f
    else raise (Arg.Bad "input file should have extension .gen")
  ) else raise (Arg.Bad ("file " ^ f ^ " does not exist"))

let set_output f =
  if Filename.check_suffix f ".s" then output_file := f
  else raise (Arg.Bad "output file should have extension .s")


let main =
  let specs = [
    ("-i", Arg.String set_input, "Input file (.gen)");
    ("-o", Arg.String set_output, "Output file (.s)");
    ("-r", Arg.Set reg_alloc_graph, "Generate the register allocation graph");
  ]
  in
  let usage = "mfc -i input.gen -o output.s [-r]" in
  Arg.parse specs (fun _ -> ()) usage;
  if !input_file = "" then begin
    Printf.eprintf "missing input file\nUsage : %s\n" usage;
    exit 1
  end
  else if !output_file = "" then begin
    Printf.eprintf "missing output file\nUsage : %s\n" usage;
    exit 2
  end else begin
    let ic = open_in !input_file in
    let oc = open_out !output_file in
    if !reg_alloc_graph then begin 
      let dot_file = (Filename.chop_suffix !output_file ".s") ^ "_reg_alloc_graph.dot" in
      generate ~graph:dot_file ic oc;
    end else generate ic oc;
    close_in ic;
    close_out oc
  end
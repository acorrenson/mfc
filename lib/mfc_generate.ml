open Mfc_parser
open Mfc_parsing
open Mfc_env
open Mfc_quad

let read_all ic =
  let data = ref "" in
  try
    while true do
      data := !data ^ (input_line ic) ^ "\n";
    done;
    !data
  with End_of_file -> !data

let generate ic =
  let data = read_all ic in
  let env = new_env () in
  let () = push_frame env in
  let () = new_function env "print" 0 1 in
  print_endline "@ ==================";
  print_endline "@ generated with mfc";
  print_endline "@ ==================";
  print_endline ".global main";
  print_endline "main:";
  parse _prog data |> 
  (function
    | Some (ast, "") -> quad_s ast env |> print_quads
    | _ -> failwith "parse error");
  print_endline "exit: b exit"




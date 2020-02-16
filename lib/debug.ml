
open Mfc
open Mfc_parser
open Mfc_parsing
open Mfc_env
open Mfc_quad

let _ =
  let env = new_env () in
  push_frame env;
  parse _prog "
    var x
    while (x < 2) {
      x = x + 1
    }"
  |>
  function
  | Some(a, _) -> quad_s a env |> print_quads
  | None -> failwith "parse error"

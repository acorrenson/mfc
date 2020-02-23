
open Mfc
open Mfc_generate

let _ =
  let ic = open_in "examples/test.gen" in
  generate ic
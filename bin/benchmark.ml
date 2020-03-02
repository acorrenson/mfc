open Core
open Core_bench
open Mfc.Mfc_ast
open Mfc.Mfc_env
open Mfc.Mfc_quad

let ast_base = [
  Declare "i";
  Declare "j";
  Set(Id "i", Cst 0);
  Set(Id "j", Cst 42);
  While(Cmp (Lt, Ref (Id "i"), Cst 10), Block [
    Set(Id "i", Binop(Add, Ref (Id "i"), Cst 1));
    Set(Id "j", Binop(Sub, Ref(Id "j"), Ref (Id "i")))
  ])
]

let repeat n l =
  let rec helper acc n l =
    if n = 0 then acc else helper (l @ acc) (n-1) l
  in helper [] n l

let ast = Block (repeat 1000 ast_base)

let _ =
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"bench"
      (fun () -> begin
        let env = new_env () in
        push_frame env;
        quad_s ast env
      end)
  ])

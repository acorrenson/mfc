(**************************************************************************)
(*                                                                        *)
(*                      This file is part of MFC                          *)
(*                  it is released under MIT license.                     *)
(*                https://opensource.org/licenses/MIT                     *)
(*                                                                        *)
(*          Copyright (c) 2020 Arthur Correnson, Nathan Graule            *)
(**************************************************************************)

open Mfc_quad
open Mfc_env
open Graph
open Pack

(** Set of integer *)
module IntSet = Set.Make(Int)

(** Graph K-coloring module *)
module Color = Coloring.Make(Graph)

(** Compute lifes of each virtual register *)
let get_lifes ql rc =
  (* Array of Integer Set (life) *)
  (* lifes[i] is the set of instructions using i *)
  let lifes = Array.make rc (IntSet.empty) in
  let save1 i r =
    let r' = IdType.reg_to_int r in
    lifes.(r') <- IntSet.add i lifes.(r');
  in
  let save2 i r1 r2 =
    let r1' = IdType.reg_to_int r1 in
    let r2' = IdType.reg_to_int r2 in
    lifes.(r1') <- IntSet.add i lifes.(r1');
    lifes.(r2') <- IntSet.add i lifes.(r2');
  in
  let save3 i r1 r2 r3 =
    let r1' = IdType.reg_to_int r1 in
    let r2' = IdType.reg_to_int r2 in
    let r3' = IdType.reg_to_int r3 in
    lifes.(r1') <- IntSet.add i lifes.(r1');
    lifes.(r2') <- IntSet.add i lifes.(r2');
    lifes.(r3') <- IntSet.add i lifes.(r3');
  in
  let update i q =
    match q with
    | Q_BINOP (_, r1, r2, r3) -> save3 i r1 r2 r3
    | Q_BINOPI (_, r1, r2, _) -> save2 i r1 r2
    | Q_CMP (r1, r2)          -> save2 i r1 r2
    | Q_IFP (r1, _)           -> save1 i r1
    | Q_LDR (r1, r2)          -> save2 i r1 r2
    | Q_POP (r1)              -> save1 i r1
    | Q_PUSH (r1)             -> save1 i r1
    | Q_SET (r1, r2)          -> save2 i r1 r2
    | Q_SETI (r1, _)          -> save1 i r1
    | Q_STR (r1, r2)          -> save2 i r1 r2
    | Q_UNOP (_, r1, r2)      -> save2 i r1 r2
    | _ -> ()
  in
  List.iteri update ql;
  lifes

(** Compute inteference matrix from virtual register lifes *)
let inter_mat arr =
  let len = Array.length arr in
  let mat = Array.make_matrix len len false in
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
      let inter = IntSet.inter arr.(i) arr.(j) in
      if i <> j && inter = IntSet.empty 
      then mat.(i).(j) <- true
    done
  done;
  mat

(** Compute inteference graph from virtual interference matrix *)
let inter_graph mat =
  let g = Graph.create () in
  let l = Array.length mat in
  let e = List.init l (fun i -> Graph.V.create i) in
  let a = Array.of_list e in
  for i = 0 to l - 1 do
    for j = 0 to l - 1 do
      if mat.(i).(j) then (
        Graph.add_edge g a.(i) a.(j)
      )
    done
  done;
  g

(** Perform register allocation, returns a int -> int map *)
let reg_alloc g =
  let m = Color.coloring g 12 in
  Color.H.find m

(** Perform reg allocation and output the result to a dot file *)
let dot_output_color f g =
  let open Printf in
  let oc = open_out f in
  let color x = reg_alloc g x |> function
    | 0 -> "red"
    | 1 -> "orange"
    | 2 -> "yellow"
    | 3 -> "cyan"
    | 4 -> "green"
    | 5 -> "blue"
    | 6 -> "pink"
    | 7 -> "purple"
    | 8 -> "grey"
    | 9 -> "brown"
    | 10 -> "magenta"
    | 11 -> "chartreuse"
    | 12 -> "crimson"
    | _ -> ""
  in
  fprintf oc "Graph {\n";
  Graph.iter_vertex (fun v ->
      fprintf oc "\t%d [style=\"filled\"; color=\"%s\"];\n" 
        (Graph.V.label v) 
        (color v)
    ) g;
  Graph.iter_edges (fun s d ->
      fprintf oc "\t%d -- %d;\n" 
        (Graph.V.label s)
        (Graph.V.label d)
    ) g;
  fprintf oc "}\n";
  close_out oc
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

(** Graph K-coloring module *)
module Color = Coloring.Make(Graph)

(**
   Compute the lifetime of each virtual register.
   @param ql  Quad list
   @param rc  Virtual registre number
*)
let get_lifes ql rc =
  let lifes = Array.make rc (-1, -1) in
  let update i rl =
    List.iter (fun r ->
        let r' = IdType.reg_to_int r in
        let b, d = lifes.(r') in
        if b = -1 
        then lifes.(r') <- (i, d)
        else lifes.(r') <- (b, i)
      ) rl
  in
  List.iteri (fun i q ->
      match q with
      | Q_BINOP (_, r1, r2, r3) -> update i [r1; r2; r3];
      | Q_BINOPI (_, r1, r2, _) -> update i [r1; r2];
      | Q_CMP (r1, r2)          -> update i [r1; r2];
      | Q_IFP (r1, _)           -> update i [r1];
      | Q_LDR (r1, r2)          -> update i [r1; r2];
      | Q_POP (r1)              -> update i [r1];
      | Q_PUSH (r1)             -> update i [r1];
      | Q_SET (r1, r2)          -> update i [r1; r2];
      | Q_SETI (r1, _)          -> update i [r1];
      | Q_STR (r1, r2)          -> update i [r1; r2];
      | Q_UNOP (_, r1, r2)      -> update i [r1; r2];
      | _ -> ()
    ) ql;
  lifes


(** Compute inteference matrix from virtual register lifes 
    @param  arr   lifetimes array *)
let inter_mat arr =
  let len = Array.length arr in
  let mat = Array.make_matrix len len false in
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
      let b1, d1 = arr.(i) in
      let b2, d2 = arr.(j) in
      if i <> j && not (d1 < b2 || d2 < b1)
      then mat.(i).(j) <- true
    done
  done;
  mat


(** Compute inteference graph from virtual interference matrix 
    @param  mat   interference matrix *)
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


(** Perform register allocation, returns a int -> int map 
    @param  g   intereference graph *)
let reg_alloc g =
  let m = Color.coloring g 12 in
  Color.H.find m


(** Perform reg allocation and output the result to a dot file 
    @param  f   output file
    @param  g   interference graph *)
let dot_output_color f g =
  let open Printf in
  let oc = open_out f in
  let c = reg_alloc g in
  let color x = c x |> function
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


(** Perform register allocation
    @param  ql  quad list
    @param  rc  register number *)
let alloc ql rc =
  let l = get_lifes ql rc in
  let g = inter_mat l |> inter_graph in
  let c = reg_alloc g in
  let opt x =
    IdType.reg_to_int x 
    |> Graph.find_vertex g
    |> c
    |> IdType.reg
  in
  List.map (
    function
    | Q_BINOP (op, a, b, c) ->
      Q_BINOP (op, opt a, opt b, opt c)
    | Q_BINOPI (op, a, b, c) ->
      Q_BINOPI (op, opt a, opt b, c)
    | Q_BRANCH _ as q -> q
    | Q_BRANCH_LINK _ as q -> q
    | Q_CMP (a, b) ->
      Q_CMP (opt a, opt b)
    | Q_GOTO _ as q -> q
    | Q_IFP (a, b) ->
      Q_IFP (opt a, b)
    | Q_LABEL _ as q -> q
    | Q_LDR (a, b) ->
      Q_LDR (opt a, opt b)
    | Q_POP (a) -> 
      Q_POP (opt a)
    | Q_PUSH (a) ->
      Q_PUSH (opt a)
    | Q_SET (a, b) ->
      Q_SET (opt a, opt b)
    | Q_SETI (a, b) ->
      Q_SETI (opt a, b)
    | Q_STR (a, b) ->
      Q_STR (opt a, opt b)
    | Q_UNOP (u, a, b) ->
      Q_UNOP (u, opt a, opt b)
  ) ql
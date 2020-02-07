open Mfc_ast

(** A frame (in the stack) *)
type frame = {
  mutable variable_counter: int;
  mutable local_vars : (string * (int * ctype)) list
}

(** A complete environment associated with a program *)
type env = {
  mutable label_counter: int;
  mutable frames : frame list;
}

let new_label env =
  let l = Printf.sprintf "label_%d" env.label_counter in
  env.label_counter <- env.label_counter + 1;
  l

let new_tmp env t =
  let l = Printf.sprintf "reg_%s_%d" (str_of_type t) env.label_counter in
  env.label_counter <- env.label_counter + 1;
  l

(** Lookup for local variable x (type and offset) *)
let lookup_opt env x =
  match env.frames with
  | [] -> None
  | f::_ -> List.assoc_opt x f.local_vars

(** Lookup for local variable x offset *)
let lookup_opt_offset env x =
  match lookup_opt env x with
  | None -> None
  | Some (o, _) -> Some o

(** Lookup for local variable x type *)
let lookup_opt_type env x =
  match lookup_opt env x with
  | None -> None
  | Some (_, t) -> Some t


(** Push a new frame *)
let push_frame (e:env) =
  e.frames <- {variable_counter = 0; local_vars = []}::e.frames

(** Get the top frame *)
let top_frame (e:env) =
  List.hd e.frames

(** Pop the top frame *)
let pop_frame (e:env) =
  e.frames <- List.tl e.frames

(** Add a local variable to the env *)
let new_local e x t =
  let l = (top_frame e) in
  l.local_vars <- (x, (l.variable_counter, t))::l.local_vars;
  l.variable_counter <- l.variable_counter + 1

let new_env () = {label_counter = 0; frames = []}
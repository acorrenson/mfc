(** A frame (in the stack) *)
type frame = {
  mutable variable_counter: int;
  mutable local_vars : (string * int) list
}

(** A complete environment associated with a program *)
type env = {
  mutable label_counter: int;
  mutable frames : frame list;
}

(** Lookup for local variable x offset *)
let lookup_opt env x =
  match env.frames with
  | [] -> None
  | f::_ -> List.assoc_opt x f.local_vars 

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
let new_local e x =
  let l = (top_frame e) in
  l.local_vars <- (x, l.variable_counter)::l.local_vars;
  l.variable_counter <- l.variable_counter + 1

let _ =
  let f1 = {variable_counter = 2; local_vars = [("x", 0); ("y", 1)]} in
  let e = {label_counter = 0; frames = [f1]} in
  new_local e "z";
  push_frame e;
  new_local e "g";
  pop_frame e;
  match lookup_opt e "z" with
  | None -> print_endline "error"
  | Some i -> print_int i

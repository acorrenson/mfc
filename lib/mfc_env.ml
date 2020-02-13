(** A frame (in the stack) *)
type frame = {
  mutable variable_counter: int;
  mutable local_vars : (string * int) list
}

(** A complete environment associated with a program *)
type env = {
  mutable label_counter: int;
  mutable frames : frame list;
  mutable functions : (string * (string * int * int)) list;
}

let new_label env =
  let l = Printf.sprintf "label_%d" env.label_counter in
  env.label_counter <- env.label_counter + 1;
  l

let new_tmp env =
  let l = Printf.sprintf "reg_%d" env.label_counter in
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
  | _ as off -> off

let lookup_opt_fun env f =
  List.assoc_opt f (env.functions)


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

let new_function e s r p =
  e.functions <- (s, (Printf.sprintf "function_%s_%d" s (e.label_counter), r, p))::e.functions;
  e.label_counter <- e.label_counter + 1


let new_env () = {label_counter = 0; frames = []; functions = []}
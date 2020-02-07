type frame = {
  mutable variable_counter: int;
  mutable local_vars : (string * int) list
}

type env = {
  mutable label_counter: int;
  mutable frames : frame list;
}

let lookup_opt env x =
  let rec lookup_frame f x =
    List.assoc_opt x f.local_vars
  in
  lookup_frame (List.hd env.frames) x

let push_frame (e:env) f =
  e.frames <- f::e.frames

let last_frame (e:env) =
  List.hd e.frames

let new_local e x =
  let l = (last_frame e) in
  l.local_vars <- (x, l.variable_counter)::l.local_vars;
  l.variable_counter <- l.variable_counter + 1

let _ =
  let f1 = {variable_counter = 2; local_vars = [("x", 0); ("y", 1)]} in
  let e = {label_counter = 0; frames = [f1]} in
  new_local e "z";
  new_local e "g";
  match lookup_opt e "g" with
  | None -> print_endline "error"
  | Some i -> print_int i

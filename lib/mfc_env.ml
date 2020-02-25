(** Module for Abstract Identifier manipulation *)
module IdType :
sig
  type reg
  type lab

  (** Instanciate a register *)
  val reg : int -> reg

  (** Instanciate a label *)
  val lab : int -> lab

  (** Instanciate a named label *)
  val nlab : string -> lab

  (** Register to int *)
  val reg_to_int : reg -> int

  (** Label to string *)
  val lab_to_string : lab -> string
end = struct
  type reg = int
  type lab = string
  let reg i = i
  let lab i = Printf.sprintf "label_%d" i
  let nlab i = i
  let reg_to_int r = r
  let lab_to_string r = r
end


(** A frame (in the stack) *)
type frame = {
  mutable variable_counter: int;
  mutable local_vars : (string * int) list
}

(** A complete environment associated with a program *)
type env = {
  mutable label_counter: int;
  mutable tmp_counter: int;
  mutable frames : frame list;
  mutable functions : (string * (IdType.lab * int * int)) list;
}

let new_label env =
  let l = IdType.lab env.label_counter in
  env.label_counter <- env.label_counter + 1;
  l

let new_tmp env =
  let l = IdType.reg env.tmp_counter in
  env.tmp_counter <- env.tmp_counter + 1;
  l

let clr_tmp env n =
  env.label_counter <- env.tmp_counter - n

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
  e.functions <- (s, (IdType.nlab s, r, p))::e.functions


let new_env () = {label_counter = 0; tmp_counter = 0; frames = []; functions = []}
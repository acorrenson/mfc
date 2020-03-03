(**************************************************************************)
(*                                                                        *)
(*                      This file is part of MFC                          *)
(*                  it is released under MIT license.                     *)
(*                https://opensource.org/licenses/MIT                     *)
(*                                                                        *)
(*          Copyright (c) 2020 Arthur Correnson, Nathan Graule            *)
(**************************************************************************)

open Mfc_ast
open Mfc_env
open Mfc_difflist

(** Alias for virtual registers *)
type vreg = IdType.reg

(** Alias for virtual labels *)
type vlab = IdType.lab

(** Type for (ARM-like) quads *)
type quad =
  | QARM_BINOP of binop * vreg * vreg * vreg
  | QARM_BINOPI of binop * vreg * vreg * int
  | QARM_IFP of vreg * int
  | QARM_UNOP of unop * vreg * vreg
  | QARM_SET of vreg * vreg
  | QARM_SETI of vreg * int
  | QARM_STR of vreg * vreg
  | QARM_LDR of vreg * vreg
  | QARM_LABEL of vlab
  | QARM_PUSH of vreg
  | QARM_POP of vreg
  | QARM_GOTO of vlab
  | QARM_BRANCH_LINK of vlab
  | QARM_CMP of vreg * vreg
  | QARM_BRANCH of compare * vlab

(** Generate quads for Statements ({!Mfc_ast.s_ast})
    @param s     statement ast
    @param env   environement ({!Mfc_env.env}) *)
let rec quad_s s env =
  match s with
  | Set (Id i, e) ->
    begin
      match lookup_opt env i with
      | None -> failwith ("unknow local variable " ^ i)
      | Some off ->
        let v = new_tmp env in
        let q1, v1 = quad_e e env in
        q1 <+ QARM_IFP (v, off) <+ QARM_STR (v1, v)
    end
  | Block s ->
    (* List.fold_left (@) [] (List.map (fun s -> quad_s s env) s) *)
    (* List.map (fun s -> quad_s s env) s |> dconcat *)
    dconst s |> fold_left (fun acc s -> acc ++ quad_s s env) dzero
  | Call (Id i, le) ->
    let lres = List.fold_left (fun a e -> a @ [quad_e e env]) [] le in
    let lq, lr = List.split lres in
    let q = dconcat lq in
    let push = dconst_map (fun s -> QARM_PUSH (s)) lr in
    begin
      match lookup_opt_fun env i with
      | Some (l, r, p) when (r = 0 && p = List.length le) -> q ++ push <+ QARM_BRANCH_LINK l
      | _ -> failwith "Error in function call"
    end
  | If (c, s1, s2) ->
    let _si = new_label env in
    let _sinon = new_label env in
    let qc = quad_c c env _si _sinon in
    let q1 = quad_s s1 env in
    let q2 = quad_s s2 env in
    ((qc <+ QARM_LABEL _si) ++ q1 <+ QARM_LABEL _sinon) ++ q2
  | While (c, s) ->
    let _loop = new_label env in
    let _body = new_label env in
    let _end = new_label env in
    let qc = quad_c c env _body _end in
    let q = quad_s s env in
    (dconst ((QARM_LABEL _loop)::(dmake qc)) <+ QARM_LABEL _body) ++ q <+ QARM_GOTO _loop <+ QARM_LABEL _end
  | Ret e ->
    let qe, ve = quad_e e env in
    qe <+ QARM_PUSH ve
  | Declare s ->
    new_local env s;
    dzero
  | DeclareFun (s, r, p) ->
    new_function env s r p;
    dzero


(** Generate quad for Expressions ({!Mfc_ast.e_ast})
    @param e     expression ast
    @param env   current env ({!Mfc_env.env}) *)
and quad_e e env =
  match e with
  | Binop (op, e1, Cst i) ->
    let r = new_tmp env in
    let q1, r1 = quad_e e1 env in
    (* q1 @ [ Q_BINOPI (op, r, r1, i)], r *)
    (q1 <+ QARM_BINOPI(op, r, r1, i), r)
  | Binop (op, e1, e2) ->
    let r = new_tmp env in
    let q1, r1 = quad_e e1 env in
    let q2, r2 = quad_e e2 env in
    (* q1 @ q2 @ [ Q_BINOP (op, r, r1, r2)], r *)
    (q1 ++ q2 <+ QARM_BINOP (op,r,r1,r2), r)
  | Cst i ->
    let r = new_tmp env in
    (* [Q_SETI (r, i)], r *)
    (dsnoc (QARM_SETI (r,i)),r)
  | Ref (Id x) ->
    let r1 = new_tmp env in
    let r2 = new_tmp env in
    begin
      match lookup_opt env x with
      | None -> failwith ("unknown variable " ^ x)
      | Some off -> (dsnoc (QARM_IFP(r1,off)) <+ QARM_LDR(r2,r1), r2)
    end
  | Ecall (Id x, le) ->
    let lres = List.fold_left (fun a e -> a @ [quad_e e env]) [] le in
    let lq, lr = List.split lres in
    let q = dconcat lq in
    let push = dconst_map (fun s -> QARM_PUSH (s)) lr in
    let ret = new_tmp env in
    begin
      match lookup_opt_fun env x with
      | Some(l, r, p) when (r = 1 && p = List.length le) ->
        (* (q @ push @ [Q_BRANCH_LINK l] @ [Q_POP ret]), ret *)
        (q ++ push <+ QARM_BRANCH_LINK l <+ QARM_POP ret, ret)
      | _ -> failwith "Error in function call"
    end
  | Unop (op, e1) ->
    let q1, r1 = quad_e e1 env in
    let r = new_tmp env in
    (* (q1 @ [Q_UNOP (op, r, r1)]), r *)
    (q1 <+ QARM_UNOP (op, r, r1), r)


(** Generate quads for tests ({!Mfc_ast.c_ast})
    @param c      condition ast
    @param env    current env ({!Mfc_env.env})
    @param si     label to target if test succeed
    @param sinon  label to target if test fails *)
and quad_c c env si sinon =
  let inv c =
    match c with
    | Lt -> Ge
    | Le -> Gt
    | Eq -> Ne
    | Gt -> Le
    | Ge -> Lt
    | Ne -> Eq
  in
  let rec cond c env si sinon p: quad dlist =
    match c with
    | Not c ->
      cond c env sinon si true
    | Or (c1, c2) ->
      let l = new_label env in
      let q1 = cond c1 env l sinon true in
      let q2 = cond c2 env si sinon true in
      begin
        match dmake q1 |> List.rev with
        | (QARM_GOTO a)::r when a = l -> dconst (List.rev r) ++ q2
        | _ -> (q1 <+ QARM_LABEL l) ++ q2
      end
    | And (c1, c2) ->
      let l = new_label env in
      let q1 = cond c1 env l sinon false in
      let q2 = cond c2 env si sinon  false in
      begin
        match dmake q1 |> List.rev with
        | (QARM_GOTO a)::r when a = l -> dconst (List.rev r) ++ q2
        | _ -> (q1 <+ QARM_LABEL l) ++ q2
      end
    | Cmp (c, e1, e2) ->
      let q1, v1 = quad_e e1 env in
      let q2, v2 = quad_e e2 env in
      if p then
        q1 ++ q2 <+ QARM_CMP (v1, v2) <+ QARM_BRANCH (c, si) <+ QARM_GOTO sinon
      else
        q1 ++ q2 <+ QARM_CMP (v1, v2) <+ QARM_BRANCH (inv c, sinon) <+ QARM_GOTO si
  in
  cond c env si sinon true


(** Pretty print quad list
    @param lq   quad list *)
let rec print_quads oc lq =
  match lq with
  | [] -> ()
  | QARM_BINOP (op, r1, r2, r3)::r ->
    let open IdType in
    let r1' = reg_to_int r1 in
    let r2' = reg_to_int r2 in
    let r3' = reg_to_int r3 in
    Printf.fprintf oc "%-4s r%d, r%d, r%d\n" (bstr op) r1' r2' r3';
    print_quads oc r
  | QARM_BINOPI (op, r1, r2, i)::r ->
    let open IdType in
    let r1' = reg_to_int r1 in
    let r2' = reg_to_int r2 in
    Printf.fprintf oc "%-4s r%d, r%d, #%d\n" (bstr op) r1' r2' i;
    print_quads oc r
  | QARM_GOTO l::r ->
    Printf.fprintf oc "b %s\n" (l |> IdType.lab_to_string);
    print_quads oc r
  | QARM_LABEL l::r ->
    Printf.fprintf oc "%s:\n" (IdType.lab_to_string l);
    print_quads oc r
  | QARM_POP l::r ->
    Printf.fprintf oc "pop  r%d\n" (IdType.reg_to_int l);
    print_quads oc r
  | QARM_PUSH l::r ->
    Printf.fprintf oc "push {r%d}\n" (IdType.reg_to_int l);
    print_quads oc r
  | QARM_LDR (a, v)::r ->
    Printf.fprintf oc "ldrb  r%d, [r%d]\n" (IdType.reg_to_int a) (IdType.reg_to_int v);
    print_quads oc r
  | QARM_STR (a, v)::r ->
    Printf.fprintf oc "strb  r%d, [r%d]\n" (IdType.reg_to_int a) (IdType.reg_to_int v);
    print_quads oc r
  | QARM_SET (a, b)::r ->
    Printf.fprintf oc "mov  r%d, r%d\n" (IdType.reg_to_int a) (IdType.reg_to_int b);
    print_quads oc r
  | QARM_SETI (a, b)::r ->
    Printf.fprintf oc "mov  r%d, #%d\n" (IdType.reg_to_int a) b;
    print_quads oc r
  | QARM_UNOP (_, b, c)::r ->
    Printf.fprintf oc "%-4s r%d, r%d\n" ("not") (IdType.reg_to_int b) (IdType.reg_to_int c);
    print_quads oc r
  | QARM_IFP (a, b)::r ->
    Printf.fprintf oc "add  r%d, SP, #%d\n" (IdType.reg_to_int a) b;
    print_quads oc r
  | QARM_CMP (a, b)::r ->
    Printf.fprintf oc "cmp  r%d, r%d\n" (IdType.reg_to_int a) (IdType.reg_to_int b);
    print_quads oc r
  | QARM_BRANCH (c, a)::r ->
    Printf.fprintf oc "b%s  %s\n" (cstr c) (IdType.lab_to_string a);
    print_quads oc r
  | QARM_BRANCH_LINK (l)::r ->
    Printf.fprintf oc "bl %s\n" (IdType.lab_to_string l);
    print_quads oc r

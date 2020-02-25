open Mfc_ast
open Mfc_env

type treg = IdType.reg
type tlab = IdType.lab

type quad =
  | Q_BINOP of binop * treg * treg * treg
  | Q_BINOPI of binop * treg * treg * int
  | Q_IFP of treg * int
  | Q_UNOP of unop * treg * treg
  | Q_SET of treg * treg
  | Q_SETI of treg * int
  | Q_STR of treg * treg
  | Q_LD of treg * treg
  | Q_LABEL of tlab
  | Q_PUSH of treg
  | Q_POP of treg
  | Q_GOTO of tlab
  | Q_BRANCH_LINK of tlab
  | Q_CMP of treg * treg
  | Q_BRANCH of compare * tlab

let rec quad_s s env =
  match s with
  | Set (Id i, e) ->
    begin
      match lookup_opt env i with
      | None -> failwith ("unknow local variable " ^ i)
      | Some off ->
        let v = new_tmp env in
        let q1, v1 = quad_e e env in
        (* clr_tmp env 1; *)
        q1 @ [Q_IFP (v, off); Q_STR (v1, v)]
    end
  | Block s ->
    List.fold_left (@) [] (List.map (fun s -> quad_s s env) s)
  | Call (Id i, le) ->
    let lres = List.fold_left (fun a e -> a @ [quad_e e env]) [] le in
    let lq, lr = List.split lres in
    let q = List.fold_left (@) [] lq in
    let push = List.map (fun s -> Q_PUSH (s)) lr in
    (* clr_tmp env (List.length le); *)
    begin
      match lookup_opt_fun env i with
      | Some (l, r, p) when (r = 0 && p = List.length le) -> q @ push @ [Q_BRANCH_LINK l]
      | _ -> failwith "Error in function call"
    end
  | If (c, s1, s2) ->
    let _si = new_label env in
    let _sinon = new_label env in
    let qc = quad_c c env _si _sinon in
    let q1 = quad_s s1 env in
    let q2 = quad_s s2 env in
    qc @ [Q_LABEL _si] @ q1 @ [Q_LABEL _sinon] @ q2
  | While (c, s) ->
    let _loop = new_label env in
    let _body = new_label env in
    let _end = new_label env in
    let qc = quad_c c env _body _end in
    let q = quad_s s env in
    [Q_LABEL _loop] @ qc @ [Q_LABEL _body] @ q @ [Q_GOTO _loop; Q_LABEL _end]
  | Ret e ->
    let qe, ve = quad_e e env in
    qe @ [Q_PUSH ve]
  | Declare s ->
    new_local env s;
    []
  | DeclareFun (s, r, p) ->
    new_function env s r p;
    []

and quad_e e env = 
  match e with
  | Binop (op, e1, Cst i) ->
    let r = new_tmp env in
    let q1, r1 = quad_e e1 env in
    q1 @ [ Q_BINOPI (op, r, r1, i)], r
  | Binop (op, e1, e2) ->
    let r = new_tmp env in
    let q1, r1 = quad_e e1 env in
    let q2, r2 = quad_e e2 env in
    q1 @ q2 @ [ Q_BINOP (op, r, r1, r2)], r
  | Cst i ->
    let r = new_tmp env in
    [Q_SETI (r, i)], r
  | Ref (Id x) ->
    let r1 = new_tmp env in
    let r2 = new_tmp env in
    begin
      match lookup_opt env x with
      | None -> failwith "unknown variable"
      | Some off -> [Q_IFP (r1, off); Q_LD (r2, r1)], r2
    end
  | Ecall (Id x, le) ->
    let lres = List.fold_left (fun a e -> a @ [quad_e e env]) [] le in
    let lq, lr = List.split lres in
    let q = List.fold_left (@) [] lq in
    let push = List.map (fun s -> Q_PUSH (s)) lr in
    let ret = new_tmp env in
    (* clr_tmp env 1; *)
    begin
      match lookup_opt_fun env x with
      | Some(l, r, p) when (r = 1 && p = List.length le) ->
        (q @ push @ [Q_BRANCH_LINK l] @ [Q_POP ret]), ret
      | _ -> failwith "Error in function call"
    end
  | Unop (op, e1) ->
    let q1, r1 = quad_e e1 env in
    let r = new_tmp env in
    (q1 @ [Q_UNOP (op, r, r1)]), r

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
  let rec cond c env si sinon p =
    match c with
    | Not c ->
      cond c env sinon si true
    | Or (c1, c2) ->
      let l = new_label env in
      let q1 = cond c1 env l sinon true in
      let q2 = cond c2 env si sinon true in
      begin
        match List.rev q1 with
        | (Q_GOTO a)::r when a = l -> (List.rev r) @ q2
        | _ -> q1 @ [Q_LABEL l] @ q2
      end
    | And (c1, c2) ->
      let l = new_label env in
      let q1 = cond c1 env l sinon false in
      let q2 = cond c2 env si sinon  false in
      begin
        match List.rev q1 with
        | (Q_GOTO a)::r when a = l -> (List.rev r) @ q2
        | _ -> q1 @ [Q_LABEL l] @ q2
      end
    | Cmp (c, e1, e2) ->
      let q1, v1 = quad_e e1 env in
      let q2, v2 = quad_e e2 env in
      (* clr_tmp env 2; *)
      if p then
        q1 @ q2 @ [Q_CMP (v1, v2); Q_BRANCH (c, si); Q_GOTO sinon]
      else
        q1 @ q2 @ [Q_CMP (v1, v2); Q_BRANCH (inv c, sinon); Q_GOTO si]
  in
  cond c env si sinon true


let rec print_quads lq =
  match lq with
  | [] -> ()
  | Q_BINOP (op, r1, r2, r3)::r ->
    let open IdType in
    let r1' = reg_to_int r1 in
    let r2' = reg_to_int r2 in
    let r3' = reg_to_int r3 in
    Printf.printf "%-4s r%d, r%d, r%d\n" (bstr op) r1' r2' r3';
    print_quads r
  | Q_BINOPI (op, r1, r2, i)::r ->
    let open IdType in
    let r1' = reg_to_int r1 in
    let r2' = reg_to_int r2 in
    Printf.printf "%-4s r%d, r%d, #%d\n" (bstr op) r1' r2' i;
    print_quads r
  | Q_GOTO l::r ->
    Printf.printf "b %s\n" (l |> IdType.lab_to_string);
    print_quads r
  | Q_LABEL l::r ->
    Printf.printf "%s:\n" (IdType.lab_to_string l);
    print_quads r
  | Q_POP l::r ->
    Printf.printf "pop  r%d\n" (IdType.reg_to_int l);
    print_quads r
  | Q_PUSH l::r ->
    Printf.printf "push {r%d}\n" (IdType.reg_to_int l);
    print_quads r
  | Q_LD (a, v)::r ->
    Printf.printf "ldr  r%d, [r%d]\n" (IdType.reg_to_int a) (IdType.reg_to_int v);
    print_quads r
  | Q_STR (a, v)::r ->
    Printf.printf "str  r%d, [r%d]\n" (IdType.reg_to_int a) (IdType.reg_to_int v);
    print_quads r
  | Q_SET (a, b)::r ->
    Printf.printf "mov  r%d, r%d\n" (IdType.reg_to_int a) (IdType.reg_to_int b);
    print_quads r
  | Q_SETI (a, b)::r ->
    Printf.printf "mov  r%d, #%d\n" (IdType.reg_to_int a) b;
    print_quads r
  | Q_UNOP (_, b, c)::r ->
    Printf.printf "%-4s r%d, r%d\n" ("not") (IdType.reg_to_int b) (IdType.reg_to_int c);
    print_quads r
  | Q_IFP (a, b)::r ->
    Printf.printf "add  r%d, SP, #%d\n" (IdType.reg_to_int a) b;
    print_quads r
  | Q_CMP (a, b)::r ->
    Printf.printf "cmp  r%d, r%d\n" (IdType.reg_to_int a) (IdType.reg_to_int b);
    print_quads r
  | Q_BRANCH (c, a)::r ->
    Printf.printf "b%s  %s\n" (cstr c) (IdType.lab_to_string a);
    print_quads r
  | Q_BRANCH_LINK (l)::r ->
    Printf.printf "bl %s\n" (IdType.lab_to_string l);
    print_quads r

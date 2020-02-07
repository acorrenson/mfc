open Mfc_ast
open Mfc_env

type quad =
  | Q_BINOP of binop * string * string * string
  | Q_IFP of string * int
  | Q_UNOP of unop * string * string
  | Q_SET of string * string
  | Q_SETI of string * int
  | Q_STR of string * string
  | Q_LD of string * string
  | Q_LABEL of string
  | Q_PUSH of string
  | Q_POP of string
  | Q_GOTO of string
  | Q_CMP of string * string
  | Q_BRANCH of compare * string

let rec quad_s s env =
  match s with
  | Seq (s1, s2) ->
    let q1 = quad_s s1 env in
    let q2 = quad_s s2 env in
    q1 @ q2
  | Set (Id i, e) ->
    begin
      match lookup_opt env i with
      | None -> failwith ("unknow local variable " ^ i)
      | Some (o, T_int) ->
        let v = new_tmp env T_int in
        let q1, v1 = quad_e e env in
        q1 @ [Q_IFP (v, o); Q_STR (v1, v)]
      | _ -> failwith "unable to assign non int variables"
    end
  | Block s ->
    let l = new_label env in
    let q = quad_s s env in
    [Q_LABEL l] @ q
  | Call (Id i, le) ->
    let lres = List.fold_left (fun a e -> a @ [quad_e e env]) [] le in
    let lq, lr = List.split lres in
    let q = List.fold_left (@) [] lq in
    let push = List.map (fun s -> Q_PUSH (s)) lr in
    q @ push @ [Q_GOTO i]
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
    [Q_LABEL _loop] @ qc @ [Q_LABEL _body] @ q @ [Q_LABEL _end]
  | Ret e ->
    let qe, ve = quad_e e env in
    qe @ [Q_PUSH ve]
  | Declare (s, t) ->
    new_local env s t;
    []
and quad_e _ _ = [], ""
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
      let q2 = cond c1 env si sinon true in
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
      if p then
        q1 @ q2 @ [Q_CMP (v1, v2); Q_BRANCH (c, si); Q_GOTO sinon]
      else
        q1 @ q2 @ [Q_CMP (v1, v2); Q_BRANCH (inv c, sinon); Q_GOTO si]
  in
  cond c env si sinon true





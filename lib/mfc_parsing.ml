type 'a parser = P of (string -> ('a * string) option)

let parse p inp =
  match p with
  | P (p) -> p inp

let fmap f p =
  P (fun inp -> match parse p inp with
      | None -> None
      | Some (v, out) -> Some (f v, out))

let (>>=) p f =
  P (fun inp -> match parse p inp with
      | None -> None
      | Some(v, out) -> parse (f v) out)

let (<|>) p q =
  P (fun inp -> match parse p inp with
      | None -> parse q inp
      | _ as r -> r)

let (let*) x f = x >>= f

let pchar c =
  P (fun inp -> match inp with
      | "" -> None
      | x when x.[0] = c -> Some(c, String.sub x 1 ((String.length x) - 1))
      | _ -> None)

let p =
  let* x = pchar 'c' in
  let* y = pchar 'd' in
  P (fun inp -> Some ((x, y), inp))

let rec some (p : 'a parser) =
  P (fun inp ->
      match parse p inp with
      | None -> Some ([], inp)
      | Some(x, rest) ->
        match parse (some p) rest with
        | Some (lx, r) -> Some (x::lx, r)
        | None -> None
    )

let many (p : 'a parser) =
  let* x = p in
  let* lx = some p in
  P (fun inp -> 
      Some (x::lx, inp)
    )

let anychar_of cl =
  match cl with
  | x::rest -> List.fold_left (<|>) (pchar x) (List.map pchar rest)
  | [] -> P (fun _ -> None)

let explode s =
  let rec step lc i =
    if i = String.length s then lc
    else step (lc @ [s.[i]]) (i+1)
  in
  step [] 0

let anychar_in s =
  explode s |> anychar_of

let parser f = P (fun inp -> f inp)

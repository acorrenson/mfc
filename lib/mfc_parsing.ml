module String =
struct
  include String

  let explode s =
    let rec step lc i =
      if i = length s then lc
      else step (lc @ [s.[i]]) (i+1)
    in
    step [] 0

  let rec combine: char list -> string =
    function
    | [] -> ""
    | c::cl -> (String.make 1 c) ^ (combine cl)

  let rec combine_str: string list -> string =
    function
    | [] -> ""
    | s::sl -> s ^ (combine_str sl)

  let substr l r s = String.sub s l ((length s) - 1 - r)
  let rest = substr 1 0
end

type 'a pdata = ('a * string)
type 'a presult = 'a pdata option
type 'a parser = P of (string -> 'a presult)

exception ParseException of string

let pzero = P(fun _ -> None)
let pone = P(fun rest -> Some([], rest))

let parse p inp =
  match p with
  | P (p) -> p inp

let pmap f: 'a presult -> 'b presult =
  function
  | Some(c, out) -> Some(f c, out)
  | None -> None

let pleak: 'a presult -> 'a =
  function
  | Some(c, _) -> c
  | None -> raise (ParseException "Attempted to get failed parse")

let fmap f p =
  P (fun inp -> parse p inp |> pmap f)

let (>>=) p f =
  P begin
    fun inp -> match parse p inp with
      | None -> None
      | Some(v, out) -> parse (f v) out
end

let (<|>) p q =
  P (fun inp -> match parse p inp with
      | None -> parse q inp
      | _ as r -> r)

let (let*) x f = x >>= f

let (~~) p q =
  let* x = p in
  let* y = q in
  P(fun inp -> Some((x,y), inp))

let (||>) p f = fmap f p

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

let any = P(fun inp -> Some(String.get inp 0, String.rest inp))

let forget p = P begin
  fun inp -> match parse p inp with
  | Some(_, rest) -> Some([], rest)
  | None -> Some([], inp)
end

let ignore p = P begin
  fun inp -> match parse p inp with
  | Some(_, rest) -> Some([], rest)
  | None -> None
end

let anychar_of cl =
  match cl with
  | x::rest -> List.fold_left (<|>) (pchar x) (List.map pchar rest)
  | [] -> P (fun _ -> None)

let anychar_in s =
  String.explode s |> anychar_of

let rec sequence: ('a parser list -> 'a list parser) =
  function
  | [] -> pone
  | p::pl ->
  begin
    let* x = p in
    let* xs = sequence pl in
    P(fun inp -> Some(x :: xs, inp))
  end

let sequence_of_chars cl = sequence (List.map pchar cl)
let sequence_of_string s = sequence_of_chars (String.explode s)
let literal s = sequence_of_string s ||> String.combine

let wrap l r p = sequence [ignore l; p; ignore r]

let parser f = P (fun inp -> f inp)

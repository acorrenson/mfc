(**************************************************************************)
(*                                                                        *)
(*                      This file is part of MFC                          *)
(*                  it is released under MIT license.                     *)
(*                https://opensource.org/licenses/MIT                     *)
(*                                                                        *)
(*          Copyright (c) 2020 Arthur Correnson, Nathan Graule            *)
(**************************************************************************)

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

(** Parsing data *)
type 'a pdata = ('a * string)

(** Parsing result (monadic) *)
type 'a presult = 'a pdata option

(** Parser constructor *)
type 'a parser = P of (string -> 'a presult)

exception ParseException of string

(**
   Null parser.
   [parse pszero s] always returns None
*)
let pzero = P (fun _ -> None)

(**
   Empty parser.
   [parse pone s] always returns [Some ([], s)].
*)
let pone = P (fun rest -> Some([], rest))

(**
   [parse p inp] applies the parser [p] to the input string [inp].
*)
let parse p inp =
  match p with
  | P (p) -> p inp

(**
   [pmap f (parse p inp)] applies the function f 
   to result of [parse p inp] application.
*)
let pmap f: 'a presult -> 'b presult =
  function
  | Some(c, out) -> Some(f c, out)
  | None -> None

(** Try to get the result of a parser, raises [ParseException] *)
let pleak: 'a presult -> 'a =
  function
  | Some(c, _) -> c
  | None -> raise (ParseException "Attempted to get failed parse")

(** [parse (fmap f p) inp] is [parse p inp |> pmap f] *)
let fmap f p =
  P (fun inp -> parse p inp |> pmap f)

(**
   Binding operator.
   [parse (p >>= f) inp] is [parse (f r) out] if [parse p inp] retunrs [Some (r, out)].
*)
let (>>=) p f =
  P begin
    fun inp -> match parse p inp with
      | None -> None
      | Some(v, out) -> parse (f v) out
  end

(**
   Or operator.
   [p <|> q] accepts all strings accepted by [p] or by [q].
*)
let (<|>) p q =
  P (fun inp -> match parse p inp with
      | None -> parse q inp
      | _ as r -> r)

(**
   Syntax sugar for bindings.
   [let* x = p in f x] is [p >>= (fun x -> f x)] where f returns a parser.
*)
let (let*) x f = x >>= f

(**
   Alias for fmap.
*)
let (||>) p f = fmap f p

(**
   Parser for a single char [c].
*)
let pchar c =
  P (fun inp -> match inp with
      | "" -> None
      | x when x.[0] = c -> Some(c, String.sub x 1 ((String.length x) - 1))
      | _ -> None)

(**
   The parser [some p] tries to apply [p] zero or more times.
   Retunrs the result as a list.
*)
let rec some (p : 'a parser) =
  P (fun inp ->
      match parse p inp with
      | None -> Some ([], inp)
      | Some(x, rest) ->
        match parse (some p) rest with
        | Some (lx, r) -> Some (x::lx, r)
        | None -> None
    )

(**
   The parser [some p] tries to apply [p] one or more times.
   Returns the result as a list (or None).
*)
let many (p : 'a parser) =
  let* x = p in
  let* lx = some p in
  P (fun inp ->
      Some (x::lx, inp)
    )

(**
   [anychar_of cl] accepts any char in the list [cl].
*)
let any = P(fun inp -> Some(String.get inp 0, String.rest inp))

let anychar_of cl =
  match cl with
  | x::rest -> List.fold_left (<|>) (pchar x) (List.map pchar rest)
  | [] -> P (fun _ -> None)

(**
   [anychar_in s] accepts any char in string [s].
*)
let anychar_in s =
  String.explode s |> anychar_of

(**
   Convert a list of chars into a string.
*)
let combine_str ls = List.fold_left (^) "" ls

(**
   Convert a list of chars into a string.
*)
let combine lc = combine_str (List.map (String.make 1) lc)

(**
   [sequence [p1; ...; pn]] tries to apply parsers p1 to pn sequentially.
   The resulting parser returns an optional list.
*)
let rec sequence =
  function
  | [] -> pone
  | p::pl ->
    let* x = p in
    let* xs = sequence pl in
    P (fun inp -> Some(x::xs, inp))


let sequence_of_chars cl = sequence (List.map pchar cl)
let sequence_of_string s = sequence_of_chars (String.explode s)
let literal s = sequence_of_string s ||> String.combine

let wrap l r p =
  let* _ = l in
  let* v = p in
  let* _ = r in
  P (fun inp -> Some(v, inp))

(** [parse f] is [P (fun inp -> f inp)] *)
let parser f = P (fun inp -> f inp)

let trim p =
  let* _ = some (pchar ' ' <|> pchar '\n' <|> pchar '\t') in
  let* e = p in
  let* _ = some (pchar ' ' <|> pchar '\n' <|> pchar '\t') in
  P (fun inp -> Some(e, inp))

let optional p =
  P (fun inp ->
      match parse p inp with
      | None -> Some ([], inp)
      | Some (x, next) -> Some ([x], next)
    )
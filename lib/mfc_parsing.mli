type 'a parse_result
type 'a parser
type 'a t = 'a parser

val is_done : 'a parse_result -> bool
val option_of_parse : 'a parse_result option -> 'a option
val pmap : 'a -> 'b -> 'b parse_result
val pand : 'a -> 'b parse_result -> 'b parse_result
val parse_char : char -> 'a t

val parse_or : ('a t) * ('a t) -> 'a t
val parse_any : 'a t list -> 'a list t
val parse_all : 'a t list -> 'a list t
val parse_many : 'a t -> 'a list t
val parse_concat_many : string parser -> string parser
val parse_ignore : 'a parser -> 'b parser -> 'b parser
val parse_skip : 'a parser -> 'b parser -> 'b parser
val parse_concat_seq : (string t) list -> string t
val parse_combine_seq : ('a t) -> 'a t
val parse_literal : string -> string parser
val parse_anychar_in : string -> string parser
val parse_delim : 'a parser -> 'b parser -> 'b parser
val parse_wrap : 'a parser -> 'b parser -> 'c parser -> 'c parser

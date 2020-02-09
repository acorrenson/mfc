type 'a parse_result = ('a * string) option
type 'a t = string -> 'a parse_result

val is_done : 'a parse_result -> bool
val option_of_parse : 'a parse_result -> 'a option
val pmap : ('a -> 'b) -> 'a parse_result -> 'b parse_result
val pand : ('a * string -> 'b parse_result) -> 'a parse_result -> 'b parse_result
val parse_char : char -> string t
val parse_or : 'a t -> 'a t -> 'a t
val parse_any : 'a t list -> 'a t
val parse_all : 'a t list -> 'a list t
val parse_many : 'a t -> 'a list t
val parse_concat_many : string t -> string t
val parse_ignore : 'a t -> 'b t -> 'b t
val parse_skip : 'a t -> 'b t -> 'b t
val parse_concat_seq : (string t) list -> string t
val parse_combine_seq : ('a t list) -> 'a list t
val parse_literal : string -> string t
val parse_anychar_in : string -> string t
val parse_delim : 'a t -> 'b t -> ('b * 'b) t
val parse_wrap : 'a t -> 'b t -> 'c t -> 'c t

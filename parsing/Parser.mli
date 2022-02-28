open Lvca_provenance

include
  Taparse.Signatures.Parser
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type stream = Token_stream.stream
     and type 'a parser = Token_stream.stream -> 'a
     and type 'a v = 'a

type 'a t = 'a Construction.t

val cls : Char_class.t -> char t
val return : 'a -> 'a t
val mark : int t
val ranged : 'a t -> (Opt_range.t * 'a) t

(** Parser surrounded by braces (['{' _ '}']) *)
val braces : 'a t -> 'a t

(** Parser surrounded by parens (['(' _ ')']) *)
val parens : 'a t -> 'a t

(** Parser surrounded by brackest (['\[' _ '\]']) *)
val brackets : 'a t -> 'a t

val symbol : string -> token t
val keyword : string -> token t
val lower_identifier : string t
val upper_identifier : string t
val integer_lit : string t
val float_lit : string t
val integer_or_float_lit : (string, string) Base.Either.t t
val char_lit : char t
val string_lit : string t
val sep_end_by : _ t -> 'a t -> 'a list t
val sep_end_by1 : _ t -> 'a t -> 'a list t
val parse_string : 'a t -> string -> ('a, string) Base.Result.t
val parse_string_or_failwith : 'a t -> string -> 'a

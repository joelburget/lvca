open Lvca_provenance

include
  Taparse.Signatures.Parser
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type stream = Token_stream.stream
     and type 'a parser = Token_stream.stream -> 'a
     and type 'a v = 'a

type 'a t = 'a Construction.t

val return : 'a -> 'a t

(** Parser surrounded by braces (['{' _ '}']) *)
val braces : 'a t -> (Opt_range.t * 'a) t

(** Parser surrounded by parens (['(' _ ')']) *)
val parens : 'a t -> (Opt_range.t * 'a) t

(** Parser surrounded by brackest (['\[' _ '\]']) *)
val brackets : 'a t -> (Opt_range.t * 'a) t

val symbol : string -> (Opt_range.t * string) t
val keyword : string -> token t
val lower_identifier : (Opt_range.t * string) t
val upper_identifier : (Opt_range.t * string) t
val integer_lit : (Opt_range.t * string) t
val float_lit : (Opt_range.t * string) t
val integer_or_float_lit : (Opt_range.t * (string, string) Base.Either.t) t
val char_lit : (Opt_range.t * char) t
val string_lit : (Opt_range.t * string) t
val regex_lit : (Opt_range.t * string) t
val sep_end_by : _ t -> 'a t -> 'a list t
val sep_end_by1 : _ t -> 'a t -> 'a list t
val parse_string : 'a t -> string -> ('a, string) Base.Result.t
val parse_string_or_failwith : 'a t -> string -> 'a
val aggregate_ranges : (Opt_range.t * 'a) list -> Opt_range.t * 'a list
val debug_print_tokens : bool ref

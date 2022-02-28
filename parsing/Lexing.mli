open Lvca_provenance

include
  Taparse.Signatures.Parser
    with type token = Char_stream.token
     and type token_tag = char
     and type stream = Char_stream.stream
     and type 'a parser = Char_stream.stream -> 'a
     and type 'a v = 'a

type 'a t = ('a * Opt_range.t) Construction.t

val char : char -> char t
val string : string -> string t
val lower_identifier : Lvca_util.String.Set.t -> string t
val upper_identifier : Lvca_util.String.Set.t -> string t

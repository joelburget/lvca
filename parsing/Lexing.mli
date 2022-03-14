open Lvca_provenance

include
  Taparse.Signatures.Parser
    with type token = Char_stream.token
     and type token_tag = char
     and type token_set = Char_class.t
     and type stream = Char_stream.stream
     and type 'a parser = Char_stream.stream -> 'a
     and type 'a v = 'a

type 'a t = ('a * Opt_range.t) Construction.t

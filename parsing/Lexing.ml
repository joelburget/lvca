module Parser :
  Taparse.Signatures.Parser
    with type token = Char_stream.token
     and type token_tag = Char_stream.token_tag
     and type stream = Char_stream.stream
     and type 'a v = 'a
     and type 'a parser = Char_stream.stream -> 'a =
  Taparse.Unstaged.Make (Char_stream)

include Parser

type 'a t = ('a * Lvca_provenance.Opt_range.t) Construction.t

let char = failwith "TODO"
let string = failwith "TODO"
let lower_identifier = failwith "TODO"
let upper_identifier = failwith "TODO"

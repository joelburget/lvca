type token = char * int
type stream

val make_stream : string -> stream

include
  Taparse.Signatures.Token_stream
    with type token := token
     and type token_tag = char
     and type token_set = Char_class.t
     and type stream := stream

exception Lexer_err of int

(** Raises {!Lexer_err} *)
val lex_exn : string -> Token_stream.Token.t list

val lex : string -> (Token_stream.Token.t list, int) Result.t

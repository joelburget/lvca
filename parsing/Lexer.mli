type lex_error_info = Token_stream.Token_tag.t Taparse_regex.Lexing.t * int

exception Lexer_err of lex_error_info

(** Raises {!Lexer_err} *)
val lex_exn : string -> Token_stream.Token.t list

val lex : string -> (Token_stream.Token.t list, lex_error_info) Result.t

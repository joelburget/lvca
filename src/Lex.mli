type token =
  { name : string
  ; start : int (* inclusive *)
  ; finish : int (* exclusive *)
  }

type token_name = string
type lexer = (token_name * Regex.t) list
type position = int

type lex_error =
  { start_pos : position
  ; end_pos : position
  ; message : string
  }

type lexbuf =
  { buf : string
  ; mutable pos : position
  }

exception LexError of lex_error

val string_of_tokens : token array -> string
val lex : lexer -> string -> (token array, lex_error) Result.t

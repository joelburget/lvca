type token_name = string
type regex = string
type lexer = (regex * token_name) list

type token =
  { name : token_name
  ; start : int
  ; (* inclusive *)
    finish : int (* exclusive *)
  }

type position = int

type lex_error =
  { start_pos : position
  ; end_pos : position
  ; message : string
  }

val lex : lexer -> string -> (lex_error, token array) Tablecloth.Result.t
val string_of_tokens : token array -> string

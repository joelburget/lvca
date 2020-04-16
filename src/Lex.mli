type token =
  { name : string
  ; start : int (* inclusive *)
  ; finish : int (* exclusive *)
  }

type token_name = string
type lexer = (token_name * Regex.t) list

type lexbuf =
  { buf : string
  ; mutable lnum : int
  (** The current line number (= number of newlines we've crossed) *)
  ; mutable bol_pos : int
  (** The character number of the beginning of the current line *)
  ; mutable abs_pos : int
  (** The absolute position in the buffer of the current position *)
  }

val string_of_tokens : token array -> string
val lex : lexer -> string -> (token array, LexerUtil.lex_error) Result.t

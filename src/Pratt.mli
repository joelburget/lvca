type token =
  { name : string
  ; content : string
  ; lbp : int
  }

(* TODO: move lbp from lexer to parser? *)
type token_rule =
  { name : string
  ; re : Re.t
  ; lbp : int option
  }
type lexer = token_rule list

type parse_result = (Binding.Nominal.term, [ `ParseError of string ]) Result.t

type expression_parser = ?rbp:int -> token list -> token list * parse_result

type led_parser =
  expression_parser -> Binding.Nominal.term -> token -> token list -> token list * parse_result
type nud_parser =
  expression_parser ->                 token -> token list -> token list * parse_result

type token_parser =
  { nud : nud_parser option
  ; led : led_parser option
  }

type parser = token_parser Util.String.Map.t

val lex_and_parse
  : parser -> lexer -> string -> (Binding.Nominal.term, [> `ParseError of string | `LexError of string ]) Result.t

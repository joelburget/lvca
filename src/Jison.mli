type parser

type js_lex = {
  rules: (string * string) array;
} [@@bs.deriving abstract]

type grammar = {
  lex: js_lex;
  bnf: string array array Js.Dict.t;
} [@@bs.deriving abstract]

val to_parser : grammar -> parser

type parser

type js_lex = {
  rules: (string * string) array;
} [@@bs.deriving abstract]

type operators = string array array

type grammar = {
  lex       : js_lex;
  operators : operators;
  bnf       : (string * string) array Js.Dict.t;
} [@@bs.deriving abstract]

val to_parser : grammar -> parser

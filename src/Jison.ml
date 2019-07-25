module M = Belt.Map.String

type jison

external jison : jison = "jison" [@@bs.module]

type parser

type js_lex = {
  rules: (string * string) array;
} [@@bs.deriving abstract]

type operators = string array array

type grammar = {
  lex       : js_lex;
  operators : operators;
  bnf       : string array array Js.Dict.t;
} [@@bs.deriving abstract]

let to_parser (grammar: grammar) : parser =
  ([%raw "function(jison, grammar) { /* console.log(JSON.stringify(grammar, null, 4)); */ return new jison.Parser(grammar); }"]
  : jison -> grammar -> parser
  ) jison grammar

open Types
open ConcreteSyntaxDescription

module M = Belt.Map.String

type jison

external jison : jison = "jison" [@@bs.module]

type parser

type js_lex = {
  rules: string list list;
} [@@bs.deriving abstract]

type grammar = {
  lex: js_lex;
  bnf: string list list Js.Dict.t;
} [@@bs.deriving abstract]

let to_grammar ({terminal_rules; sort_rules}: ConcreteSyntaxDescription.t)
  : grammar
  =
    let lex' = js_lex ~rules:
      (List.map
        (fun (name, regex) -> [name; "return " ^ regex])
        (M.toList terminal_rules))
    in
    let rec print_tokens = function
      | [] -> ""
      | tok :: toks -> (match tok with
        | TerminalName    name -> name
        | NonterminalName name -> name
      ) ^ " " ^ print_tokens toks
    in
    let mk_variable: variable_rule option -> string list = function
      | None
      -> []
      | Some { tokens; var_capture }
      -> [print_tokens tokens; Printf.sprintf "$$ = %i" var_capture]
    in
    let mk_operator_rules: operator_match list -> string list list
      = List.map (fun (OperatorMatch { tokens; term_pattern }) ->
          let operator_name, term_scopes = term_pattern in
          [ print_tokens tokens;
            (* TODO: conversion from this format to a term *)
            Printf.sprintf
            "$$ = { name: %s, scopes: %s }"
            operator_name
            "TODO"
          ]
        )
    in
    let mk_sort_rule = fun (sort_name, SortRule { operator_rules; variable }) ->
      (sort_name, mk_variable variable :: mk_operator_rules operator_rules)
    in
    let bnf = Js.Dict.fromList (List.map mk_sort_rule (M.toList sort_rules)) in
    grammar ~lex:lex' ~bnf:bnf

let to_parser (grammar: grammar) : parser =
  ([%raw "function(jison, grammar) { return new jison.Parser(grammar); }"]
  : jison -> grammar -> parser
  ) jison grammar

(* XXX: need to fix return value *)
let parse (parser: parser) (str: string) : ConcreteSyntax.tree =
  ([%raw "function(parser, str) { return parser.parse(str); }"]
  : parser -> string -> ConcreteSyntax.tree
  ) parser str

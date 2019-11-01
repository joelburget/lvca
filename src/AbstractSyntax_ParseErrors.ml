
(* This file was auto-generated based on "src/AbstractSyntax_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Unexpected \";\". Expected a set of sort definitions.\n"
    | 1 ->
        "Unexpected \";\". Expected \":=\".\n"
    | 2 ->
        "Unexpected \";\" when parsing a sort definition. Expected an operator definition or \"|\".\n"
    | 5 ->
        "Unexpected \";\" when parsing an operator definition. Expecte an arity.\n"
    | 6 ->
        "Unexpected \";\" when parsing an arity. Expected a list of valences.\n"
    | 42 ->
        "Unexpected \";\" when parsing a list of operator definitions. Expected an identifier or \"|\".\n"
    | 43 ->
        "Unexpected \";\" when parsing a list of operator definitions. Expected an operator definition.\n"
    | 7 ->
        "Unexpected \";\" when parsing a sort. Expected a sort or \")\".\n"
    | 13 ->
        "Unexpected \";\". Expected \"(\" (parsing a sort).\n"
    | 17 ->
        "Sort expected (got \";\").\n"
    | 31 ->
        "Identifier expected or \"]\" expected.\n"
    | 32 ->
        "Comma-separated list of identifiers (followed by a bracket) expected.\n"
    | 36 ->
        "Expected a paren-enclosed list of valences.\n"
    | 37 ->
        "Expected a valence.\n"
    | 33 ->
        "Identifier expected.\n"
    | 4 ->
        "Malformed sort declaration -- expected an operator, eg `add(arith; arith)`\n"
    | _ ->
        raise Not_found

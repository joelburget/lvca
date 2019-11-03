
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
        "Unexpected \";\" when parsing an operator definition. Expected an arity.\n"
    | 6 ->
        "Unexpected \";\" when parsing an arity. Expected a list of valences.\n"
    | 42 ->
        "Unexpected \";\" when parsing a list of operator definitions. Expected an identifier or \"|\".\n"
    | 43 ->
        "Unexpected \";\" when parsing a list of operator definitions. Expected an operator definition.\n"
    | 7 ->
        "Unexpected \";\" when parsing a sort.\n"
    | 13 ->
        "Unexpected \";\" when parsing a sort. Expected \")\".\n"
    | 8 ->
        "Unexpected \"[\" when parsing a sort.\n"
    | 23 ->
        "Unexpected \";\" when parsing a list of valences. Expected \")\" or a valence.\n"
    | 16 ->
        "Unexpected \"]\".\n"
    | 17 ->
        "Sort expected (got \";\") when parsing a valence.\n"
    | 18 ->
        "Unexpected \";\" when parsing a valence. Expected \"[\".\n"
    | 22 ->
        "Unexpected \"]\" when parsing a list of valences. Expected \")\" or \";\".\n"
    | 24 ->
        "Unexpected \"[\" when parsing a list of sorts.\n"
    | 11 ->
        "Unexpected \"[\" when parsing a list of sorts. Expected \";\" or a sort.\n"
    | 25 ->
        "Unexpected \";\" when parsing a list of sorts. Expected a sort or \")\".\n"
    | 31 ->
        "Identifier expected or \"]\" expected.\n"
    | 32 ->
        "Comma-separated list of identifiers (followed by a bracket) expected.\n"
    | 36 ->
        "Expected a paren-enclosed list of valences when parsing an arity.\n"
    | 37 ->
        "Expected a valence when parsing an arity.\n"
    | 33 ->
        "Identifier expected when parsing a list of IDs.\n"
    | 4 ->
        "Malformed sort declaration -- expected an operator, eg `add(arith; arith)`\n"
    | _ ->
        raise Not_found

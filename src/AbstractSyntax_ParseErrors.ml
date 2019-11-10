
(* This file was auto-generated based on "src/AbstractSyntax_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Malformed language definition. Expected a set of sort definitions.\n"
    | 1 ->
        "Malformed import -- expected a set of identifiers, eg \"{list, string}\".\n"
    | 2 ->
        "Malformed import -- expected an identifier list.\n"
    | 3 ->
        "Comma-separated list of identifiers (followed by a bracket) expected.\n"
    | 6 ->
        "Malformed import -- expected \"}\".\n"
    | 7 ->
        "Malformed import -- expected \"from\".\n"
    | 57 ->
        "Malformed import list -- expected another import or the first sort declaration.\n"
    | 8 ->
        "Malformed import -- expected a string after \"from\".\n"
    | 4 ->
        "Identifier expected when parsing a list of IDs.\n"
    | 11 ->
        "Malformed language definition. Expected \":=\".\n"
    | 12 ->
        "Malformed sort definition. Expected an operator definition or \"|\".\n"
    | 15 ->
        "Malformed operator definition. Expected an arity.\n"
    | 16 ->
        "Malformed arity. Expected a list of valences.\n"
    | 49 ->
        "Malformed list of operator definitions. Expected an identifier, \"|\", or EOF.\n"
    | 50 ->
        "Malformed list of operator definitions. Expected an operator definition.\n"
    | 17 ->
        "Malformed sort.\n"
    | 18 ->
        "Unexpected \"[\" when parsing a sort.\n"
    | 23 ->
        "Malformed sort. Expected \")\".\n"
    | 26 ->
        "Unexpected \"]\".\n"
    | 33 ->
        "Malformed valence list. Expected a valence or list terminator.\n"
    | 27 ->
        "Malformed variable valence. Expected a sort.\n"
    | 28 ->
        "Malformed variable valence. Expected \"]\".\n"
    | 32 ->
        "Unexpected \"]\" when parsing a list of valences. Expected \")\" or \";\".\n"
    | 21 ->
        "Unexpected \"[\" when parsing a list of sorts. Expected \";\" or a sort.\n"
    | 34 ->
        "Unexpected \"[\" when parsing a list of sorts.\n"
    | 35 ->
        "Malformed list of sorts.\n"
    | 41 ->
        "Identifier expected or \"]\" expected.\n"
    | 43 ->
        "Expected a paren-enclosed list of valences when parsing an arity.\n"
    | 44 ->
        "Expected a valence when parsing an arity.\n"
    | 42 ->
        "Malformed arity -- expected a \"]\" after a list of identifiers.\n"
    | 14 ->
        "Malformed sort declaration -- expected an operator, eg `add(arith; arith)`\n"
    | _ ->
        raise Not_found


(* This file was auto-generated based on "AbstractSyntax_Parser.messages". *)

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
    | 7 ->
        "Malformed import -- expected \"from\".\n"
    | 67 ->
        "Malformed import list -- expected another import or the first sort declaration.\n"
    | 8 ->
        "Malformed import -- expected a string after \"from\".\n"
    | 11 ->
        "Identifier expected when parsing a list of IDs.\n"
    | 14 ->
        "Malformed language definition. Expected \":=\".\n"
    | 18 ->
        "Malformed sort definition. Expected an operator definition or \"|\".\n"
    | 21 ->
        "Malformed operator definition. Expected an arity.\n"
    | 22 ->
        "Malformed arity. Expected a list of valences.\n"
    | 59 ->
        "Malformed list of operator definitions. Expected an identifier, \"|\", or EOF.\n"
    | 60 ->
        "Malformed list of operator definitions. Expected an operator definition.\n"
    | 23 ->
        "Malformed sort.\n"
    | 24 ->
        "Unexpected \"[\" when parsing a sort.\n"
    | 29 ->
        "Malformed sort. Expected \")\".\n"
    | 32 ->
        "Unexpected \"]\".\n"
    | 39 ->
        "Malformed valence list. Expected a valence or list terminator.\n"
    | 33 ->
        "Malformed variable valence. Expected a sort.\n"
    | 34 ->
        "Malformed variable valence. Expected \"]\".\n"
    | 38 ->
        "Unexpected \"]\" when parsing a list of valences. Expected \")\" or \";\".\n"
    | 27 ->
        "Unexpected \"[\" when parsing a list of sorts. Expected \";\" or a sort.\n"
    | 40 ->
        "Unexpected \"[\" when parsing a list of sorts.\n"
    | 41 ->
        "Malformed list of sorts.\n"
    | 47 ->
        "Identifier expected or \"]\" expected.\n"
    | 53 ->
        "Expected a paren-enclosed list of valences when parsing an arity.\n"
    | 54 ->
        "Expected a valence when parsing an arity.\n"
    | 48 ->
        "Malformed arity -- expected a \"]\" after a list of identifiers.\n"
    | 20 ->
        "Malformed sort declaration -- expected an operator, eg `add(arith; arith)`\n"
    | _ ->
        raise Not_found

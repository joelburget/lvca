
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
        "Comma-separated list of import symbols (followed by a \"}\") expected.\n"
    | 7 ->
        "Malformed import -- expected \"from\".\n"
    | 73 ->
        "Malformed import list -- expected another import or the first sort declaration.\n"
    | 8 ->
        "Malformed import -- expected a string after \"from\".\n"
    | 11 ->
        "Expected a list of symbols to import.\n"
    | 4 ->
        "Expected an identifier after \"as\".\n"
    | 10 ->
        "Expected a comma-separated list of symbols to import.\n"
    | 14 ->
        "Expected a sort declaration.\n"
    | 65 ->
        "Expected a \"|\"-separated list of operator definitions.\n"
    | 25 ->
        "Malformed operator definition. Expected an arity.\n"
    | 26 ->
        "Malformed arity. Expected a list of valences.\n"
    | 62 ->
        "Malformed list of operator definitions. Expected an identifier, \"|\", or EOF.\n"
    | 63 ->
        "Malformed list of operator definitions. Expected an operator definition.\n"
    | 27 ->
        "Unexpected \"]\".\n"
    | 15 ->
        "Unexpected token in sort definition variable list.\n"
    | 21 ->
        "Unexpected token in sort definition. Expected `:=`.\n"
    | 22 ->
        "Unexpected token in sort definition. Expected an operator definition.\n"
    | 24 ->
        "Unexpected token in sort definition. Expected an operator definition.\n"
    | 16 ->
        "Unexpected token in `;`-separated identifier list.\n"
    | 17 ->
        "Unexpected token in `;`-separated identifier list.\n"
    | 40 ->
        "Parsing a valence, after `*`, expected `.`.\n"
    | 41 ->
        "Parsing a valence, after `*.`, expected a sort`.\n"
    | 37 ->
        "As part of a valence list, expected `(` or `;`.\n"
    | 38 ->
        "Malformed valence list. Expected a valence or list terminator.\n"
    | 28 ->
        "After `(`, expected a (possibly empty) list of sorts.\n"
    | 39 ->
        "As part of a list of sorts, expected `;` or `.`.\n"
    | 29 ->
        "As part of a list of sorts, expected `(` or `;`.\n"
    | 30 ->
        "Expected a `;`-separated list of sorts.\n"
    | 44 ->
        "Expected a `.` or `;` separating sorts.\n"
    | 43 ->
        "Malformed list of sorts.\n"
    | 50 ->
        "Identifier expected or \"]\" expected.\n"
    | 56 ->
        "Expected a paren-enclosed list of valences when parsing an arity.\n"
    | 57 ->
        "Expected a valence when parsing an arity.\n"
    | 51 ->
        "Expected a comma-separated list of ids, followed by \"]\".\n"
    | 52 ->
        "Expected a comma-separated list of ids.\n"
    | 66 ->
        "Malformed sort declaration -- expected an operator, eg `add(arith; arith)`\n"
    | _ ->
        raise Not_found

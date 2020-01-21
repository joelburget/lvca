
(* This file was auto-generated based on "src/ConcreteSyntax_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 44 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 17>\n"
    | 45 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 16>\n"
    | 46 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 15>\n"
    | 47 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 14>\n"
    | 48 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 13>\n"
    | 20 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 12>\n"
    | 14 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 11>\n"
    | 15 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 10>\n"
    | 35 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 9>\n"
    | 31 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 8>\n"
    | 24 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE 7>\n"
    | 23 ->
        "Malformed sort.\n"
    | 37 ->
        "Expected an operator matching rule: a list of tokens followed by the term or pattern it corresponds to.\n"
    | 28 ->
        "Malformed sort.\n"
    | 30 ->
        "Parsing type signature, expected either `->` and then another sort, or `:=` followed by operator declarations.\n"
    | 21 ->
        "After nonterminal name, expected either a type signature or `:=` followed by operator declarations.\n"
    | 13 ->
        "After ':', expected a type signature.\n"
    | 0 ->
        "Expected a capture number, eg $1.\n"
    | 1 ->
        "Expected a capture number, eg $1.\n"
    | 4 ->
        "Expected a language (starting with terminals).\n"
    | 5 ->
        "Expected \":=\" after terminal id.\n"
    | 6 ->
        "Expected a regex or string.\n"
    | 9 ->
        "Expected more terminal rules or a nonterminal rule.\n"
    | 100 ->
        "Expected EOF after nonterminal token.\n"
    | 38 ->
        "Expected nonterminal token.\n"
    | 98 ->
        "Expected nonterminal token.\n"
    | 62 ->
        "Expected operator match pattern after '{'.\n"
    | 63 ->
        "Expected '(' after operator name.\n"
    | 64 ->
        "Expected a term scope pattern or ')' after '('.\n"
    | 71 ->
        "Expected a ';', ')', or '.' after a capture number.\n"
    | 68 ->
        "Expected a term scope pattern.\n"
    | 72 ->
        "Expected capture number.\n"
    | 65 ->
        "Expected token number, eg $1.\n"
    | 78 ->
        "Expected '}' after operator match pattern.\n"
    | 79 ->
        "Expected optional fixity then EOF after operator match.\n"
    | 108 ->
        "Expected EOF after operator match.\n"
    | 59 ->
        "Expected more nonterminal tokens or '{'.\n"
    | 106 ->
        "Expected operator match.\n"
    | 94 ->
        "Expected nonterminal rule.\n"
    | 12 ->
        "Expected \":=\" after identifier in nonterminal rule.\n"
    | 96 ->
        "Expected EOF after nonterminal rule.\n"
    | 56 ->
        "Expected EOF or ('|' or '>'), then more operator matches.\n"
    | 57 ->
        "Expected a set of operator matches.\n"
    | 84 ->
        "Expected a set of operator matches.\n"
    | 86 ->
        "Expected a set of operator matches.\n"
    | 87 ->
        "Expected a set of operator matches.\n"
    | 114 ->
        "Expected terminal rule.\n"
    | 116 ->
        "Expected EOF after terminal rule.\n"
    | _ ->
        raise Not_found

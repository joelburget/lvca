
(* This file was auto-generated based on "src/ConcreteSyntax_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
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
    | 59 ->
        "Expected EOF after nonterminal token.\n"
    | 16 ->
        "Expected nonterminal token.\n"
    | 57 ->
        "Expected nonterminal token.\n"
    | 28 ->
        "Expected operator match pattern after '{'.\n"
    | 29 ->
        "Expected '(' after operator name.\n"
    | 30 ->
        "Expected a term scope pattern or ')' after '('.\n"
    | 37 ->
        "Expected a ';', ')', or '.' after a capture number.\n"
    | 34 ->
        "Expected a term scope pattern.\n"
    | 38 ->
        "Expected capture number.\n"
    | 31 ->
        "Expected token number, eg $1.\n"
    | 43 ->
        "Expected '}' after operator match pattern.\n"
    | 44 ->
        "Expected optional fixity then EOF after operator match.\n"
    | 63 ->
        "Expected EOF after operator match.\n"
    | 25 ->
        "Expected more nonterminal tokens or '{'.\n"
    | 61 ->
        "Expected operator match.\n"
    | 65 ->
        "Expected sort rule.\n"
    | 12 ->
        "Expected \":=\" after identifier in sort rule.\n"
    | 67 ->
        "Expected EOF after sort rule.\n"
    | 22 ->
        "Expected EOF or ('|' or '>'), then more operator matches.\n"
    | 23 ->
        "Expected a set of operator matches.\n"
    | 50 ->
        "Expected a set of operator matches.\n"
    | 13 ->
        "Expected a set of operator matches.\n"
    | 15 ->
        "Expected a set of operator matches.\n"
    | 69 ->
        "Expected terminal rule.\n"
    | 71 ->
        "Expected EOF after terminal rule.\n"
    | _ ->
        raise Not_found

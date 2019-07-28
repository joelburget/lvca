
(* This file was auto-generated based on "LanguageParser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 1 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 2 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 5 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 6 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 41 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 42 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 7 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 8 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 22 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 23 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 24 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 9 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 11 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 10 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 16 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 17 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 21 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE>\n"
    | 30 ->
        "Identifier expected.\n"
    | 31 ->
        "Comma-separated list of identifiers (followed by a bracket) expected.\n"
    | 35 ->
        "Expected a paren-enclosed list of valences.\n"
    | 36 ->
        "Expected a valence.\n"
    | 32 ->
        "Identifier expected.\n"
    | 4 ->
        "Malformed sort declaration -- expected an operator, eg `add(arith; arith)`\n"
    | _ ->
        raise Not_found

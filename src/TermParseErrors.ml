
(* This file was auto-generated based on "TermParser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 24 ->
        "Unexpected token. Expected end of file.\n"
    | 0 ->
        "Unexpected token. Expected term.\n"
    | 17 ->
        "Unexpected token after term. Expected `]` or `;`.\n"
    | 18 ->
        "Unexpected token after semicolon. Expected list of terms.\n"
    | 2 ->
        "Unexpected token after left bracket. Expected (semicolon-separated) list of terms.\n"
    | 4 ->
        "Unexpected token after identifier. Expected left paren or semicolon.\n"
    | 12 ->
        "Unexpected token. Missing right paren?\n"
    | 13 ->
        "Unexpected symbol after semicolon.\n"
    | 5 ->
        "Unexpected token after opening paren. Expected a variable name or term.\n"
    | 6 ->
        "Unexpected token after identifier. Expected `.`, `(`, or `;`.\n"
    | 7 ->
        "Missing body after binding. Example of a binding and use: `lam(x. x)`.\n"
    | _ ->
        raise Not_found

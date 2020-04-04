
(* This file was auto-generated based on "Statics_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Invalid token. Expected a list of rules.\n"
    | 26 ->
        "Invalid token after line separator. Expected a conclusion.\n"
    | 23 ->
        "Invalid token after rule. Expected another rule or EOF.\n"
    | 1 ->
        "Invalid token after `ctx`. Expected `,` or `>>`.\n"
    | 29 ->
        "Invalid token after `>>`. Expected either an inference or checking rule.\n"
    | 31 ->
        "Invalid token after term. Expected `<=` or `=>`.\n"
    | 32 ->
        "Invalid token after `=>`. Expected a term.\n"
    | 34 ->
        "Invalid token after `<=`. Expected a term.\n"
    | 38 ->
        "Invalid token after hypothesis. Expected either another hypothesis or line separator.\n"
    | 2 ->
        "Invalid token after `,`. Expected a list of typed terms.\n"
    | 3 ->
        "Invalid token after ID. Expected a `:`.\n"
    | 4 ->
        "Invalid token after `:`. Expected a type (term).\n"
    | 18 ->
        "Invalid token after term typing in context.\n"
    | 19 ->
        "Invalid token after `,` in context. Expected a typed term.\n"
    | 42 ->
        "Invalid token, expected a term.\n"
    | 44 ->
        "Invalid token after term.\n"
    | 6 ->
        "Invalid token after `(`. Expected subterms followed by `)`..\n"
    | 13 ->
        "Invalid token after term. Expected a scope.\n"
    | 7 ->
        "Invalid token after term.\n"
    | 8 ->
        "Invalid term after `.`.\n"
    | 5 ->
        "Invalid term. Terms look like `tm(...)` and variables like `x`.\n"
    | _ ->
        raise Not_found


(* This file was auto-generated based on "Core_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 75 ->
        "Unexpected token. Expected a term.\n"
    | 77 ->
        "Unexpected token after term. Expected end of file.\n"
    | 45 ->
        "Unexpected token after term.\n"
    | 5 ->
        "Unexpected token after operator id. Expected a list of subterms, followed by `)`.\n"
    | 22 ->
        "Unexpected token. Expected `.` or `;`.\n"
    | 20 ->
        "Unexpected token after `;`. Expected another term.\n"
    | 23 ->
        "Unexpected token after `.`.\n"
    | 27 ->
        "Unexpected token after `match`. Expected a term.\n"
    | 55 ->
        "Unexpected token after `with`. Expected `{`.\n"
    | 56 ->
        "Unexpected token after `{`. Expected a list of pattern matches.\n"
    | 61 ->
        "Unexpected token after pattenr. Expected `->`.\n"
    | 62 ->
        "Unexpected token after `->`. Expected a term.\n"
    | 64 ->
        "Unexpected token. Expected `|` (beginning another line) or `}` (ending the match).\n"
    | 65 ->
        "Unexpected token after `|`. Expected a pattern.\n"
    | 58 ->
        "Unexpected token in match. Expected a list of cases, each starting with `|`.\n"
    | 54 ->
        "Unexpected token after term. Expected `with`.\n"
    | 28 ->
        "Unexpected token after `let`. Expected variable declaration.\n"
    | 31 ->
        "Unexpected token after variable. Expected `=`.\n"
    | 32 ->
        "Unexpected token after `=`. Expected term.\n"
    | 51 ->
        "Unexpected token after term. Expected `in`.\n"
    | 52 ->
        "Unexpected token after `in`. Expected term.\n"
    | 33 ->
        "Unexpected token after `(`. Expected term.\n"
    | 49 ->
        "Unexpected token after term. Expected `)`.\n"
    | 6 ->
        "Unexpected token after `[`.\n"
    | 12 ->
        "Unexpected token in list. Expected `[` or `,`.\n"
    | 13 ->
        "Unexpected token after `,`.\n"
    | 34 ->
        "Unexpected token after `\\`. Expected an argument list.\n"
    | 35 ->
        "Unexpected token after `(`. Expected variable declaration.\n"
    | 36 ->
        "Unexpected token after variable declaration. Expected `:`.\n"
    | 37 ->
        "Unexpected token after `:`. Expected sort.\n"
    | 38 ->
        "Unexpected token after sort. Expected `)`.\n"
    | 41 ->
        "Unexpected token after argument. Expected another argument or `->`.\n"
    | 42 ->
        "Unexpected token after `->`. Expected a term.\n"
    | 0 ->
        "Unexpected token. Expected definition.\n"
    | 1 ->
        "Unexpected token after variable name.\n"
    | _ ->
        raise Not_found

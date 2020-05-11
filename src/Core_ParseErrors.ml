
(* This file was auto-generated based on "src/Dynamics_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Unexpected token. Expected a term.\n"
    | 65 ->
        "Unexpected token after term. Expected end of file.\n"
    | 41 ->
        "Unexpected token after term.\n"
    | 3 ->
        "Unexpected token after operator id. Expected a list of subterms, followed by `)`.\n"
    | 20 ->
        "Unexpected token. Expected `.` or `;`.\n"
    | 18 ->
        "Unexpected token after `;`. Expected another term.\n"
    | 21 ->
        "Unexpected token after `.`.\n"
    | 23 ->
        "Unexpected token after `match`. Expected a term.\n"
    | 51 ->
        "Unexpected token after `with`. Expected `{`.\n"
    | 52 ->
        "Unexpected token after `{`. Expected a list of pattern matches.\n"
    | 57 ->
        "Unexpected token after pattenr. Expected `->`.\n"
    | 58 ->
        "Unexpected token after `->`. Expected a term.\n"
    | 60 ->
        "Unexpected token. Expected `|` (beginning another line) or `}` (ending the match).\n"
    | 61 ->
        "Unexpected token after `|`. Expected a pattern.\n"
    | 54 ->
        "Unexpected token in match. Expected a list of cases, each starting with `|`.\n"
    | 50 ->
        "Unexpected token after term. Expected `with`.\n"
    | 24 ->
        "Unexpected token after `let`. Expected variable declaration.\n"
    | 25 ->
        "Unexpected token after variable. Expected `=`.\n"
    | 26 ->
        "Unexpected token after `=`. Expected term.\n"
    | 47 ->
        "Unexpected token after term. Expected `in`.\n"
    | 48 ->
        "Unexpected token after `in`. Expected term.\n"
    | 27 ->
        "Unexpected token after `(`. Expected term.\n"
    | 45 ->
        "Unexpected token after term. Expected `)`.\n"
    | 4 ->
        "Unexpected token after `[`.\n"
    | 10 ->
        "Unexpected token in list. Expected `[` or `,`.\n"
    | 11 ->
        "Unexpected token after `,`.\n"
    | 28 ->
        "Unexpected token after `\\`. Expected an argument list.\n"
    | 29 ->
        "Unexpected token after `(`. Expected variable declaration.\n"
    | 30 ->
        "Unexpected token after variable declaration. Expected `:`.\n"
    | 31 ->
        "Unexpected token after `:`. Expected sort.\n"
    | 32 ->
        "Unexpected token after sort. Expected `)`.\n"
    | 35 ->
        "Unexpected token after argument. Expected another argument or `->`.\n"
    | 38 ->
        "Unexpected token after `->`. Expected a term.\n"
    | 67 ->
        "Unexpected token. Expected definition.\n"
    | 68 ->
        "Unexpected token after variable name.\n"
    | 69 ->
        "Unexpected token after `=`. Expected definition.\n"
    | 70 ->
        "Unexpected token after definition (expected `;`).\n"
    | 75 ->
        "Unexpected token after definition.\n"
    | _ ->
        raise Not_found

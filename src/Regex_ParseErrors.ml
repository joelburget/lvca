
(* This file was auto-generated based on "Regex_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Invalid regex.\n"
    | 50 ->
        "Unexpected character after regex.\n"
    | 32 ->
        "Unexpected character after regex.\n"
    | 36 ->
        "Expected count after `{`.\n"
    | 37 ->
        "Expected `}` after count.\n"
    | 44 ->
        "Expected a valid regex after `|`.\n"
    | 3 ->
        "Expected a valid regex after `(`.\n"
    | 47 ->
        "Expected a `)` after regex.\n"
    | 4 ->
        "Invalid character set element.\n"
    | 21 ->
        "Unexpected character set.\n"
    | 22 ->
        "Unexpected character after `-`.\n"
    | 19 ->
        "Invalid character set element.\n"
    | 5 ->
        "Unexpected character after `\\`.\n"
    | 27 ->
        "Unexpected character after `\\`.\n"
    | _ ->
        raise Not_found

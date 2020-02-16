
(* This file was auto-generated based on "src/Regex_Parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Unexpected token. Expected regex.\n"
    | 1 ->
        "Unexpected token. Expected regex.\n"
    | 15 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE TODO 2>\n"
    | 12 ->
        "<YOUR SYNTAX ERROR MESSAGE HERE TODO>\n"
    | 21 ->
        "Unexpected token after regex.\n"
    | 17 ->
        "Unexpected token after \"|\".\n"
    | 22 ->
        "Unexpected token. Expected regex.\n"
    | 26 ->
        "Unexpected token after regex.\n"
    | _ ->
        raise Not_found

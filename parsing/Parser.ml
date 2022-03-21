open Base
open Lvca_provenance

module Parser :
  Taparse.Signatures.Parser
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type token_set =
      (Token_stream.token_tag, Token_stream.Token_tag.comparator_witness) Set.t
     and type stream = Token_stream.stream
     and type 'a v = 'a
     and type 'a parser = Token_stream.stream -> 'a =
  Taparse.Unstaged.Make (Token_stream)

include Parser
open Construction
open Token_stream

let map2 = Lvca_util.Tuple2.map2

type 'a t = 'a Construction.t

let singleton = Set.singleton (module Token_stream.Token_tag)
let tok' tag = tok (singleton tag)
let extract_token_content (_, lit, range) = range, lit
let symbol str = tok' (Token_tag.Symbol str) >>| extract_token_content
let keyword str = tok' (Token_tag.Keyword str)

let bracketed_parser open_ close p =
  let+ range1, _ = symbol open_
  and+ p
  and+ range2, _ = symbol close in
  Opt_range.union range1 range2, p
;;

let braces p = bracketed_parser "{" "}" p
let parens p = bracketed_parser "(" ")" p
let brackets p = bracketed_parser "[" "]" p
let lower_identifier = tok' Token_tag.Lower_ident >>| extract_token_content
let upper_identifier = tok' Token_tag.Upper_ident >>| extract_token_content
let sep_end_by sep p = sep_by sep p <* option sep
let sep_end_by1 sep p = sep_by1 sep p <* option sep
let return = eps
let integer_lit = tok' (Token_tag.Literal Integer) >>| extract_token_content
let float_lit = tok' (Token_tag.Literal Floating) >>| extract_token_content

let integer_or_float_lit =
  choice
    ~failure_msg:"integer_or_float_lit"
    [ integer_lit >>| map2 ~f:Base.Either.first
    ; float_lit >>| map2 ~f:Base.Either.second
    ]
;;

let char_lit =
  let extract_char_content tok =
    let range, str = extract_token_content tok in
    if String.length str = 3
    then range, String.unsafe_get str 1
    else failwith "TODO: char_lit error"
  in
  tok' (Token_tag.Literal Single_quoted) >>| extract_char_content
;;

let string_lit =
  let extract_string_content tok =
    let range, str = extract_token_content tok in
    range, String.sub ~pos:1 ~len:(String.length str - 2) str
  in
  tok' (Token_tag.Literal Double_quoted) >>| extract_string_content
;;

let regex_lit =
  let extract_string_content tok =
    let range, str = extract_token_content tok in
    range, String.sub ~pos:1 ~len:(String.length str - 2) str
  in
  tok' (Token_tag.Literal Regex) >>| extract_string_content
;;

exception Lex_error of string * Token_stream.Token_tag.t Taparse_regex.Lexing.t * int

let debug_print_tokens = ref false

let stream_of_str str =
  match Lexer.lex str with
  | Ok token_list ->
    (if !debug_print_tokens
    then
      Fmt.(
        pr
          "%a@."
          (brackets (list ~sep:semi Token_stream.Token_tag.pp))
          (List.map ~f:(fun (tag, _, _) -> tag) token_list)));
    Stdlib.Stream.of_list token_list
  | Error (lexer, i) -> raise (Lex_error (str, lexer, i))
;;

open Taparse.Prelude

let parse_string_or_failwith p str =
  try str |> stream_of_str |> parse_exn p with
  | Parse_error msg ->
    Fmt.pr "@[parse_string Parse_error:@,%s@]" msg;
    failwith (Fmt.str "Parse error: %s" msg)
  | Type_error fmt ->
    Fmt.pr "@[parse_string Type_error:@,%a@]" fmt None;
    failwith "Parsing type error"
  | Lex_error (str, lexer, i) ->
    Fmt.pr "Lexer error (no match found):@.";
    let pos = Int.max 0 (i - 5) in
    let len = Int.min (String.length str - i) 10 in
    let spaces = if i < 5 then i else 5 in
    Fmt.pr "%s@." (String.sub str ~pos ~len);
    Fmt.pr "%s^@." (String.make spaces ' ');
    Fmt.pr "Lexer:@;%a" (Taparse_regex.Lexing.pp Token_stream.Token_tag.pp) lexer;
    failwith "Lexer error"
;;

let parse_string p str =
  try Result.Ok (str |> stream_of_str |> parse_exn p) with
  | Parse_error msg ->
    Result.Error (Fmt.str "@[TODO: parse_string Parse_error:@;%s@]" msg)
  | Type_error fmt -> Error (Fmt.str "@[TODO: parse_string Type_error:@;%a@]" fmt None)
  | Lex_error (str, lexer, i) ->
    Fmt.pr "Lexer error (no match found):@.";
    let pos = Int.max 0 (i - 5) in
    let len = Int.min (String.length str - i) 10 in
    let spaces = if i < 5 then i else 5 in
    Fmt.pr "%s@." (String.sub str ~pos ~len);
    Fmt.pr "%s^@." (String.make spaces ' ');
    Fmt.pr "Lexer:@;%a" (Taparse_regex.Lexing.pp Token_stream.Token_tag.pp) lexer;
    Error (Fmt.str "@[TODO: parse_string Lex_error:@;position %d@]" i)
;;

let aggregate_ranges lst =
  let ranges, lst = List.unzip lst in
  Opt_range.list_range ranges, lst
;;

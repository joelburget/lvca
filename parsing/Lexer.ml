open Base
open Lvca_provenance
open Taparse_regex

type lex_error_info = Token_stream.Token_tag.t Taparse_regex.Lexing.t * int

exception Lexer_err of lex_error_info

let escaped_char = Regex.(chr '\\' >>> any)

let lexer : Token_stream.Token_tag.t Lexing.t =
  let alphanum, digit, lowercase, uppercase, whitespace =
    Char_class.Common.(alphanum, digit, lowercase, uppercase, whitespace)
  in
  let mk_delimited c =
    let non_escaped =
      Char_class.(negate (union (Char.singleton c) (Char.singleton '\\')))
    in
    Regex.(chr c >>> star (escaped_char || char_class non_escaped) >>> chr c)
  in
  let single_quoted = mk_delimited '\'' in
  let double_quoted = mk_delimited '"' in
  let regex = mk_delimited '/' in
  let keywords =
    [ "in"; "rec"; "and"; "match"; "with"; "unquote"; "ctx" ]
    |> List.map ~f:(fun kw ->
           Regex.str kw, Lexing.Return (Token_stream.Token_tag.Keyword kw))
  in
  let ident_follow_chars = Char_class.(union alphanum (Char.singleton '_')) in
  let symbols =
    [ "{"
    ; "}"
    ; "["
    ; "]"
    ; "("
    ; ")"
    ; "."
    ; ";"
    ; ","
    ; "*"
    ; "/"
    ; "~"
    ; "="
    ; ">"
    ; ":="
    ; "|"
    ; "\\"
    ; ":"
    ; "->"
    ]
    |> List.map ~f:(fun str ->
           Regex.str str, Lexing.Return (Token_stream.Token_tag.Symbol str))
  in
  let opt_neg = Regex.(chr '-' || eps) in
  keywords
  @ symbols
  @ Regex.
      [ char_class whitespace, Lexing.Skip
      ; ( str "//" >>> star (char_class Char_class.(negate (Char.singleton '\n')))
        , Lexing.Skip )
      ; ( opt_neg >>> plus (char_class digit) >>> chr '.' >>> star (char_class digit)
        , Return (Literal Floating) )
      ; opt_neg >>> plus (char_class digit), Return (Literal Integer)
      ; ( char_class Char_class.(union lowercase (Char.singleton '_'))
          >>> star (char_class ident_follow_chars)
        , Return Lower_ident )
      ; char_class uppercase >>> star (char_class ident_follow_chars), Return Upper_ident
      ; single_quoted, Return (Literal Single_quoted)
      ; double_quoted, Return (Literal Double_quoted)
      ; regex, Return (Literal Regex)
      ]
;;

let lex_exn str =
  let filter_toks =
    List.filter_map ~f:(fun (Lexing.{ start; finish }, action) ->
        match action with
        | Lexing.Return token_tag ->
          Some
            ( token_tag
            , String.sub ~pos:start ~len:(finish - start) str
            , Opt_range.mk start finish )
        | Error _err -> raise (Lexer_err (lexer, 0 (* TODO *)))
        | _ -> None)
  in
  match Lexing.lex lexer str with
  | Ok toks -> filter_toks toks
  | Error err -> raise (Lexer_err (lexer, err))
;;

let lex str = try Ok (lex_exn str) with Lexer_err err -> Error err

let%expect_test _ =
  let pp = Fmt.(brackets (list ~sep:semi Token_stream.Token.pp)) in
  Fmt.pr
    "%a"
    pp
    (lex_exn {|1.1 and abc in Abc ("str \" str") ['c'] // comment
    {'\''}
  |});
  [%expect
    {|
    [(Literal Floating, "1.1", _); (Keyword "and", "and", _);
     (Lower_ident, "abc", _); (Keyword "in", "in", _); (Upper_ident, "Abc", _);
     (Symbol "(", "(", _); (Literal Double_quoted, "\"str \\\" str\"", _);
     (Symbol ")", ")", _); (Symbol "[", "[", _);
     (Literal Single_quoted, "'c'", _); (Symbol "]", "]", _);
     (Comment, "// comment", _); (Symbol "{", "{", _);
     (Literal Single_quoted, "'\\''", _); (Symbol "}", "}", _)] |}]
;;

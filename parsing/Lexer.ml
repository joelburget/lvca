open Base
open Lvca_provenance
open Taparse_regex

exception Lexer_err of int

let escaped_char = Regex.(chr '\\' >>> any)

let lexer : Token_stream.Token_tag.t Lexing.t =
  let alphanum, digit, lowercase, uppercase, whitespace =
    Char_class.Common.(alphanum, digit, lowercase, uppercase, whitespace)
  in
  let single_quoted =
    let non_escaped = Char_class.(negate (of_string "'\\")) in
    Regex.(chr '\'' >>> star (escaped_char || char_class non_escaped) >>> chr '\'')
  in
  let double_quoted =
    let non_escaped = Char_class.(negate (of_string "\"\\")) in
    Regex.(chr '"' >>> star (escaped_char || char_class non_escaped) >>> chr '"')
  in
  let keywords = [ "in"; "rec"; "and"; "match"; "with"; "unquote"; "ctx" ] in
  Regex.
    [ keywords |> List.map ~f:str |> List.fold ~init:empty ~f:( || ), Return Keyword
    ; char_class whitespace, Return Whitespace
    ; ( str "//" >>> star (char_class Char_class.(negate (Char.singleton '\n')))
      , Return Comment )
    ; ( star (char_class digit) >>> chr '.' >>> star (char_class digit)
      , Return (Literal Floating) )
    ; plus (char_class digit), Return (Literal Integer)
    ; char_class lowercase >>> star (char_class alphanum), Return Lower_ident
    ; char_class uppercase >>> star (char_class alphanum), Return Upper_ident
    ; single_quoted, Return (Literal Single_quoted)
    ; double_quoted, Return (Literal Double_quoted)
    ; char_class (Char_class.of_string "{}[]()"), Return Symbol
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
        | Error _err -> raise (Lexer_err 0 (* TODO *))
        | _ -> None)
  in
  match Lexing.lex lexer str with
  | Ok toks -> filter_toks toks
  | Error err -> raise (Lexer_err err)
;;

let lex str = try Ok (lex_exn str) with Lexer_err err -> Error err

let%expect_test _ =
  let pp = Fmt.(brackets (list ~sep:semi Token_stream.Token.pp)) in
  Fmt.pr "%a" pp (lex_exn {|1.1 and abc in Abc ("str \" str") ['c'] {'\''} // comment|});
  [%expect
    {|
    [Literal Floating "1.1"; Whitespace " "; Keyword "and"; Whitespace " ";
     Lower_ident "abc"; Whitespace " "; Keyword "in"; Whitespace " ";
     Upper_ident "Abc"; Whitespace " "; Symbol "(";
     Literal Double_quoted "\"str \\\" str\""; Symbol ")"; Whitespace " ";
     Symbol "["; Literal Single_quoted "'c'"; Symbol "]"; Whitespace " ";
     Symbol "{"; Literal Single_quoted "'\\''"; Symbol "}"; Whitespace " ";
     Comment "// comment"] |}]
;;

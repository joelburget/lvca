open Base
open Lexing
open Construction
open Lvca_provenance

type 'a t = 'a Lexing.t

let space = tok [ ' '; '\t'; '\n'; '\r' ] (* TODO: use char_class *)

let tok' : char -> (char * int) Construction.t = fun c -> tok [ c ]
let tok'' : char -> char t = fun c -> tok' c ==> fun (c, i) -> c, Opt_range.mk i (i + 1)
let cons (c, cs) = c :: cs

let aggregate_ranges cs =
  let cs, poss = List.unzip cs in
  match cs with
  | [] -> [], None
  | _ -> cs, Opt_range.mk (List.hd_exn poss) (List.last_exn poss)
;;

let aggregate_string : (char * int) list -> string * Opt_range.t =
 fun cs ->
  let cs, range = aggregate_ranges cs in
  String.of_char_list cs, range
;;

let to_str : (char * Opt_range.t) * (char * Opt_range.t) list -> string * Opt_range.t =
 fun (c, cs) ->
  let cs, ranges = List.unzip (c :: cs) in
  String.of_char_list cs, Opt_range.list_range ranges
;;

let whitespace1 = plus space ==> aggregate_string

let string str : string t =
  match String.to_list str with
  | [] -> eps ("", None)
  | c :: cs ->
    List.fold_right
      cs
      ~f:(fun c cs -> tok' c ++ cs ==> cons)
      ~init:(tok' c ==> List.return)
    ==> aggregate_string
;;

let comment : string t =
  string "//" ++ star (failwith "TODO: non-newline")
  ==> fun ((str, pos), charposs) ->
  let cs, poss = List.unzip charposs in
  str ^ String.of_char_list cs, Opt_range.union pos (List.last poss)
;;

let charset ~failure_msg s : char t =
  s |> String.to_list |> List.map ~f:tok'' |> choice ~failure_msg
;;

let lower_char : char t = charset ~failure_msg:"lower" "abcdefghijklmnopqrstuvwxyz"
(* TODO: use char classes *)

let upper_char : char t = charset ~failure_msg:"upper" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let char : char t = lower_char <|> upper_char (* TODO: alphanum+ *)

let lower_ident : string t = lower_char ++ star char ==> to_str
let upper_ident : string t = upper_char ++ star char ==> to_str
let keyword = choice ~failure_msg:"keyword" [ (* TODO *) ]
let literal, symbol = failwith "TODO"

let mk_tok : Token_stream.Token_tag.t -> string * Opt_range.t -> Token_stream.Token.t =
 fun tag (str, range) -> tag, str, range
;;

let lexer_construction : Token_stream.Token.t Construction.t =
  choice
    ~failure_msg:"symbol, comment, identifier, literal, or keyword"
    Token_stream.Token_tag.
      [ symbol ==> mk_tok (Symbol (failwith "TODO"))
      ; comment ==> mk_tok Comment
      ; upper_ident ==> mk_tok Upper_ident
      ; lower_ident ==> mk_tok Lower_ident
      ; literal ==> mk_tok (Literal (failwith "TODO"))
      ; keyword ==> mk_tok Keyword
      ; whitespace1 ==> mk_tok Whitespace
      ]
;;

let lex stream = Lexing.parse_exn (plus lexer_construction) stream
let lex' str = lex (Char_stream.make_stream str)

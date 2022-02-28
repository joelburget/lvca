module Parser :
  Taparse.Signatures.Parser
    with type token = Token_stream.token
     and type token_tag = Token_stream.token_tag
     and type stream = Token_stream.stream
     and type 'a v = 'a
     and type 'a parser = Token_stream.stream -> 'a =
  Taparse.Unstaged.Make (Token_stream)

include Parser
open Construction
open Token_stream

let ( >> ) = Lvca_util.(( >> ))

type 'a t = 'a Construction.t

let extract_token_content (_, lit, _) = lit
let symbol str = tok [ Token_tag.Symbol str ]
let braces p = symbol "{" *> p <* symbol "}"
let parens p = symbol "(" *> p <* symbol ")"
let brackets p = symbol "[" *> p <* symbol "]"
let lower_identifier = tok [ Token_tag.Lower_ident ] >>| extract_token_content
let upper_identifier = tok [ Token_tag.Upper_ident ] >>| extract_token_content
let cls _ = failwith "TODO"
let mark = failwith "TODO"
let sep_end_by sep p = sep_by sep p <* option sep
let sep_end_by1 sep p = sep_by1 sep p <* option sep
let return = eps

let ranged p =
  let+ start = mark
  and+ v = p
  and+ finish = mark in
  Lvca_provenance.Opt_range.mk start finish, v
;;

let integer_lit = tok [ Token_tag.Literal Integer ] >>| extract_token_content
let float_lit = tok [ Token_tag.Literal Floating ] >>| extract_token_content

let integer_or_float_lit =
  choice
    ~failure_msg:"integer_or_float_lit"
    [ integer_lit >>| Base.Either.first; float_lit >>| Base.Either.second ]
;;

let char_lit =
  let extract_char_content tok =
    let str = extract_token_content tok in
    if String.length str = 0 then String.unsafe_get str 0 else failwith "TODO: error"
  in
  tok [ Token_tag.Literal Single_quoted ] >>| extract_char_content
;;

let string_lit = tok [ Token_tag.Literal Double_quoted ] >>| extract_token_content
let stream_of_str = Lexer.lex' >> Stdlib.Stream.of_list
let parse_string_or_failwith p = stream_of_str >> parse_exn p

let parse_string p str =
  try Result.Ok (parse_string_or_failwith p str) with _ -> Result.Error "TODO: error"
;;

open Lvca_syntax

let language =
  [%lvca_abstract_syntax
    {|
lang := rows(list row)

// TODO: define exactly what term is
row := row(term[term]. directive)

directive :=
  | literal(string)
  | many(list directive)
  | many1(list directive)
  | sepby(directive; list directive)
  | sepby1(directive; list directive)
  | term(term)
  |}]
;;

let parser_mapping =
  {|

TODO

let translate_row = \(row : row) -> match row with {
  | row(pat. directive) -> ...
}

\(lang : lang) -> match lang with { rows(rows) -> list.map translate_row rows }
|}
;;

let printer_mapping =
  {|
let translate_directive = \(directive : directive) -> match directive with {
  | literal(str) -> text(str)
  | many(directives) ->
  | many1(directives) ->
    let directives = list.map translate_directive directives in
    let broken_directives = list.intersperse directives line() in
    alt(directives; broken_directives)
  | sepby(sep; directives) ->
  | sepby1(sep; directives) ->
    let directives = list.map translate_directive directives in
    let sep_directives = list.intersperse directives sep in
    let broken_sep_directives = list.intersperse directives cat(sep; line()) in
    alt(sep_directives; broken_sep_directives)
  | term(tm) -> term.print tm
}

let translate_row = \(row : row) -> match row with {
  // XXX how does this bind? Also, need term here.
  row(pat. directive) -> binding_aware_pattern.match(pat; term; translate_directive directive)
}

\(lang : lang) -> match lang with { rows(rows) -> list.map translate_row rows }
|}
;;

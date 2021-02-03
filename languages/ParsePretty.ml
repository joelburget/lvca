open Lvca_syntax

let language =
  [%lvca_abstract_syntax
    {|
lang := rows(list row)

row := row(tm[tm]. directive)

directive :=
  | literal(string)
  | many(list directive)
  | many1(list directive)
  | sepby(directive; list directive)
  | sepby1(directive; list directive)
  | var(tm)
  |}]
;;

let parser_mapping =
  {|

TODO

let translate_row row = match row with {
  | row(pat. directive) -> ...
}

\(x : lang) -> match x with { rows(rows) -> map translate_row rows }
|}
;;

let printer_mapping = {|
|}

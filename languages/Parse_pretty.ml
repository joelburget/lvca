open Base
open Lvca_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
list : * -> *  // module Lvca_core.List_model.List
string : *  // module Primitive.String
term : *  // module Nominal.Term
binding_aware_pattern : *  // TODO module Binding_aware_pattern

lang := Rows(list row)

row := Row(binding_aware_pattern; directive)

directive :=
  | Literal(string)
  | Many(list directive)
  | Many1(list directive)
  | Sep_by(directive; list directive)
  | Sep_by1(directive; list directive)
  | Term(term)
  |}]

let hutton_example =
  {|
expr:
  | lit(i)    <-> i
  | add(a; b) <-> a "+" b XXX precedence needed

type:
  | int() <-> "int"
  |}
;;

let eff_example =
  {|
value:
  | True()         <-> "true"
  | False()        <-> "false"
  | Fun(x. c)      <-> "fun" x "->" c
  | Handler_val(h) <-> h

handler_clause:
  | Return_clause(x. c)      <-> "return" x "->" c
  | Op_clause(name; x. k. c) <-> name "(" x "." k ")" "->" c

handler:
  | Handler(clauses) <-> "handler" "{" clauses "}"

computation:
  | Return(v)         <-> "return" v
  | Op(name; v; y. c) <-> name "(" v ";" y "." c ")"
  | Do(c1; x. c2)     <-> "do" x "<-" c1 "in" c2
  | If(v; c1; c2)     <-> "if" v "then" c1 "else" c2
  | App(v1; v2)       <-> v1 v2
  | With_handle(v; c) <-> "with" v "handle" c

v_type:
  | Bool() <-> "bool"
  | Fun_ty(v; c) <-> v "->" c
  | Handler_ty(c1; c2) <-> c1 "=>" c2

c_type:
  | Computation(v; ops) <-> v "!" "{" ops "}"
|}
;;

module Todo = struct
  (* Translate from this language to the parser langugage *)
  let parser_mapping =
    {|

TODO

let translate_row = \(row : row) -> match row with {
  | row(pat. directive) -> ...
}

\(lang : lang) -> match lang with { rows(rows) -> list.map translate_row rows }
|}
  ;;

  (* Translate from this language to the JYP printer langugage *)
  let printer_mapping =
    {|
let translate_directive = \(directive : directive) -> match directive with {
  | literal(str) -> text(str)
  | many(directives) ->
  | many1(directives) ->
    let directives = list.map translate_directive directives in
    let broken_directives = list.intersperse directives line() in
    alt(directives; broken_directives)
  | Sep_by(sep; directives) ->
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
end

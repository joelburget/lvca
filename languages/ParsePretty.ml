open Base
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

module Directive = struct
  type t =
    | Literal of string
    | Many of t list
    | Many1 of t list
    | SepBy of t * t list
    | SepBy1 of t * t list

  (* | Term *)

  let rec of_nonbinding tm =
    let open Result.Let_syntax in
    match tm with
    | NonBinding.Operator (_, "literal", [ Primitive (_, PrimString str) ]) ->
      Ok (Literal str)
    | Operator (_, "many", [ directives ]) ->
      let%map directives = list_of_nonbinding directives in
      Many directives
    | Operator (_, "many1", [ directives ]) ->
      let%map directives = list_of_nonbinding directives in
      Many1 directives
    | Operator (_, "sepby", [ directive; directives ]) ->
      let%bind directive = of_nonbinding directive in
      let%map directives = list_of_nonbinding directives in
      SepBy (directive, directives)
    | Operator (_, "sepby1", [ directive; directives ]) ->
      let%bind directive = of_nonbinding directive in
      let%map directives = list_of_nonbinding directives in
      SepBy1 (directive, directives)
    | _ -> Error ("Couldn't convert term", tm)

  and list_of_nonbinding tm =
    let open Result.Let_syntax in
    match tm with
    | NonBinding.Operator (_, "nil", []) -> Ok []
    | Operator (_, "cons", [ x; xs ]) ->
      let%bind x = of_nonbinding x in
      let%map xs = list_of_nonbinding xs in
      x :: xs
    | _ -> Error ("Couldn't convert term", tm)
  ;;
end

module Row = struct
  type 'info t =
    { pattern : 'info Pattern.t
    ; directive : Directive.t
    }
end

type 'info t = 'info Row.t list

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

open Lvca
module Parse_abstract = Parsing.Incremental (Parsing.Parseable_abstract_syntax)
module Parse_concrete = Parsing.Incremental (Parsing.Parseable_concrete_syntax)

let abstract_syntax_str =
  {|
import {integer} from "builtin:integer"

expr :=
  | lit(integer)    // an expression can be a literal integer
  | add(expr; expr) // or the addition of two expressions

type := int() // there's only one type in the language
  |}
;;

let abstract : AbstractSyntax.t =
  match Parse_abstract.parse abstract_syntax_str with
    | Error msg -> failwith msg
    | Ok desc -> desc
;;

let concrete_syntax_str =
  {|
// terminals:
ADD := "+"
LPAREN := "("
RPAREN := ")"
INTEGER := /\d+/
INT := "int"

// nonterminals:

expr := x = expr_1 { x }

// expressions at precedence level 1
expr_1 :=
  | x = expr_2 ADD y = expr_1 { add(x; y) }
  | x = expr_2                { x         }

// expressions at precedence level 2
expr_2 :=
  | LPAREN x = expr_1 RPAREN { x               }
  | x = INTEGER              { lit(integer(x)) }

type := INT { int() }
  |}
;;

let concrete =
  let pre_terminal_rules, sort_rules =
    match Parse_concrete.parse concrete_syntax_str with
    | Error msg -> failwith msg
    | Ok desc -> desc
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
;;

let parse_concrete = ConcreteSyntax.parse concrete "expr"
;;

let to_ast = ConcreteSyntax.to_ast concrete
;;

let expr_sort = AbstractSyntax.Types.SortAp ("expr", [||])

let of_ast = ConcreteSyntax.of_ast abstract.sort_defs concrete expr_sort "expr" 80
;;

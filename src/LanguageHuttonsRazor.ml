open Core_kernel

module Parse_abstract = Parsing.Incremental (Parsing.Parseable_abstract_syntax)
module Parse_concrete = Parsing.Incremental (Parsing.Parseable_concrete_syntax)
module Parse_statics = Parsing.Incremental (Parsing.Parseable_statics)
module Parse_dynamics = Parsing.Incremental (Parsing.Parseable_dynamics)

let abstract_syntax_str =
  {|
import {integer} from "builtin:integer"

expr :=
  | lit(integer())    // an expression can be a literal integer
  | add(expr(); expr()) // or the addition of two expressions

type := int() // there's only one type in the language
  |}
;;

let abstract : AbstractSyntax.t =
  match Parse_abstract.parse abstract_syntax_str with
    | Ok tm -> tm
    | Error err -> failwith (ParseError.to_string err)
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
  | x = INTEGER              { lit(integer(x)) }
  | LPAREN x = expr_1 RPAREN { x               }

type := INT { int() }
  |}
;;

let concrete =
  let pre_terminal_rules, sort_rules =
    match Parse_concrete.parse concrete_syntax_str with
      | Ok tm -> tm
      | Error err -> failwith (ParseError.to_string err)
  in
  ConcreteSyntax.make_concrete_description pre_terminal_rules sort_rules
;;

let statics_str =
  {|
--------------------------
ctx >> lit(_) => int()

-----------------------------
ctx >> add(_; _) => int()
  |}
;;

let statics = match Parse_statics.parse statics_str with
  | Ok statics -> statics
  | Error err -> failwith (ParseError.to_string err)
;;

let bidirectional_env = Bidirectional.
  { rules = statics
  ; var_types = String.Map.empty
  }

let check : Statics.typing -> unit
  = Bidirectional.check bidirectional_env
;;

exception InferenceError

(**
 @raise [FreeVar]
 @raise [InferenceError]
 *)
let infer : Binding.Nominal.term -> Binding.Nominal.term
  = fun tm -> tm
    |> Binding.DeBruijn.from_nominal
    |> Result.ok_or_failwith
    |> Statics.Types.of_de_bruijn
    |> Bidirectional.infer bidirectional_env
    |> Statics.Types.to_de_bruijn_exn
    |> Binding.DeBruijn.to_nominal
    |> Util.get_option InferenceError
;;

(*
let dynamics_str =
  {|
dynamics = \(expr : expr()) -> match expr with {
  | add(a; b) -> #add(dynamics a; dynamics b)
  | lit(i) -> i
}
  |}
;;

let dynamics = Parse_dynamics.parse dynamics_str |> Result.ok_or_failwith
;;
*)

(* TODO: write a functor to do this stuff for any language? *)

let parse_concrete = ConcreteSyntax.parse concrete "expr"
;;

let to_ast = ConcreteSyntax.to_ast concrete
;;

let expr_sort = AbstractSyntax.Types.SortAp ("expr", [||])

let of_ast = ConcreteSyntax.of_ast abstract.sort_defs concrete expr_sort "expr" 80
;;

let%expect_test {|pretty lit(1)|} =
  let tm = Binding.Nominal.(Operator ("lit",
    [ Scope ([], Primitive (PrimInteger (Bigint.of_int 1)))
    ]))
  in
  print_string (ConcreteSyntax.to_string (of_ast tm));
  [%expect{| 1 |}]

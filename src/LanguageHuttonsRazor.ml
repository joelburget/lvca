open Core_kernel

module Description = struct

  let abstract_syntax =
    {|
  import {integer} from "builtin"

  expr :=
    | lit(integer())    // an expression can be a literal integer
    | add(expr(); expr()) // or the addition of two expressions

  type := int() // there's only one type in the language
    |}
  ;;

  let concrete_syntax =
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

  let statics =
    {|
  --------------------------
  ctx >> lit(_) => int()

  -----------------------------
  ctx >> add(_; _) => int()
    |}
  ;;

  let expr_name = "expr";;

end

open Language.Make(Description)

(*
let dynamics_str =
  {|
dynamics = \(expr : expr()) -> match expr with {
  | add(a; b) ->
      let a' = dynamics a in
      let b' = dynamics b in
      #add(a'; b')
  | lit(i) -> i
};
  |}
;;

let dynamics = match Parsing.Dynamics.parse dynamics_str with
  | Error err -> failwith (ParseError.to_string err)
  | Ok dynamics -> dynamics
;;
*)

let%expect_test {|pretty lit(1)|} =
  let tm = Binding.Nominal.(Operator ("lit",
    [ Scope ([], Primitive (PrimInteger (Bigint.of_int 1)))
    ]))
  in
  print_string (ConcreteSyntax.to_string (of_ast tm));
  [%expect{| 1 |}]

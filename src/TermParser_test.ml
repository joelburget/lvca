open Types
open Binding
open Nominal

let expect_parse str tm =
  Term.Parser.top_term Term.Lexer.read (Lexing.from_string str) = tm

let%test_module "TermParser" = (module struct
  let%test "" = expect_parse "[x]" (Sequence [Var "x"])
  let%test "" = expect_parse "x" (Var "x")
  let%test "" = expect_parse "123" (Primitive (PrimInteger (Bigint.of_int 123)))
  let%test "" = expect_parse "\"abc\"" (Primitive (PrimString "abc"))
  let%test "" = expect_parse "lam(x. x)" (Operator ("lam", [Scope ([Var "x"], Var "x")]))
end)

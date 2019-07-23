open Jest
open Expect
open Types
open Binding

let _ = describe "TermParser" (fun () ->
  let open Nominal in

  let expectParse str tm = test ("'" ^ str ^ "'") (fun () ->
    expect (TermParser.top_term TermLexer.read (Lexing.from_string str))
    |> toEqual tm
  ) in

  expectParse "[x]" (Sequence [Var "x"]);
  expectParse "x" (Var "x");
  expectParse "123" (Primitive (PrimInteger (Bigint.of_int 123)));
  (* Question: should we support true / false?
  expectParse "true" (Primitive (PrimBool true));
  expectParse "false" (Primitive (PrimBool false)); *)
  expectParse "\"abc\"" (Primitive (PrimString "abc"));
  expectParse "lam(x. x)" (Operator ("lam", [Scope (["x"], Var "x")]));
)

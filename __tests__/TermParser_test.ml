open Jest
open Expect
open Types

(* expected serialized values via http://cbor.me/ *)

let _ = describe "TermParser" (fun () ->
  let open Ast in

  let expectParse str tm = test ("'" ^ str ^ "'") (fun () ->
    (match TermParser.top_term TermLexer.read (Lexing.from_string str) with
    | tm' -> expect tm' |> toEqual tm
    | exception _except -> fail ("'" ^ str ^ "' triggered an exception")
  )) in

  expectParse "[x]" (Sequence [Var "x"]);
  expectParse "x" (Var "x");
  expectParse "123" (Primitive (PrimInteger (Bigint.of_int 123)));
  (* Question: should we support true / false?
  expectParse "true" (Primitive (PrimBool true));
  expectParse "false" (Primitive (PrimBool false)); *)
  expectParse "\"abc\"" (Primitive (PrimString "abc"));
  expectParse "lam(x. x)" (Term ("lam", [Scope (["x"], Var "x")]));
)

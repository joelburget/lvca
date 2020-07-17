open Base
open Lvca

module Description = struct
  module ParseAbstract = AbstractSyntax.Parse(Util.Angstrom.CComment)

  let abstract_syntax : AbstractSyntax.t =
    Angstrom.parse_string ~consume:All ParseAbstract.t
    {|import {integer} from "builtin"

  expr :=
    | lit(integer())    // an expression can be a literal integer
    | add(expr(); expr()) // or the addition of two expressions

  type := int() // there's only one type in the language
    |}
    |> Result.ok_or_failwith
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

(* Write by hand first, later assert the generated parser is equivalent *)
module AngstromParse(Comment : Util.Angstrom.Comment_int) = struct
  module Parsers = Util.Angstrom.Mk(Comment)
  let chainl1 = Util.Angstrom.chainl1
  let char, integer_lit, parens = Parsers.(char, integer_lit, parens)
  let choice, fix, return, (>>|), (<|>), ( *> ) = Angstrom.(choice, fix, return, (>>|), (<|>), ( *> ))

  let lit : NonBinding.term Angstrom.t
    = integer_lit >>| (fun str -> NonBinding.Primitive (Primitive.PrimInteger (Bigint.of_string str)))

  let t : NonBinding.term Angstrom.t
    = fix (fun t ->
      let atom = lit <|> parens t in
      let add = char '+' *> return (fun x y -> NonBinding.Operator ("add", [[x]; [y]])) in
      chainl1 atom add)

end
;;

let%test_module "Hutton's Razor" = (module struct
  module Parse = AngstromParse(Util.Angstrom.NoComment)
  let parse str = Angstrom.parse_string ~consume:All Parse.t str
  let print_parse str = match parse str with
    | Error str -> Caml.print_string str
    | Ok tm -> NonBinding.pp Caml.Format.std_formatter tm

  let%expect_test _ = print_parse "1"; [%expect{| 1 |}]
  let%expect_test _ = print_parse "1 + 2"; [%expect{| add(1; 2) |}]
  let%expect_test _ = print_parse "1 + 2 + 3"; [%expect{| add(add(1; 2); 3) |}]
  let%expect_test _ = print_parse "1 + (2 + 3)"; [%expect{| add(1; add(2; 3)) |}]
end)
;;

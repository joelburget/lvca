open Base
open Lvca

module Description = struct
  module ParseAbstract = AbstractSyntax.Parse(struct
    open Angstrom
    let comment =
      string "//" >>= fun _ ->
      many (satisfy Char.(fun x -> x <> '\n')) >>| fun _ ->
      ()
    let reserved = Util.String.Set.empty
  end)

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

let%test_module "Hutton's Razor" = (module struct
  let%expect_test {|pretty lit(1)|} =
    let tm = Binding.Nominal.(Operator ("lit",
      [ Scope ([], Primitive (PrimInteger (Bigint.of_int 1)))
      ]))
    in
    print_string (ConcreteSyntax.to_string (of_ast tm));
    [%expect{| 1 |}]
end)
;;
*)

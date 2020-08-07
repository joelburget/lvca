open Base
open Lvca

module Description = struct
  module ParseAbstract = AbstractSyntax.Parse(Util.Angstrom.CComment)
  module ParseDynamics = Core.Parse(Util.Angstrom.CComment)

  let abstract_syntax : AbstractSyntax.t =
    Angstrom.parse_string ~consume:All ParseAbstract.t
    {|import {integer} from "lvca/builtin"

  expr :=
    | lit(integer())    // an expression can be a literal integer
    | add(expr(); expr()) // or the addition of two expressions

  type := int() // there's only one type in the language
    |}
    |> Result.ok_or_failwith
  ;;

  let parser_str =
    {|
  fix (parser ->
    let space = (' ' | '\n' | '\t')* in
    let lit = satisfy (c -> is_digit c) in
    let atom =
      | lit space -> { lit }
      | '(' space parser space ')' space -> { parser })
    in
    l=atom r=('+' space parser)? -> {
      match r with
        | some(r') -> add(l; r')
        | none() -> l
    }
  )
    |}
  ;;

  let statics =
    {|
  ----------------------
  ctx >> lit(_) => int()

  -------------------------
  ctx >> add(_; _) => int()
    |}
  ;;

  let expr_name = "expr";;

  let dynamics_str = {|
  let rec dynamics = \(expr : expr()) -> match expr with {
    | add(a; b) ->
        let a' = dynamics a in
        let b' = dynamics b in
        {add(a'; b')}
    | lit(i) -> i
  }
  in dynamics
  |}

  let dynamics : Core.term
    = Angstrom.parse_string ~consume:All
      Angstrom.(Util.Angstrom.whitespace *> ParseDynamics.term) dynamics_str
    |> Result.ok_or_failwith
  ;;
end

(* Write by hand first, later assert the generated parser is equivalent *)
module AngstromParse(Comment : Util.Angstrom.Comment_int) = struct
  module Parsers = Util.Angstrom.Mk(Comment)
  let chainl1, whitespace = Util.Angstrom.(chainl1, whitespace)
  let char, integer_lit, parens = Parsers.(char, integer_lit, parens)
  let choice, fix, return, (>>|), (<|>), ( *> ) = Angstrom.(choice, fix, return, (>>|), (<|>), ( *> ))

  let lit : NonBinding.term Angstrom.t
    = integer_lit >>| (fun str -> NonBinding.(Operator
      ( "lit"
      , [ [ Primitive (Primitive.PrimInteger (Bigint.of_string str)) ] ]
      )))

  let t : NonBinding.term Angstrom.t
    = fix (fun t ->
      let atom = lit <|> parens t in
      let add = char '+' *> return (fun x y -> NonBinding.Operator ("add", [[x]; [y]])) in
      chainl1 atom add)

  let whitespace_t = whitespace *> t
end
;;

let rec eval' : NonBinding.term -> (Bigint.t, string) Result.t
  = function
    | Operator ("add", [[a];[b]]) ->
      begin
        match eval' a, eval' b with
          | Ok a', Ok b' -> Ok Bigint.(a' + b')
          | Error msg, _ | _, Error msg -> Error msg
      end
    | Operator ("lit", [[Primitive (PrimInteger i)]]) -> Ok i
    | tm -> Error ("found un-evaluable term: " ^ NonBinding.to_string tm)


let eval : string -> (Bigint.t, string) Result.t
  = let module Parse = AngstromParse(Util.Angstrom.NoComment) in
    fun str ->
    match Angstrom.parse_string ~consume:All Parse.whitespace_t str with
      | Error str -> Error str
      | Ok tm -> eval' tm

let eval_2 : string -> (Bigint.t, string) Result.t
  = let module Parse = AngstromParse(Util.Angstrom.NoComment) in
    fun str ->
    match Angstrom.parse_string ~consume:All Parse.whitespace_t str with
      | Error str -> Error str
      | Ok tm ->
        begin
          match Core.(eval (CoreApp (Description.dynamics, Term (NonBinding.to_nominal tm)))) with
            | Error (msg, tm) -> Error (msg ^ ": " ^ Core.to_string tm)
            | Ok (Primitive (PrimInteger i)) -> Ok i
            | _ -> Error "unexpected non-integer result"
        end

let%test_module "Hutton's Razor" = (module struct
  module Parse = AngstromParse(Util.Angstrom.NoComment)
  let parse str = Angstrom.parse_string ~consume:All Parse.whitespace_t str
  let print_parse str = match parse str with
    | Error str -> Caml.print_string str
    | Ok tm -> NonBinding.pp Caml.Format.std_formatter tm

  let%expect_test _ = print_parse "1"; [%expect{| lit(1) |}]
  let%expect_test _ = print_parse "1 + 2"; [%expect{| add(lit(1); lit(2)) |}]
  let%expect_test _ = print_parse "1 + 2 + 3"; [%expect{| add(add(lit(1); lit(2)); lit(3)) |}]
  let%expect_test _ = print_parse "1 + (2 + 3)"; [%expect{| add(lit(1); add(lit(2); lit(3))) |}]

  let print_eval : string -> unit
    = fun str -> Caml.print_string (match eval str with
      | Error msg -> msg
      | Ok i -> Bigint.to_string i)

  let%expect_test _ = print_eval "1"; [%expect{| 1 |}]
  let%expect_test _ = print_eval "1 + 2"; [%expect{| 3 |}]
  let%expect_test _ = print_eval "1 + 2 + 3"; [%expect{| 6 |}]
  let%expect_test _ = print_eval "1 + (2 + 3)"; [%expect{| 6 |}]
end)
;;

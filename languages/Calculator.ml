open Base
open Lvca_syntax
open Lvca_provenance
open Stdio
module List_model = Lvca_core.List_model

module Lang =
[%lvca.abstract_syntax_module
{|
integer : *  // module Primitive.Integer
float : *  // module Primitive.Float

either a b := Left(a) | Right(b)

expr :=
  | Add(expr; expr)
  | Sub(expr; expr)
  | Mul(expr; expr)
  | Div(expr; expr)
  | Max(expr; expr)
  | Min(expr; expr)

  | Negate(expr)
  | Sqrt(expr)
  | Abs(expr)
  | Exp(expr)
  | Ln(expr)
  | Sin(expr)
  | Cos(expr)
  | Tan(expr)
  | Asin(expr)
  | Acos(expr)
  | Atan(expr)

  | Pi()
  | E()

  | Lit(either integer float)
|}]

open Lang

let rec to_tex ~none expr =
  let open Tex_math.Tex in
  let list = List_model.of_list in
  let go x =
    match to_tex ~none x with
    | List_model.List.Cons (_, x, Nil _) -> x
    | xs -> Grouped (none, xs)
  in
  match expr with
  | Expr.Add (i, x, y) | Sub (i, x, y) | Mul (i, x, y) | Div (i, x, y) ->
    let c =
      match expr with
      | Expr.Add _ -> '+'
      | Sub _ -> '-'
      | Mul _ -> '*'
      | Div _ -> '/'
      | _ -> Lvca_util.invariant_violation ~here:[%here] "unexpected constructor"
    in
    list ~empty_info:i [ go x; Token (i, (none, c)); go y ]
  | Max (i, x, y) ->
    list ~empty_info:i [ Control_seq (none, (none, "\\max")); go x; go y ]
  | Min (i, x, y) ->
    list ~empty_info:i [ Control_seq (none, (none, "\\min")); go x; go y ]
  | Negate (i, x) -> list ~empty_info:i [ Literal (i, (none, "-")); go x ]
  | Sqrt (i, x)
  | Abs (i, x)
  | Exp (i, x)
  | Ln (i, x)
  | Sin (i, x)
  | Cos (i, x)
  | Tan (i, x)
  | Asin (i, x)
  | Acos (i, x)
  | Atan (i, x) ->
    let control_seq =
      match expr with
      | Sqrt _ -> "\\sqrt"
      | Abs _ -> "\\abs"
      | Exp _ -> "\\exp"
      | Ln _ -> "\\ln"
      | Sin _ -> "\\sin"
      | Cos _ -> "\\cos"
      | Tan _ -> "\\tan"
      | Asin _ -> "\\asin"
      | Acos _ -> "\\acos"
      | Atan _ -> "\\atan"
      | _ -> Lvca_util.invariant_violation ~here:[%here] "unexpected constructor"
    in
    list ~empty_info:i [ Control_seq (none, (none, control_seq)); go x ]
  | Pi i -> list ~empty_info:i [ Control_seq (i, (none, "\\pi")) ]
  | E i -> list ~empty_info:i [ Token (i, (none, 'e')) ]
  | Lit (i, lit) ->
    let x =
      match lit with
      | Left (i, x) -> Literal (i, (i, Primitive.Integer.to_string x))
      | Right (i, x) -> Literal (i, (i, Primitive.Float.to_string x))
    in
    list ~empty_info:i [ x ]
;;

type term = Opt_range.t Expr.t

let unary_operators =
  [ "negate"; "sqrt"; "abs"; "exp"; "ln"; "sin"; "cos"; "tan"; "asin"; "acos"; "atan" ]
;;

let constants = [ "pi"; "e" ]

module Parse = struct
  open Lvca_parsing

  let lit : term Lvca_parsing.t =
    (* TODO: this fails on too-large float lits *)
    Ws.integer_or_float_lit
    >>~ fun range lit ->
    let lit =
      match lit with
      | Base.Either.First str -> Either.Left (range, (range, Z.of_string str))
      | Second f -> Right (range, (range, f))
    in
    Expr.Lit (range, lit)
  ;;

  let mk_const range name =
    match name with
    | "pi" -> Expr.Pi range
    | "e" -> E range
    | _ -> Lvca_util.invariant_violation ~here:[%here] "Unexpected constant"
  ;;

  let mk_binary range name x y =
    match name with
    | "add" -> Expr.Add (range, x, y)
    | "sub" -> Sub (range, x, y)
    | "mul" -> Mul (range, x, y)
    | "div" -> Div (range, x, y)
    | "max" -> Max (range, x, y)
    | "min" -> Min (range, x, y)
    | _ -> Lvca_util.invariant_violation ~here:[%here] "Unexpected binary operator"
  ;;

  let mk_unary range name x =
    match name with
    | "negate" -> Expr.Negate (range, x)
    | "sqrt" -> Sqrt (range, x)
    | "abs" -> Abs (range, x)
    | "exp" -> Exp (range, x)
    | "ln" -> Ln (range, x)
    | "sin" -> Sin (range, x)
    | "cos" -> Cos (range, x)
    | "tan" -> Tan (range, x)
    | "asin" -> Asin (range, x)
    | "acos" -> Acos (range, x)
    | "atan" -> Atan (range, x)
    | _ -> Lvca_util.invariant_violation ~here:[%here] "Unexpected unary operator"
  ;;

  let const =
    constants
    |> List.map ~f:Ws.string
    |> choice ~failure_msg:"looking for a constant name"
    >>~ mk_const
  ;;

  (* Precedence:
   * application
   * *, /
   * +, -
   *)
  let t : term Lvca_parsing.t =
    fix (fun t ->
        let atom : term Lvca_parsing.t =
          choice
            ~failure_msg:"looking for a literal, constant, or parenthesized expression"
            [ lit; const; Ws.parens t ]
        in
        (* TODO: rename negate to - *)
        let unary_op : term Lvca_parsing.t =
          unary_operators
          |> List.map ~f:(fun name ->
                 Ws.string name
                 >>== fun { value = name; range = p1 } ->
                 atom
                 >>| fun body ->
                 let pos = Opt_range.union p1 (Expr.info body) in
                 mk_unary pos name body)
          |> choice ~failure_msg:"looking for a unary operator expression"
        in
        let application =
          let min_max =
            choice
              [ Ws.string "min"; Ws.string "max" ]
              ~failure_msg:"looking for min or max"
            >>== fun { value = name; range = p1 } ->
            lift2
              (fun atom1 atom2 ->
                let pos = Opt_range.union p1 (Expr.info atom2) in
                mk_binary pos name atom1 atom2)
              atom
              atom
          in
          atom <|> unary_op <|> min_max
        in
        let pair p1 p2 = lift2 (fun x y -> x, y) p1 p2 in
        let mul_div : term Lvca_parsing.t =
          let op = Ws.char '*' <|> Ws.char '/' in
          let f l (op, r) =
            let rng = Opt_range.union (Expr.info l) (Expr.info r) in
            match op with
            | '*' -> Expr.Mul (rng, l, r)
            | '/' -> Div (rng, l, r)
            | _ -> failwith "error: impossible operator"
          in
          application >>= fun init -> many (pair op application) >>| List.fold ~init ~f
        in
        let add_sub : term Lvca_parsing.t =
          let op = Ws.char '+' <|> Ws.char '-' in
          let f l (op, r) =
            let rng = Opt_range.union (Expr.info l) (Expr.info r) in
            match op with
            | '+' -> Expr.Add (rng, l, r)
            | '-' -> Sub (rng, l, r)
            | _ -> failwith "error: impossible operator"
          in
          mul_div >>= fun init -> many (pair op mul_div) >>| List.fold ~init ~f
        in
        add_sub)
    <?> "parser"
  ;;
end

let rec interpret : term -> Constructive_real.t =
 fun tm ->
  match tm with
  | Lit (_, Left (_, (_, i))) -> Constructive_real.of_bigint i
  | Lit (_, Right (_, (_, f))) -> Constructive_real.of_float f
  | E _ -> Constructive_real.e
  | Pi _ -> Constructive_real.pi
  | Add (_, l, r)
  | Sub (_, l, r)
  | Mul (_, l, r)
  | Div (_, l, r)
  | Min (_, l, r)
  | Max (_, l, r) ->
    let f =
      Constructive_real.(
        match tm with
        | Add _ -> ( + )
        | Sub _ -> ( - )
        | Mul _ -> ( * )
        | Div _ -> ( / )
        | Min _ -> min
        | Max _ -> max
        | _ -> Lvca_util.invariant_violation ~here:[%here] "unexpected term")
    in
    f (interpret l) (interpret r)
  | Negate (_, x)
  | Sqrt (_, x)
  | Abs (_, x)
  | Exp (_, x)
  | Ln (_, x)
  | Sin (_, x)
  | Cos (_, x)
  | Tan (_, x)
  | Asin (_, x)
  | Acos (_, x)
  | Atan (_, x) ->
    let f =
      Constructive_real.(
        match tm with
        | Negate _ -> negate
        | Sqrt _ -> sqrt
        | Abs _ -> abs
        | Exp _ -> exp
        | Ln _ -> ln
        | Sin _ -> sin
        | Cos _ -> cos
        | Tan _ -> tan
        | Asin _ -> asin
        | Acos _ -> acos
        | Atan _ -> atan
        | _ -> Lvca_util.invariant_violation ~here:[%here] "unexpected term")
    in
    f (interpret x)
;;

let%test_module "Evaluation" =
  (module struct
    let go str =
      match Lvca_parsing.parse_string Parse.t str with
      | Error msg -> print_endline msg
      | Ok tm -> tm |> interpret |> Constructive_real.eval_to_string |> print_endline
    ;;

    let%expect_test _ =
      go "1 + 1";
      go "e + pi";
      go "ln e + cos pi";
      go "1 + 2 * 3 / 6 - 1";
      go "acos (cos pi)";
      go "asin (sin pi)";
      go "acos (cos pi) + asin (sin pi) * negate 1";
      go "min 1 2";
      go "max 1 2";
      go "1.2";
      go "(1.356 + 1.355) / 2.0";
      [%expect
        {|
      2.0000000000
      5.8598744820
      0.0000000000
      1.0000000000
      3.1415926536
      0.0000000000
      3.1415926536
      1.0000000000
      2.0000000000
      1.2000000000
      1.3555000000
      |}]
    ;;
  end)
;;

let%test_module "to_tex" =
  (module struct
    let go str =
      match Lvca_parsing.parse_string Parse.t str with
      | Error msg -> print_endline msg
      | Ok tm ->
        tm
        |> to_tex ~none:None
        |> List_model.to_list
        |> List.map ~f:Tex_math.Tex.to_plain
        |> Fmt.pr "%a\n" Fmt.(list ~sep:(any "") Tex_math.pp)
    ;;

    let%expect_test _ =
      go "1 + 1";
      go "sqrt 1";
      go "negate 1";
      go "sin pi";
      [%expect {|
      1+1
      \sqrt1
      -1
      \sin\pi
      |}]
    ;;
  end)
;;

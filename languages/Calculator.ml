open Base
open Lvca_syntax
open Lvca_provenance
open Lvca_util
open Lvca_models
open Stdio

module Lang =
[%lvca.abstract_syntax_module
{|
integer : *
float : *

either a b := Left(a) | Right(b);

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
  ;
|}
, { integer = "Primitive.Integer"; float = "Primitive.Float" }]

include Lang

let rec to_tex expr =
  let open Tex_math.Tex in
  let here = Provenance.of_here [%here] in
  let list = List_model.of_list in
  let go x =
    match to_tex x with List_model.Cons (_, x, Nil _) -> x | xs -> Grouped (here, xs)
  in
  match expr with
  | Expr.Add (i, x, y) | Sub (i, x, y) | Mul (i, x, y) | Div (i, x, y) ->
    let c =
      match expr with
      | Expr.Add _ -> '+'
      | Sub _ -> '-'
      | Mul _ -> '*'
      | Div _ -> '/'
      | _ -> Lvca_util.invariant_violation [%here] "unexpected constructor"
    in
    list [ go x; Token (i, (here, c)); go y ]
  | Max (_, x, y) -> list [ Control_seq (here, (here, "\\max")); go x; go y ]
  | Min (_, x, y) -> list [ Control_seq (here, (here, "\\min")); go x; go y ]
  | Negate (i, x) -> list (Tex_math.literal i "-" @ [ go x ])
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
      | _ -> Lvca_util.invariant_violation [%here] "unexpected constructor"
    in
    list [ Control_seq (here, (here, control_seq)); go x ]
  | Pi i -> list [ Control_seq (i, (here, "\\pi")) ]
  | E i -> list [ Token (i, (here, 'e')) ]
  | Lit (_, lit) ->
    let x =
      match lit with
      | Left (i, x) -> Tex_math.literal i (Primitive.Integer.to_string x)
      | Right (i, x) -> Tex_math.literal i (Primitive.Float.to_string x)
    in
    list x
;;

let unary_operators =
  [ "negate"; "sqrt"; "abs"; "exp"; "ln"; "sin"; "cos"; "tan"; "asin"; "acos"; "atan" ]
;;

let constants = [ "pi"; "e" ]

module Parse = struct
  open Lvca_parsing
  open C_comment_parser

  let lit : Expr.t Lvca_parsing.t =
    (* TODO: this fails on too-large float lits *)
    integer_or_float_lit
    >>~ fun range lit ->
    let lit =
      match lit with
      | Base.Either.First str ->
        Either.Left
          (Provenance.of_range range, (Provenance.of_range range, Z.of_string str))
      | Second f -> Right (Provenance.of_range range, (Provenance.of_range range, f))
    in
    let pos = Provenance.of_range range in
    Expr.Lit (pos, lit)
  ;;

  let mk_const range name =
    let range = Provenance.of_range range in
    match name with
    | "pi" -> Expr.Pi range
    | "e" -> E range
    | _ -> Lvca_util.invariant_violation [%here] "Unexpected constant"
  ;;

  let mk_binary range name x y =
    match name with
    | "add" -> Expr.Add (range, x, y)
    | "sub" -> Sub (range, x, y)
    | "mul" -> Mul (range, x, y)
    | "div" -> Div (range, x, y)
    | "max" -> Max (range, x, y)
    | "min" -> Min (range, x, y)
    | _ -> Lvca_util.invariant_violation [%here] "Unexpected binary operator"
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
    | _ -> Lvca_util.invariant_violation [%here] "Unexpected unary operator"
  ;;

  let const =
    constants
    |> List.map ~f:string
    |> choice ~failure_msg:"looking for a constant name"
    >>~ mk_const
  ;;

  (* Precedence:
   * application
   * *, /
   * +, -
   *)
  let t : Expr.t Lvca_parsing.t =
    fix (fun t ->
        let atom : Expr.t Lvca_parsing.t =
          choice
            ~failure_msg:"looking for a literal, constant, or parenthesized expression"
            [ lit; const; parens t ]
        in
        (* TODO: rename negate to - *)
        let unary_op : Expr.t Lvca_parsing.t =
          unary_operators
          |> List.map ~f:(fun name ->
                 let%bind p1, name = attach_pos' (string name) in
                 atom
                 >>~ fun p2 body ->
                 let pos = Opt_range.union p1 p2 |> Provenance.of_range in
                 mk_unary pos name body)
          |> choice ~failure_msg:"looking for a unary operator expression"
        in
        let application =
          let min_max =
            let%bind p1, name =
              attach_pos'
                (choice
                   [ string "min"; string "max" ]
                   ~failure_msg:"looking for min or max")
            in
            lift2
              (fun atom1 (p2, atom2) ->
                let pos = Opt_range.union p1 p2 |> Provenance.of_range in
                mk_binary pos name atom1 atom2)
              atom
              (attach_pos' atom)
          in
          atom <|> unary_op <|> min_max
        in
        let pair p1 p2 = lift2 (fun x y -> x, y) p1 p2 in
        let mul_div : Expr.t Lvca_parsing.t =
          let op = char '*' <|> char '/' in
          let f (l_range, l) (op, (r_range, r)) =
            let range = Opt_range.union l_range r_range in
            let info = Provenance.of_range range in
            let tm =
              match op with
              | '*' -> Expr.Mul (info, l, r)
              | '/' -> Div (info, l, r)
              | _ -> failwith "error: impossible operator"
            in
            range, tm
          in
          let%bind init = attach_pos' application in
          many (pair op (attach_pos' application)) >>| (List.fold ~init ~f >> snd)
        in
        let add_sub : Expr.t Lvca_parsing.t =
          let op = char '+' <|> char '-' in
          let f (l_range, l) (op, (r_range, r)) =
            let range = Opt_range.union l_range r_range in
            let info = Provenance.of_range range in
            let tm =
              match op with
              | '+' -> Expr.Add (info, l, r)
              | '-' -> Sub (info, l, r)
              | _ -> failwith "error: impossible operator"
            in
            range, tm
          in
          let%bind init = attach_pos' mul_div in
          many (pair op (attach_pos' mul_div)) >>| (List.fold ~init ~f >> snd)
        in
        add_sub)
    <?> "parser"
  ;;
end

let rec interpret : Expr.t -> Constructive_real.t =
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
        | _ -> Lvca_util.invariant_violation [%here] "unexpected term")
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
        | _ -> Lvca_util.invariant_violation [%here] "unexpected term")
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
        |> to_tex
        |> List_model.to_list
        |> Fmt.pr "%a\n" Fmt.(list ~sep:nop Tex_math.pp)
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

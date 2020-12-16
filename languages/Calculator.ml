open Base
open Lvca_syntax
open Stdio

type term = (OptRange.t, ConstructiveReal.t) NonBinding.term

let binary_operators = [ "add"; "sub"; "mul"; "div"; "max"; "min" ]

let unary_operators =
  [ "negate"; "sqrt"; "abs"; "exp"; "ln"; "sin"; "cos"; "tan"; "asin"; "acos"; "atan" ]
;;

let constants = [ "pi"; "e" ]

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let lit =
    (* TODO: this fails on too-large float lits *)
    integer_or_float_lit
    >>|| fun ~pos lit ->
    let lit =
      match lit with
      | Either.First str -> ConstructiveReal.of_bigint (Z.of_string str)
      | Either.Second f -> ConstructiveReal.of_float f
    in
    let tm = NonBinding.Operator (pos, "lit", [ [ Primitive (pos, lit) ] ]) in
    tm, pos
  ;;

  let const =
    constants
    |> List.map ~f:string
    |> choice
    >>|| fun ~pos name -> NonBinding.Operator (pos, name, []), pos
  ;;

  (* Precedence:
   * application
   * *, /
   * +, -
   *)
  let t : term Parsers.t =
    fix (fun t ->
        let atom : term Parsers.t = choice [ lit; const; parens t ] in
        (* TODO: rename negate to - *)
        let unary_op : term Parsers.t =
          unary_operators
          |> List.map ~f:(fun name ->
                 string name
                 >>== fun ~pos:p1 name ->
                 atom
                 >>| fun body ->
                 let pos = OptRange.union p1 (NonBinding.location body) in
                 NonBinding.Operator (pos, name, [ [ body ] ]))
          |> choice
        in
        let application =
          let min_max =
            choice [ string "min"; string "max" ]
            >>== fun ~pos:p1 name ->
            lift2
              (fun atom1 atom2 ->
                let pos = OptRange.union p1 (NonBinding.location atom2) in
                NonBinding.Operator (pos, name, [ [ atom1 ]; [ atom2 ] ]))
              atom
              atom
          in
          atom <|> unary_op <|> min_max
        in
        let pair p1 p2 = lift2 (fun x y -> x, y) p1 p2 in
        let mul_div : term Parsers.t =
          let op = char '*' <|> char '/' in
          let f l (op, r) =
            let rng = OptRange.union (NonBinding.location l) (NonBinding.location r) in
            match op with
            | '*' -> NonBinding.Operator (rng, "mul", [ [ l ]; [ r ] ])
            | '/' -> NonBinding.Operator (rng, "div", [ [ l ]; [ r ] ])
            | _ -> failwith "error: impossible operator"
          in
          application >>= fun init -> many (pair op application) >>| List.fold ~init ~f
        in
        let add_sub : term Parsers.t =
          let op = char '+' <|> char '-' in
          let f l (op, r) =
            let rng = OptRange.union (NonBinding.location l) (NonBinding.location r) in
            match op with
            | '+' -> NonBinding.Operator (rng, "add", [ [ l ]; [ r ] ])
            | '-' -> NonBinding.Operator (rng, "sub", [ [ l ]; [ r ] ])
            | _ -> failwith "error: impossible operator"
          in
          mul_div >>= fun init -> many (pair op mul_div) >>| List.fold ~init ~f
        in
        add_sub)
    <?> "parser"
  ;;
end

let rec interpret : term -> (ConstructiveReal.t, term * string) Result.t =
 fun tm ->
  let open Result.Let_syntax in
  match tm with
  | Operator (_, "lit", [ [ Primitive (_, real) ] ]) -> Ok real
  | Operator (_, name, []) when List.mem constants name ~equal:String.equal ->
    ConstructiveReal.(
      (match name with
      | "e" -> Ok e
      | "pi" -> Ok pi
      | _ -> Error (tm, "expected e or pi")))
  | Operator (_, name, [ [ l ]; [ r ] ])
    when List.mem binary_operators name ~equal:String.equal ->
    let%bind l' = interpret l in
    let%bind r' = interpret r in
    ConstructiveReal.(
      (match name with
      | "add" -> Ok (l' + r')
      | "sub" -> Ok (l' - r')
      | "mul" -> Ok (l' * r')
      | "div" -> Ok (l' / r')
      | "max" -> Ok (max l' r')
      | "min" -> Ok (min l' r')
      | _ -> Error (tm, "expected a binary operator")))
  | Operator (_, name, [ [ x ] ]) when List.mem unary_operators name ~equal:String.equal
    ->
    let%bind x' = interpret x in
    ConstructiveReal.(
      (match name with
      | "negate" -> Ok (negate x')
      | "sqrt" -> Ok (sqrt x')
      | "abs" -> Ok (abs x')
      | "exp" -> Ok (exp x')
      | "ln" -> Ok (ln x')
      | "sin" -> Ok (sin x')
      | "cos" -> Ok (cos x')
      | "tan" -> Ok (tan x')
      | "asin" -> Ok (asin x')
      | "acos" -> Ok (acos x')
      | "atan" -> Ok (atan x')
      | _ -> Error (tm, "expected a unary operator")))
  | _ -> Error (tm, "unexpected term")
;;

(* let%test_module "Parsing" = (module struct module ParseCore = Lvca_core.Core.Parse
   (ParseUtil.CComment) module ParseParser = Parser.Parse (ParseUtil.CComment)

   let parse_print : string -> unit = fun parser_str -> match ParseUtil.parse_string
   (ParseParser.whitespace_t ParseCore.term) parser_str with | Error msg ->
   print_endline ("failed to parse parser desc: " ^ msg) | Ok parser -> Fmt.pr "%a\n"
   Parser.pp parser ;;

   let parser_str = {| let constants = "e" | "pi" in let unary_operators = return {list(
   "negate"; "sqrt"; "abs"; "exp"; "ln"; "sin"; "cos"; "tan"; "asin"; "acos"; "atan"; )}
   in

   let binary_operators = return {list("add"; "sub"; "mul"; "div"; "max"; "min")} in //
   let integer_or_float_lit = fix (t -> let atom = constants | sequence (_. t. _. t) ['(',
   t, ')'] in // let unary_op = unquote { // } in // let application = atom | unary_op |
   min_max in // let mul_div = ... in // let add_sub = ... in // add_sub atom ) |} ;;

   let%expect_test _ = parse_print parser_str; [%expect]

   end) ;; *)

let%test_module "Evaluation" =
  (module struct
    module P = Parse (ParseUtil.CComment)

    let go str =
      match ParseUtil.parse_string P.t str with
      | Error msg -> print_endline msg
      | Ok tm ->
        (match interpret tm with
        | Error (_tm, msg) -> print_endline msg
        | Ok real -> print_endline @@ ConstructiveReal.eval_to_string real)
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

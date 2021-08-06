open Base
open Lvca_syntax
open Lvca_provenance
open Stdio

type term = Opt_range.t Nonbinding.term

let binary_operators = [ "add"; "sub"; "mul"; "div"; "max"; "min" ]

let unary_operators =
  [ "negate"; "sqrt"; "abs"; "exp"; "ln"; "sin"; "cos"; "tan"; "asin"; "acos"; "atan" ]
;;

let constants = [ "pi"; "e" ]

module Parse = struct
  open Lvca_parsing

  let lit : Opt_range.t Nonbinding.term Lvca_parsing.t =
    (* TODO: this fails on too-large float lits *)
    Ws.integer_or_float_lit
    >>~ fun range lit ->
    let lit =
      match lit with
      | Either.First str -> Primitive_impl.All_plain.Integer (Z.of_string str)
      | Second f -> Float f
    in
    Nonbinding.Operator (range, "lit", [ Primitive (range, lit) ])
  ;;

  let const =
    constants
    |> List.map ~f:Ws.string
    |> choice ~failure_msg:"looking for a constant name"
    >>~ fun range name -> Nonbinding.Operator (range, name, [])
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
                 >>== fun { value = name; range = p1; _ } ->
                 atom
                 >>| fun body ->
                 let pos = Opt_range.union p1 (Nonbinding.info body) in
                 Nonbinding.Operator (pos, name, [ body ]))
          |> choice ~failure_msg:"looking for a unary operator expression"
        in
        let application =
          let min_max =
            choice
              [ Ws.string "min"; Ws.string "max" ]
              ~failure_msg:"looking for min or max"
            >>== fun { value = name; range = p1; _ } ->
            lift2
              (fun atom1 atom2 ->
                let pos = Opt_range.union p1 (Nonbinding.info atom2) in
                Nonbinding.Operator (pos, name, [ atom1; atom2 ]))
              atom
              atom
          in
          atom <|> unary_op <|> min_max
        in
        let pair p1 p2 = lift2 (fun x y -> x, y) p1 p2 in
        let mul_div : term Lvca_parsing.t =
          let op = Ws.char '*' <|> Ws.char '/' in
          let f l (op, r) =
            let rng = Opt_range.union (Nonbinding.info l) (Nonbinding.info r) in
            match op with
            | '*' -> Nonbinding.Operator (rng, "mul", [ l; r ])
            | '/' -> Nonbinding.Operator (rng, "div", [ l; r ])
            | _ -> failwith "error: impossible operator"
          in
          application >>= fun init -> many (pair op application) >>| List.fold ~init ~f
        in
        let add_sub : term Lvca_parsing.t =
          let op = Ws.char '+' <|> Ws.char '-' in
          let f l (op, r) =
            let rng = Opt_range.union (Nonbinding.info l) (Nonbinding.info r) in
            match op with
            | '+' -> Nonbinding.Operator (rng, "add", [ l; r ])
            | '-' -> Nonbinding.Operator (rng, "sub", [ l; r ])
            | _ -> failwith "error: impossible operator"
          in
          mul_div >>= fun init -> many (pair op mul_div) >>| List.fold ~init ~f
        in
        add_sub)
    <?> "parser"
  ;;
end

let rec interpret : term -> (Constructive_real.t, term * string) Result.t =
 fun tm ->
  let open Result.Let_syntax in
  match tm with
  | Operator (_, "lit", [ Primitive (_, Integer i) ]) ->
    Ok (Constructive_real.of_bigint i)
  | Operator (_, "lit", [ Primitive (_, Float f) ]) -> Ok (Constructive_real.of_float f)
  | Operator (_, name, []) when List.mem constants name ~equal:String.equal ->
    Constructive_real.(
      (match name with
      | "e" -> Ok e
      | "pi" -> Ok pi
      | _ -> Error (tm, "expected e or pi")))
  | Operator (_, name, [ l; r ]) when List.mem binary_operators name ~equal:String.equal
    ->
    let%bind l' = interpret l in
    let%bind r' = interpret r in
    Constructive_real.(
      (match name with
      | "add" -> Ok (l' + r')
      | "sub" -> Ok (l' - r')
      | "mul" -> Ok (l' * r')
      | "div" -> Ok (l' / r')
      | "max" -> Ok (max l' r')
      | "min" -> Ok (min l' r')
      | _ -> Error (tm, "expected a binary operator")))
  | Operator (_, name, [ x ]) when List.mem unary_operators name ~equal:String.equal ->
    let%bind x' = interpret x in
    Constructive_real.(
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
   (Lvca_parsing.CComment) module ParseParser = Parser.Parse (Lvca_parsing.CComment)

   let parse_print : string -> unit = fun parser_str -> match Lvca_parsing.parse_string
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
    let go str =
      match Lvca_parsing.parse_string Parse.t str with
      | Error msg -> print_endline msg
      | Ok tm ->
        (match interpret tm with
        | Error (_tm, msg) -> print_endline msg
        | Ok real -> print_endline @@ Constructive_real.eval_to_string real)
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

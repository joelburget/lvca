(* TODO:
   [ ] check for JS keywords
 *)
open Base

module Rule = struct
  type t =
    | Str_lit of string
    | Re_lit of Regex.t
    | Symbol of string
    | Seq of t list
    | Choice of t list
    | Repeat of t
    | Repeat1 of t
    | Optional of t
    | Prec of int * t
    | Left of int option * t
    | Right of int option * t
    | Dynamic of int * t
    | Token of t
    | Immediate of t
    | Alias of string * t
    | Field of string * t

  let rec pp ppf =
    let open Fmt in
    function
    | Str_lit str -> pf ppf "%S" str
    | Re_lit re -> pf ppf "/%a/" Regex.pp re
    | Symbol name -> pf ppf "$.%s" name
    | Seq ts -> pf ppf "@[seq(%a)@]" (list ~sep:comma pp) ts
    | Choice ts -> pf ppf "@[choice(%a)@]" (list ~sep:comma pp) ts
    | Repeat t -> pf ppf "@[repeat(%a)@]" pp t
    | Repeat1 t -> pf ppf "@[repeat1(%a)@]" pp t
    | Optional t -> pf ppf "@[optional(%a)@]" pp t
    | Prec (i, t) -> pf ppf "@[prec(%d, %a)@]" i pp t
    | Left (i_opt, t) ->
      (match i_opt with
      | None -> pf ppf "@[left(%a)@]" pp t
      | Some i -> pf ppf "@[left(%d, %a)@]" i pp t)
    | Right (i_opt, t) ->
      (match i_opt with
      | None -> pf ppf "@[right(%a)@]" pp t
      | Some i -> pf ppf "@[right(%d, %a)@]" i pp t)
    | Dynamic (i, t) -> pf ppf "@[dynamic(%d, %a)@]" i pp t
    | Token t -> pf ppf "@[token(%a)@]" pp t
    | Immediate t -> pf ppf "@[token.immediate(%a)@]" pp t
    | Alias (name, t) -> pf ppf "@[alias(%a, %S)@]" pp t name
    | Field (name, t) -> pf ppf "@[field(%a, %S)@]" pp t name
  ;;

  let of_sort_syntax
      ~sort_def
      Concrete.Sort_syntax.
        { info = _; name = _, name; operators; variables; operator_ranking = _ (* TODO*) }
    =
    let (Sort_def.Sort_def (_vars, abstract_ops, _)) = sort_def in
    let choices =
      List.map
        operators
        ~f:(fun Concrete.Operator_syntax_row.{ pattern; concrete_syntax; _ } ->
          let op_name = pattern.name in
          let (Operator_def.Operator_def (_, _, Arity (_, valences))) =
            List.find_exn abstract_ops ~f:(fun (Operator_def.Operator_def (_, name, _)) ->
                String.(name = op_name))
          in
          let _, sequence_items = concrete_syntax in
          let sequence_items =
            List.filter_map sequence_items ~f:(function
                | Var (_, name) ->
                  let sort_slot = Concrete.find_var_sort pattern.slots valences name in
                  let sort =
                    match sort_slot with
                    | Sort_binding sort -> sort
                    | Sort_pattern { var_sort; _ } -> var_sort
                  in
                  Some (Symbol (Sort.name sort))
                | Literal (_, str) -> Some (Str_lit str)
                | Space _ -> None)
          in
          match sequence_items with
          | [] -> Lvca_util.invariant_violation [%here] "Empty sequence"
          | [ item ] -> item
          | _ -> Seq sequence_items)
    in
    let choices =
      match variables with
      | None -> choices
      | Some Concrete.Variable_syntax_row.{ re; _ } -> Re_lit re :: choices
    in
    let choices =
      match choices with
      | [] -> Lvca_util.invariant_violation [%here] "No choices"
      | [ choice ] -> choice
      | _ -> Choice choices
    in
    name, choices
  ;;
end

module Grammar = struct
  type t =
    { name : string
    ; rules : (string * Rule.t) list
    ; extras : string list option
    ; inline : string list option
    ; conflicts : string list list option
    ; externals : string list option
    ; word : string option
    ; supertypes : string list option
    }

  let opt_field name fmt =
    Option.map ~f:(fun v ppf _ -> Fmt.pf ppf "@[%s:@ %a@]" name fmt v)
  ;;

  let mk ?extras ?inline ?conflicts ?externals ?word ?supertypes name rules =
    { name; rules; extras; inline; conflicts; externals; word; supertypes }
  ;;

  let of_concrete ~abstract ~name sort_syntaxes =
    let rules =
      List.map sort_syntaxes ~f:(fun sort_syntax ->
          let _, sort_name = sort_syntax.Concrete.Sort_syntax.name in
          let sort_def = Map.find_exn abstract sort_name in
          Rule.of_sort_syntax ~sort_def sort_syntax)
    in
    mk name rules
  ;;

  let pp ppf t =
    let open Fmt in
    let pp_name ppf = pf ppf "%S" in
    let pp_rule ppf (name, rule) = pf ppf "%s: $ => %a" name Rule.pp rule in
    let pp_word ppf word = pf ppf "word: $ => $.%s" word in
    let pp_list pp = brackets (list ~sep:comma pp) in
    let fields =
      [ Some (field "name" (fun t -> t.name) pp_name)
      ; Some (field "rules" (fun t -> t.rules) (braces (list ~sep:comma pp_rule)))
      ; opt_field "word" pp_word t.word
      ; opt_field "extras" (pp_list string) t.extras
      ; opt_field "inline" (pp_list string) t.inline
      ; opt_field "conflicts" (pp_list (pp_list string)) t.conflicts
      ; opt_field "externals" (pp_list string) t.externals
      ; opt_field "supertypes" (pp_list string) t.externals
      ]
      |> List.filter_map ~f:Fn.id
    in
    pf ppf "@[module.exports = grammar({@,%a@,})@]" (record fields) t
  ;;
end

let%test_module _ =
  (module struct
    let grammar =
      Grammar.mk
        "javascript"
        [ ( "_expression"
          , Choice
              [ Symbol "identifier"
              ; Symbol "unary_expression"
              ; Symbol "binary_expression"
              ] )
        ; ( "binary_expression"
          , Choice
              [ Left
                  ( Some 1
                  , Seq
                      [ Symbol "_expression"; Str_lit "instanceof"; Symbol "_expression" ]
                  )
              ] )
        ; ( "unary_expression"
          , Choice [ Left (Some 1, Seq [ Str_lit "typeof"; Symbol "_expression" ]) ] )
        ; "identifier", Re_lit Regex.(Plus (Set [ Range ('a', 'z'); Single_char '_' ]))
        ]
    ;;

    let%expect_test _ =
      Grammar.pp Fmt.stdout grammar;
      [%expect
        {|
        module.exports = grammar({
        name: "javascript"
        rules:
         {_expression: $ => choice($.identifier, $.unary_expression,
                            $.binary_expression),
          binary_expression: $ => choice(left(1, seq($._expression, "instanceof",
                                                 $._expression))),
          unary_expression: $ => choice(left(1, seq("typeof", $._expression))),
          identifier: $ => /[a-z_]+/}
        }) |}]
    ;;

    let abstract_defn =
      {|expr :=
  | Add(expr; expr)
  | Mul(expr; expr)
  | Fun(val. expr)
  | Val(val)
  ;

val := True() | False();
      |}
    ;;

    let concrete_defn =
      {|expr:
  | Add(x; y) ~ x _ "+" _ y
  | Mul(x; y) ~ x _ "*" _ y
  | Fun(v. e) ~ "fun" _ v _ "->" _ e
  | Val(v)    ~ v
  | x         ~ /[a-z][a-zA-Z0-9_]*/
  \ ()"*" > ()"+"

val:
  | True()  ~ "true"
  | False() ~ "false"
  ;
|}
    ;;

    let mk_concrete = Lvca_parsing.parse_string_or_failwith Concrete.parse

    let mk_abstract str =
      match
        Lvca_parsing.parse_string_or_failwith Abstract_syntax.parse str
        |> Abstract_syntax.mk_unordered
      with
      | `Duplicate_key k -> failwith (Fmt.str "duplicate key %s" k)
      | `Ok Abstract_syntax.Unordered.{ sort_defs; _ } -> sort_defs
    ;;

    let abstract = mk_abstract abstract_defn
    let concrete = mk_concrete concrete_defn

    let%expect_test _ =
      let grammar = Grammar.of_concrete ~name:"test" ~abstract concrete in
      Grammar.pp Fmt.stdout grammar;
      [%expect
        {|
        module.exports = grammar({
        name: "test"
        rules:
         {expr: $ => choice(/[a-z][a-zA-Z0-9_]*/, seq($.expr, "+", $.expr),
                     seq($.expr, "*", $.expr), seq("fun", $.val, "->", $.expr), $.val),
          val: $ => choice("true", "false")}
        }) |}]
    ;;
  end)
;;

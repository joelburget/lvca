(* TODO:
   [ ] check for JS keywords
 *)
open Base
open Lvca_syntax
open Lvca_util

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
    let app1 name pp ppf v = pf ppf "@[<hov>%s(%a)@]" name pp v in
    let app2 name pp1 v1 pp2 v2 = pf ppf "@[<hov>%s(%a, %a)@]" name pp1 v1 pp2 v2 in
    let string ppf str = pf ppf "%S" str in
    function
    | Str_lit str -> string ppf str
    | Re_lit re -> pf ppf "/%a/" Regex.pp re
    | Symbol name -> pf ppf "$.%s" name
    | Seq ts -> app1 "seq" (list ~sep:comma pp) ppf ts
    | Choice ts -> app1 "choice" (list ~sep:comma pp) ppf ts
    | Repeat t -> app1 "repeat" pp ppf t
    | Repeat1 t -> app1 "repeat1" pp ppf t
    | Optional t -> app1 "optional" pp ppf t
    | Prec (i, t) -> app2 "prec" int i pp t
    | Left (i_opt, t) ->
      (match i_opt with None -> app1 "left" pp ppf t | Some i -> app2 "left" int i pp t)
    | Right (i_opt, t) ->
      (match i_opt with
      | None -> app1 "right" pp ppf t
      | Some i -> app2 "right" int i pp t)
    | Dynamic (i, t) -> app2 "dynamic" int i pp t
    | Token t -> app1 "token" pp ppf t
    | Immediate t -> app1 "token.immediate" pp ppf t
    | Alias (name, t) -> app2 "alias" pp t string name
    | Field (name, t) -> app2 "field" pp t string name
  ;;

  let get_binary_op_info operator_names row levels =
    let open Concrete in
    let open Option.Let_syntax in
    let%bind _, op_name, _ =
      Operator_concrete_syntax_row.is_binary_operator
        ~operator_names
        row.Operator_syntax_row.concrete_syntax
    in
    let num_levels = List.length levels in
    List.find_mapi levels ~f:(fun i fixities ->
        let%map _, fixity, _ =
          List.find fixities ~f:(fun (_, _, op_name') -> String.(op_name' = op_name))
        in
        num_levels - i, fixity)
  ;;

  let of_sort_syntax ~sort_def sort_syntax =
    let open Concrete in
    let (Sort_def.Sort_def (_vars, abstract_ops, _)) = sort_def in
    let Sort_syntax.{ info = _; name = _, name; operators; variables; operator_ranking } =
      sort_syntax
    in
    let operator_names =
      sort_syntax |> Sort_syntax.operator_infos |> Sort_syntax.operator_names
    in
    let choices =
      List.map operators ~f:(fun row ->
          let Operator_syntax_row.{ pattern; concrete_syntax; _ } = row in
          let (Operator_def.Operator_def (_, _, Arity (_, valences))) =
            List.find abstract_ops ~f:(fun (Operator_def.Operator_def (_, name, _)) ->
                String.(name = pattern.name))
            |> Option.get_invariant [%here] (fun () ->
                   Fmt.str "Operator %s must appear in the abstract syntax" pattern.name)
          in
          let _, sequence_items = concrete_syntax in
          let sequence_items =
            List.filter_map sequence_items ~f:(function
                | Var (_, name) ->
                  let sort_slot = find_var_sort pattern.slots valences name in
                  let sort =
                    match sort_slot with
                    | Sort_binding sort -> sort
                    | Sort_pattern { var_sort; _ } -> var_sort
                  in
                  Some (Symbol (Sort.name sort))
                | Literal (_, str) -> Some (Str_lit str)
                | Space _ -> None)
          in
          let rule =
            match sequence_items with
            | [] -> Lvca_util.invariant_violation [%here] "Empty sequence"
            | [ item ] -> item
            | _ -> Seq sequence_items
          in
          let operator_ranking =
            match operator_ranking with None -> [] | Some (_prov, ranking) -> ranking
          in
          match get_binary_op_info operator_names row operator_ranking with
          | None -> rule
          | Some (prec, fixity) ->
            (match fixity with
            | Fixity.Left -> Left (Some prec, rule)
            | None -> Prec (prec, rule)
            | Right -> Right (Some prec, rule)))
    in
    let choices =
      match variables with
      | None -> choices
      | Some Variable_syntax_row.{ re; _ } -> Re_lit re :: choices
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

  let of_language ~name ~abstract ~concrete =
    let rules =
      List.map concrete ~f:(fun sort_syntax ->
          let _, sort_name = sort_syntax.Concrete.Sort_syntax.name in
          let sort_def =
            Map.find abstract sort_name
            |> Option.get_invariant [%here] (fun () ->
                   Fmt.str "Sort %s must appear in the abstract syntax" sort_name)
          in
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
    pf ppf "@[<hov 2>module.exports = grammar({@,%a@,})@]" (record ~sep:comma fields) t
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
          name: "javascript",
          rules:
           {_expression: $ => choice($.identifier, $.unary_expression,
                              $.binary_expression),
            binary_expression: $ => choice(left(1, seq($._expression, "instanceof",
                                                   $._expression))),
            unary_expression: $ => choice(left(1, seq("typeof", $._expression))),
            identifier: $ => /[a-z_]+/}}) |}]
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
      let grammar = Grammar.of_language ~name:"test" ~abstract ~concrete in
      Grammar.pp Fmt.stdout grammar;
      [%expect
        {|
        module.exports = grammar({
          name: "test",
          rules:
           {expr: $ => choice(/[a-z][a-zA-Z0-9_]*/,
                       left(1, seq($.expr, "+", $.expr)),
                       left(2, seq($.expr, "*", $.expr)),
                       seq("fun", $.val, "->", $.expr), $.val),
            val: $ => choice("true", "false")}}) |}]
    ;;
  end)
;;

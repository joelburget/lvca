(* Decisions:
 * - always allow parens
 * - common whitespace definition
 *   - how to define required / optional?
 * - how to define boxes?
 *
 * TODO:
 * [ ] actually parse / pretty-print with a term
 * [x] check validity
 * [ ] multiple concrete syntaxes mapping to the same abstract
 * [ ] operator ranking
 * [x] whitespace
 *)
open Base
open Lvca_provenance
open Lvca_util

let pp_set = Fmt.(braces (list string ~sep:comma))
let reserved_words = Lvca_util.String.Set.empty
let lower_ident = Lvca_parsing.C_comment_parser.lower_identifier reserved_words
let upper_ident = Lvca_parsing.C_comment_parser.upper_identifier reserved_words

module Sequence_item = struct
  type t =
    | Var of Provenance.t * string
    | Literal of Provenance.t * string
    | Space of Provenance.t

  let info = function Var (i, _) | Literal (i, _) | Space i -> i
  let vars = function Var (_, v) -> [ v ] | Literal _ | Space _ -> []

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    choice
      ~failure_msg:"looking for a variable or literal"
      [ (char '_' >>~ fun range _ -> Space (Provenance.of_range range))
      ; (lower_ident >>~ fun range str -> Var (Provenance.of_range range, str))
      ; (string_lit >>~ fun range str -> Literal (Provenance.of_range range, str))
      ]
    <?> "sequence item"
  ;;

  let pp ppf t =
    let info = info t in
    Provenance.open_stag ppf info;
    (match t with
    | Var (_, name) -> Fmt.string ppf name
    | Literal (_, str) -> Fmt.pf ppf "%S" str
    | Space _ -> Fmt.pf ppf "_");
    Provenance.close_stag ppf info
  ;;
end

module Operator_concrete_syntax = struct
  type t = Provenance.t * Sequence_item.t list

  let info (i, _) = i
  let vars (_, items) = items |> List.map ~f:Sequence_item.vars |> List.concat

  let parse =
    let open Lvca_parsing in
    many1 Sequence_item.parse
    >>~ (fun range items -> Provenance.of_range range, items)
    <?> "operator concrete syntax"
  ;;

  let pp ppf (info, sequence_items) =
    Provenance.open_stag ppf info;
    Fmt.(pf ppf "@[%a@]" (list Sequence_item.pp ~sep:sp) sequence_items);
    Provenance.close_stag ppf info
  ;;
end

module Variable_syntax_row = struct
  type t =
    { info : Provenance.t
    ; var_name : string
    ; re : Regex.t
    }

  let info t = t.info

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      lift2
        (fun (ident_range, var_name) (re_range, re) ->
          let info = Opt_range.union ident_range re_range |> Provenance.of_range in
          { info; var_name; re })
        (attach_pos' lower_ident <* char '~')
        (attach_pos' (char '/' *> of_angstrom Regex.parse <* char '/'))
    in
    p <?> "variable regex"
  ;;

  let pp ppf { info; var_name; re } =
    Provenance.open_stag ppf info;
    Fmt.pf ppf "%s ~ /%a/" var_name Regex.pp re;
    Provenance.close_stag ppf info
  ;;
end

module Operator_pattern_slot = struct
  type t =
    { info : Provenance.t
    ; variable_names : string list
    ; body_name : string
    }

  let vars { variable_names; body_name; _ } = body_name :: variable_names

  let pp ppf { info; variable_names; body_name } =
    Provenance.open_stag ppf info;
    Fmt.(
      pf ppf "@[%a@]" (list string ~sep:(any ".@ ")) (List.snoc variable_names body_name));
    Provenance.close_stag ppf info
  ;;

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      attach_pos' (sep_end_by1 (char '.') lower_ident)
      >>| fun (pos, idents) ->
      let variable_names, body_name = List.unsnoc idents in
      { info = Provenance.of_range pos; variable_names; body_name }
    in
    p <?> "operator pattern slot"
  ;;
end

module Operator_pattern = struct
  type t =
    { info : Provenance.t
    ; name : string
    ; slots : Operator_pattern_slot.t list
    }

  let vars { slots; _ } = slots |> List.map ~f:Operator_pattern_slot.vars |> List.concat

  let pp ppf { info; name; slots } =
    Provenance.open_stag ppf info;
    Fmt.(pf ppf "@[%s(%a)@]" name (list Operator_pattern_slot.pp ~sep:semi) slots);
    Provenance.close_stag ppf info
  ;;

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      lift2
        (fun (name_range, name) (slots_range, slots) ->
          let info = Opt_range.union name_range slots_range |> Provenance.of_range in
          { info; name; slots })
        (attach_pos' upper_ident)
        (attach_pos' (parens (sep_by (char ';') Operator_pattern_slot.parse)))
    in
    p <?> "operator pattern"
  ;;
end

module Operator_syntax_row = struct
  type t =
    { info : Provenance.t
    ; pattern : Operator_pattern.t
    ; concrete_syntax : Operator_concrete_syntax.t
    }

  let info t = t.info

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      lift2
        (fun (pat_range, pattern) (syntax_range, concrete_syntax) ->
          let range = Opt_range.union pat_range syntax_range in
          let info = Provenance.of_range range in
          { info; pattern; concrete_syntax })
        (* TODO: don't parse any pattern -- just operator patterns *)
        (attach_pos' Operator_pattern.parse <* char '~')
        (attach_pos' Operator_concrete_syntax.parse)
    in
    p <?> "operator syntax row"
  ;;

  let pp ppf { info; pattern; concrete_syntax } =
    Provenance.open_stag ppf info;
    Fmt.pf
      ppf
      "%a ~ %a"
      Operator_pattern.pp
      pattern
      Operator_concrete_syntax.pp
      concrete_syntax;
    Provenance.close_stag ppf info
  ;;
end

module Operator_syntax = struct
  type t = (Operator_syntax_row.t, Variable_syntax_row.t) Either.t

  let info = function
    | Either.First row -> Operator_syntax_row.info row
    | Second row -> Variable_syntax_row.info row
  ;;

  let parse =
    let open Lvca_parsing in
    choice
      ~failure_msg:"looking for an operator or variable syntax row"
      [ (Variable_syntax_row.parse >>| fun row -> Either.Second row)
      ; (Operator_syntax_row.parse >>| fun row -> Either.First row)
      ]
    <?> "operator syntax"
  ;;

  let pp ppf = function
    | Either.First row -> Operator_syntax_row.pp ppf row
    | Second row -> Variable_syntax_row.pp ppf row
  ;;
end

module Sort_syntax = struct
  type t =
    { info : Provenance.t
    ; name : Provenance.t * string
    ; operators : Operator_syntax_row.t list
    ; variables : Variable_syntax_row.t option
    }

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      attach_pos' lower_ident
      >>= fun (name_info, name) ->
      char ':'
      >>= fun _ ->
      attach_pos' (many (char '|' *> Operator_syntax.parse))
      >>= fun (ops_info, operators) ->
      let range = Opt_range.union name_info ops_info in
      let info = Provenance.of_range range in
      let name = Provenance.of_range name_info, name in
      let operators, vars = List.partition_map operators ~f:Fn.id in
      match vars with
      | [] -> return ~range { info; name; operators; variables = None }
      | [ row ] -> return ~range { info; name; operators; variables = Some row }
      | _ -> fail "Only one variable row is allowed in a sort"
    in
    p <?> "sort syntax"
  ;;

  let pp_name ppf (info, name) =
    Provenance.open_stag ppf info;
    Fmt.string ppf name;
    Provenance.close_stag ppf info
  ;;

  let pp_row ppf = Fmt.pf ppf "| %a" Operator_syntax.pp

  let pp ppf { info; name; operators; variables } =
    let open Fmt in
    let operators = List.map operators ~f:(fun row -> Either.First row) in
    let operators =
      match variables with
      | None -> operators
      | Some row -> List.snoc operators (Either.Second row)
    in
    Provenance.open_stag ppf info;
    pf ppf "@[<v 2>@[<h>%a:@]@;%a@]" pp_name name (list pp_row) operators;
    Provenance.close_stag ppf info
  ;;
end

type ordered_t = Sort_syntax.t list

type unordered_t =
  (Operator_syntax_row.t list * Variable_syntax_row.t option) String.Map.t

let compile ordered =
  ordered
  |> List.map ~f:(fun Sort_syntax.{ info = _; name = _, name; operators; variables } ->
         name, (operators, variables))
  |> String.Map.of_alist
;;

let parse = Lvca_parsing.(many1 Sort_syntax.parse <?> "parse / pretty definition")
let pp = Fmt.(list Sort_syntax.pp ~sep:(any "@.@."))

(* Check
 * [x] No overlapping variable definitions (on rhs or lhs)
 * [x] Variables used 1-1 (lhs vs rhs)
 * [x] 1-1 mapping between sorts given an abstract / concrete syntax
 * [x] every operator in a sort given a concrete syntax
 * [x] variable given concrete syntax iff allowed
 * [ ] space hygiene:
 *   - no repeated spaces
 *   - no leading / trailing spaces
 *)
let check sort_defs ordered =
  let sort = List.sort ~compare:String.compare in
  match compile ordered with
  | `Duplicate_key k -> Some (Fmt.str "duplicate sort definition for %s" k)
  | `Ok _compiled ->
    let operator_syntax_row
        sort_name
        Operator_syntax_row.{ info = _; pattern; concrete_syntax }
      =
      let lhs_vars = pattern |> Operator_pattern.vars |> sort in
      let rhs_vars = concrete_syntax |> Operator_concrete_syntax.vars |> sort in
      match
        ( List.find_consecutive_duplicate ~equal:String.( = ) lhs_vars
        , List.find_consecutive_duplicate ~equal:String.( = ) lhs_vars )
      with
      | None, None ->
        if List.equal String.( = ) lhs_vars rhs_vars
        then None
        else
          Some
            Fmt.(
              str
                "LHS and RHS don't bind the same vars (%a vs %a)"
                pp_set
                lhs_vars
                pp_set
                rhs_vars)
      | Some (dupe, _), _ ->
        Some
          (Fmt.str
             "Duplicate variable found in abstract pattern for sort %s / operator %s: %s"
             sort_name
             pattern.name
             dupe)
      | _, Some (dupe, _) ->
        Some
          (Fmt.str
             "Duplicate variable found in concrete pattern for sort %s / operator %s: %s"
             sort_name
             pattern.name
             dupe)
    in
    let sort_syntax
        Sort_syntax.{ info = _; operators = concrete_ops; name = _, sort_name; variables }
      =
      let (Sort_def.Sort_def (_vars, abstract_ops, var_names)) =
        Map.find_exn sort_defs sort_name
      in
      match variables, var_names with
      | None, _ :: _ ->
        Some "Abstract syntax defines variables but concrete syntax doesn't"
      | Some _, [] -> Some "Concrete syntax defines variables but abstract syntax doesn't"
      | _ ->
        let abstract_op_names =
          abstract_ops
          |> List.map ~f:(fun (Operator_def.Operator_def (_, name, _)) -> name)
          |> sort
        in
        let concrete_op_names =
          concrete_ops
          |> List.map ~f:(function Operator_syntax_row.{ pattern = { name; _ }; _ } ->
                 name)
          |> sort
        in
        if not (List.equal String.( = ) abstract_op_names concrete_op_names)
        then
          Some
            Fmt.(
              str
                "Concrete syntax definition for sort %s doesn't have the same operators \
                 (%a) as the abstract syntax (%a)"
                sort_name
                pp_set
                concrete_op_names
                pp_set
                abstract_op_names)
        else List.find_map concrete_ops ~f:(operator_syntax_row sort_name)
    in
    let abstract_sort_names = sort_defs |> Map.keys |> sort in
    let concrete_sort_names =
      ordered |> List.map ~f:(fun Sort_syntax.{ name = _, name; _ } -> name) |> sort
    in
    if List.equal String.( = ) abstract_sort_names concrete_sort_names
    then List.find_map ordered ~f:sort_syntax
    else
      Some
        Fmt.(
          str
            "Concrete syntax definition doesn't have the same sorts (%a) as abstract \
             syntax (%a)"
            pp_set
            concrete_sort_names
            pp_set
            abstract_sort_names)
;;

module Pp_term = struct
  let pp_term sorts start_sort ppf tm =
    let rec operator_row
        Operator_syntax_row.
          { info = _
          ; pattern = { info = _; name; slots }
          ; concrete_syntax = _, sequence_items
          }
        tm
      =
      match tm with
      | Nominal.Term.Var _ ->
        Lvca_util.invariant_violation ~here:[%here] "operator_row doesn't handle vars"
      | Primitive prim -> Ok (Some (Primitive.All.pp ppf prim))
      | Operator (_, op_name, scopes) ->
        if String.(name <> op_name)
        then Ok None
        else (
          match List.zip slots scopes with
          | Ok slotscopes ->
            let open Result.Let_syntax in
            let%bind var_mappings =
              slotscopes
              |> List.map
                   ~f:(fun ({ info = _; variable_names; body_name }, Scope (pats, body))
                      ->
                     match
                       List.zip
                         variable_names
                         (List.map pats ~f:(fun p -> Either.First p))
                     with
                     | Unequal_lengths -> Error "TODO"
                     | Ok varpats ->
                       Ok
                         (varpats
                         |> String.Map.of_alist_exn
                         |> Map.set ~key:body_name ~data:(Either.Second body)))
              |> Result.all
            in
            let var_mapping = String.Map.unions_left_biased var_mappings in
            Stdlib.Format.pp_open_hvbox ppf 2;
            List.iter sequence_items ~f:(function
                | Var (_, name) ->
                  (match Map.find_exn var_mapping name with
                  | First pat -> pat |> Nominal.Term.of_pattern |> go
                  | Second tm -> go tm)
                | Literal (_, str) -> Fmt.string ppf str
                | Space _ -> Fmt.sp ppf ());
            Stdlib.Format.pp_close_box ppf ();
            Ok (Some ())
          | Unequal_lengths -> Error "TODO")
    and go tm =
      match tm with
      | Nominal.Term.Var (_, var_name) -> Fmt.string ppf var_name
      | _ ->
        let operator_syntaxes : Operator_syntax_row.t list =
          match Map.find sorts start_sort with
          | Some (operator_syntax_rows, _) -> operator_syntax_rows
          | None ->
            Lvca_util.invariant_violation
              ~here:[%here]
              (Fmt.str "didn't find expected operator %S" start_sort)
        in
        (match
           List.find_map operator_syntaxes ~f:(fun row ->
               match operator_row row tm with Error _ -> None | Ok v -> v)
         with
        | None ->
          Lvca_util.invariant_violation
            ~here:[%here]
            (Fmt.str "Didn't find matching operator syntax for `%a`" Nominal.Term.pp tm)
        | Some () -> ())
    in
    go tm
  ;;
end

module Parse_term = struct
  open Lvca_parsing

  let build_slot env Operator_pattern_slot.{ info = _; variable_names; body_name } =
    let pats =
      List.map variable_names ~f:(fun name ->
          match Hashtbl.find_exn env name with
          | Nominal.Term.Var (info, name) -> Pattern.Var (info, name)
          | _ -> failwith "TODO build_slot non-var")
    in
    Nominal.Scope.Scope (pats, Hashtbl.find_exn env body_name)
  ;;

  let build_operator ?input rng env Operator_pattern.{ info = _; name; slots } =
    Nominal.Term.Operator
      (Provenance.of_range ?input rng, name, List.map slots ~f:(build_slot env))
  ;;

  let sequence_items ?input var_parsers pattern =
    let open C_comment_parser in
    let env = Hashtbl.create (module String) in
    let rec go rng = function
      | [] -> return (build_operator ?input rng env pattern)
      | item :: items ->
        (match item with
        | Sequence_item.Space _ -> go rng items
        | Literal (_, str) ->
          attach_pos' (string str)
          >>= fun (rng', _) -> go (Opt_range.union rng rng') items
        | Var (_, name) ->
          attach_pos' (Map.find_exn var_parsers name)
          >>= fun (rng', data) ->
          Hashtbl.set env ~key:name ~data;
          go (Opt_range.union rng rng') items)
    in
    go None
  ;;

  type dispatch = { map : (dispatch -> Nominal.Term.t Lvca_parsing.t) String.Map.t }

  let sort
      sort_name
      (self : Nominal.Term.t Lvca_parsing.t)
      (dispatch : dispatch)
      (sort : Sort.t)
    =
    let sort_name', _ = Sort.split sort in
    if String.(sort_name' = sort_name)
    then self
    else Map.find_exn dispatch.map sort_name' dispatch
  ;;

  let operator_syntax_row
      ?input
      sort_name
      self
      dispatch
      (Operator_def.Operator_def (_, name, Arity (_, valences)))
      Operator_syntax_row.{ info = _; pattern; concrete_syntax = _, items }
    =
    let sort = sort sort_name self dispatch in
    let var_parsers =
      List.zip_exn pattern.slots valences
      |> List.bind
           ~f:(fun
                ( Operator_pattern_slot.{ info = _; variable_names; body_name }
                , Valence (sort_slots, sort') )
              ->
             let slot_parsers : Nominal.Term.t t list =
               List.map sort_slots ~f:(function
                   | Sort_binding s -> sort s
                   | Sort_pattern _ -> failwith "TODO: operator_syntax_row Sort_pattern")
             in
             (body_name, sort sort') :: List.zip_exn variable_names slot_parsers)
      |> String.Map.of_alist_exn
    in
    sequence_items ?input var_parsers pattern items <?> name
  ;;

  let sort_syntax
      ?input
      sort_name
      self
      dispatch
      (Sort_def.Sort_def (_, operator_defs, _vars))
      (operators, variables)
    =
    let operator_names =
      List.map operators ~f:(fun Operator_syntax_row.{ pattern = { name; _ }; _ } -> name)
    in
    let failure_msg =
      match variables with
      | None ->
        Fmt.str "looking for an operator from `%s` (%a)" sort_name pp_set operator_names
      | Some _ ->
        Fmt.str
          "looking for an operator from `%s` (%a) or a variable"
          sort_name
          pp_set
          operator_names
    in
    let choices =
      List.zip_exn operator_defs operators
      |> List.map ~f:(fun (op_def, op) ->
             operator_syntax_row ?input sort_name self dispatch op_def op)
    in
    let choices =
      match variables with
      | None -> choices
      | Some _ ->
        let var_parser =
          lower_ident >>~ fun pos name -> Nominal.Term.Var (Provenance.of_range pos, name)
        in
        List.snoc choices var_parser
    in
    (* Avoid going through `choice` if possible for better error messages *)
    let p =
      match choices with
      | [] -> fail (Fmt.str "no `%s` operators" sort_name)
      | [ p ] -> p
      | _ -> choice ~failure_msg choices
    in
    p <?> sort_name
  ;;

  (* TODO: Take input later -- shouldn't rebuild parser when it changes *)
  let lang ?input abstract_syntax concrete_syntax =
    let map =
      Map.mapi abstract_syntax ~f:(fun ~key:sort_name ~data:abstract_sort dispatch ->
          fix
          @@ fun self ->
          let concrete_sort = Map.find_exn concrete_syntax sort_name in
          sort_syntax ?input sort_name self dispatch abstract_sort concrete_sort)
    in
    { map }
  ;;
end

let%test_module "parsing / pretty-printing" =
  (module struct
    let () = Stdlib.Format.set_tags false

    let%test_module "Sequence_item" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Sequence_item.parse
        let parse_print = parse >> Sequence_item.pp Fmt.stdout

        let%expect_test _ =
          parse_print "a";
          [%expect {|a|}]
        ;;

        let%expect_test _ =
          parse_print {|"foo"|};
          [%expect {|"foo"|}]
        ;;
      end)
    ;;

    let%test_module "Operator_concrete_syntax" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_concrete_syntax.parse
        let parse_print = parse >> Operator_concrete_syntax.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|x "+" y|};
          [%expect {|x "+" y|}]
        ;;
      end)
    ;;

    let%test_module "Operator_pattern_slot" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_pattern_slot.parse
        let parse_print = parse >> Operator_pattern_slot.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|x|};
          [%expect {|x|}]
        ;;

        let%expect_test _ =
          parse_print {|x. y. z|};
          [%expect {|x. y. z|}]
        ;;
      end)
    ;;

    let%test_module "Operator_pattern" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_pattern.parse
        let parse_print = parse >> Operator_pattern.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|Add(x; y)|};
          [%expect {|Add(x; y)|}]
        ;;
      end)
    ;;

    let%test_module "Operator_syntax_row" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_syntax_row.parse
        let parse_print = parse >> Operator_syntax_row.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|Add(x; y) ~ x "+" y|};
          [%expect {|Add(x; y) ~ x "+" y|}]
        ;;
      end)
    ;;

    let%test_module "Variable_syntax_row" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Variable_syntax_row.parse
        let parse_print = parse >> Variable_syntax_row.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|x ~ /[a-z][a-zA-Z0-9_]*/|};
          [%expect {|x ~ /[a-z][a-zA-Z0-9_]*/|}]
        ;;
      end)
    ;;

    let%test_module "Operator_syntax" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_syntax.parse
        let parse_print = parse >> Operator_syntax.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|Add(x; y) ~ x "+" y|};
          [%expect {|Add(x; y) ~ x "+" y|}]
        ;;

        let%expect_test _ =
          parse_print {|x ~ /[a-z][a-zA-Z0-9_]*/|};
          [%expect {|x ~ /[a-z][a-zA-Z0-9_]*/|}]
        ;;
      end)
    ;;

    let%test_module "Sort_syntax" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Sort_syntax.parse
        let parse_print = parse >> Sort_syntax.pp Fmt.stdout

        let expr_defn =
          {|expr:
  | Add(x; y) ~ x "+" y
  | Mul(x; y) ~ x "*" y
  | x         ~ /[a-z][a-zA-Z0-9_]*/
          |}
        ;;

        let%expect_test _ =
          parse_print expr_defn;
          [%expect
            {|
            expr:
              | Add(x; y) ~ x "+" y
              | Mul(x; y) ~ x "*" y
              | x ~ /[a-z][a-zA-Z0-9_]*/
              |}]
        ;;
      end)
    ;;

    let parse = Lvca_parsing.parse_string_or_failwith parse
    let parse_print str = pp Fmt.stdout (parse str)

    let lang_abstract_defn =
      {|expr :=
  | Add(expr; expr)
  | Mul(expr; expr)
  | Fun(val. expr)
  | Val(val)

val := True() | False()
      |}
    ;;

    let lang_concrete_defn =
      {|expr:
  | Add(x; y) ~ "+" _ x _ y
  | Mul(x; y) ~ "*" _ x _ y
  | Fun(v. e) ~ "fun" _ v _ "->" _ e
  | Val(v)    ~ v
  | x         ~ /[a-z][a-zA-Z0-9_]*/

val:
  | True()  ~ "true"
  | False() ~ "false"
  | x       ~ /[a-z][a-zA-Z0-9_]*/
|}
    ;;

    let%expect_test _ =
      parse_print lang_concrete_defn;
      [%expect
        {|
        expr:
          | Add(x; y) ~ "+" _ x _ y
          | Mul(x; y) ~ "*" _ x _ y
          | Fun(v. e) ~ "fun" _ v _ "->" _ e
          | Val(v) ~ v
          | x ~ /[a-z][a-zA-Z0-9_]*/

        val:
          | True() ~ "true"
          | False() ~ "false"
          | x ~ /[a-z][a-zA-Z0-9_]*/
          |}]
    ;;

    let lang_concrete =
      match lang_concrete_defn |> parse |> compile with
      | `Duplicate_key k -> failwith (Fmt.str "Unexpected duplicate key %s" k)
      | `Ok lang -> lang
    ;;

    let%test_module "pp_term" =
      (module struct
        let go sort tm = Pp_term.pp_term lang_concrete sort Fmt.stdout tm
        let here = Provenance.of_here [%here]

        let%expect_test _ =
          let tm =
            Nominal.Term.Operator
              ( here
              , "Add"
              , [ Scope ([], Primitive (here, Integer Z.one))
                ; Scope ([], Primitive (here, Integer Z.one))
                ] )
          in
          go "expr" tm;
          [%expect {|+ 1 1|}]
        ;;

        let%expect_test _ =
          let tm =
            Nominal.Term.Operator
              (here, "Fun", [ Scope ([ Var (here, "x") ], Var (here, "x")) ])
          in
          go "expr" tm;
          [%expect {|fun x -> x|}]
        ;;

        let%expect_test _ =
          go "expr" (Nominal.Term.Var (here, "z"));
          [%expect {|z|}]
        ;;

        let%expect_test _ =
          go "val" (Nominal.Term.Operator (here, "True", []));
          [%expect {|true|}]
        ;;

        let%expect_test _ =
          go "val" (Nominal.Term.Operator (here, "False", []));
          [%expect {|false|}]
        ;;
      end)
    ;;

    let%test_module "Parse_term" =
      (module struct
        let lang_abstract =
          match
            Lvca_parsing.parse_string_or_failwith Abstract_syntax.parse lang_abstract_defn
            |> Abstract_syntax.mk_unordered
          with
          | `Duplicate_key k -> failwith (Fmt.str "duplicate key %s" k)
          | `Ok Abstract_syntax.Unordered.{ sort_defs; _ } -> sort_defs
        ;;

        let parser =
          Parse_term.lang ~input:(Buffer_name "test") lang_abstract lang_concrete
        ;;

        let parse sort =
          Lvca_parsing.parse_string_or_failwith (Map.find_exn parser.map sort parser)
        ;;

        let parse_print start_sort = parse start_sort >> Nominal.Term.pp Fmt.stdout

        let () =
          Stdlib.Format.set_formatter_stag_functions Provenance.stag_functions;
          Stdlib.Format.set_tags true;
          Stdlib.Format.set_mark_tags true
        ;;

        let%expect_test _ =
          parse_print "val" "true";
          [%expect
            {|<{ input = Buffer_name "test"; range = {0,4} }>True()</{ input = Buffer_name "test"; range = {0,4} }>|}]
        ;;

        let%expect_test _ =
          parse_print "val" "false";
          [%expect
            {|<{ input = Buffer_name "test"; range = {0,5} }>False()</{ input = Buffer_name "test"; range = {0,5} }>|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "+ x x";
          [%expect
            {|<{ input = Buffer_name "test"; range = {0,5} }>Add(<{ input = Buffer_name "test"; range = {2,3} }>Val(<{ input = Input_unknown; range = {2,3} }>x</{ input = Input_unknown; range = {2,3} }>)</{ input = Buffer_name "test"; range = {2,3} }>; <{ input = Buffer_name "test"; range = {4,5} }>Val(<{ input = Input_unknown; range = {4,5} }>x</{ input = Input_unknown; range = {4,5} }>)</{ input = Buffer_name "test"; range = {4,5} }>)</{ input = Buffer_name "test"; range = {0,5} }>|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "fun x -> x";
          [%expect
            {|<{ input = Buffer_name "test"; range = {0,10} }>Fun(<{ input = Input_unknown; range = {4,5} }>x</{ input = Input_unknown; range = {4,5} }>. <{ input = Buffer_name "test"; range = {9,10} }>Val(<{ input = Input_unknown; range = {9,10} }>x</{ input = Input_unknown; range = {9,10} }>)</{ input = Buffer_name "test"; range = {9,10} }>)</{ input = Buffer_name "test"; range = {0,10} }>|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "true";
          [%expect
            {|<{ input = Buffer_name "test"; range = {0,4} }>Val(<{ input = Buffer_name "test"; range = {0,4} }>True()</{ input = Buffer_name "test"; range = {0,4} }>)</{ input = Buffer_name "test"; range = {0,4} }>|}]
        ;;

        let () =
          Stdlib.Format.set_tags false;
          Stdlib.Format.set_mark_tags false
        ;;
      end)
    ;;
  end)
;;

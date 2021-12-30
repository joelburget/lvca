(* Decisions:
 * - always allow parens
 * - common whitespace definition
 *   - how to define required / optional?
 * - how to define boxes?
 *
 * TODO:
 * [x] check validity
 * [x] operator ranking
 * [x] variables colliding with keywords
 * [x] do all operators in the same level have to have the same fixity?
 * [ ] multiple concrete syntaxes mapping to the same abstract
 * [ ] whitespace
 *)
open Base
open Lvca_provenance
open Lvca_util
module Directed_graph = Directed_graph.Make (String)

let pp_set = Fmt.(braces (list string ~sep:comma))
let reserved_words = Lvca_util.String.Set.empty
let lower_ident = Lvca_parsing.C_comment_parser.lower_identifier reserved_words
let upper_ident = Lvca_parsing.C_comment_parser.upper_identifier reserved_words
let string_sort = List.sort ~compare:String.compare

(*
 * [ ] space hygiene:
 *   - no repeated spaces
 *   - no leading / trailing spaces
 * [ ] every operator has a line
 * [ ] will var parser be clobbered by left-recursion deferring to a var parser
     from another sort? Is this a grammar problem?
 *)

module Fixity = struct
  type t =
    | Left
    | None
    | Right

  let ( = ) t1 t2 =
    match t1, t2 with Left, Left | None, None | Right, Right -> true | _ -> false
  ;;

  let pp ppf = function
    | Left -> Fmt.pf ppf "Left"
    | None -> Fmt.pf ppf "None"
    | Right -> Fmt.pf ppf "Right"
  ;;
end

module Operator_fixity = struct
  type t = Provenance.t * Fixity.t * string

  let info (i, _, _) = i

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let* _, lparens = option' (string "()") in
    let* range, lit = string_lit in
    let* _, rparens = option' (string "()") in
    let info = Provenance.of_range range in
    match lparens, rparens with
    | Some _, None -> return (info, Fixity.Left, lit)
    | None, None -> return (info, Fixity.None, lit)
    | None, Some _ -> return (info, Fixity.Right, lit)
    | Some _, Some _ -> fail "operator can't be both left and right-infix"
  ;;

  let pp ppf (info, fixity, lit) =
    let pp' ppf = function
      | Fixity.Left -> Fmt.pf ppf "()%S" lit
      | None -> Fmt.pf ppf "%S" lit
      | Right -> Fmt.pf ppf "%S()" lit
    in
    Provenance.fmt_stag info pp' ppf fixity
  ;;
end

module Operator_ranking = struct
  type t = Provenance.t * Operator_fixity.t list list

  let info (i, _) = i

  let parse =
    let open Lvca_parsing in
    let char = C_comment_parser.char in
    let level = sep_by1 (char '=') Operator_fixity.parse in
    let+ range, levels = sep_by1 (char '>') level in
    Provenance.of_range range, levels
  ;;

  let pp_level = Fmt.(hovbox (list Operator_fixity.pp ~sep:(any "@ =@ ")))

  let pp ppf (info, levels) =
    Provenance.fmt_stag info Fmt.(hovbox (list pp_level ~sep:(any "@ >@ "))) ppf levels
  ;;

  module Checks = struct
    let same_fixity level =
      Fmt.str
        "All operators in the same level must have the same fixity: found %a"
        pp_level
        level
    ;;
  end

  let check (_, levels) =
    List.find_map levels ~f:(fun level ->
        match level with
        | [] -> Some "Invalid empty level"
        | (_, fixity0, _) :: op_fixities ->
          List.fold_until
            op_fixities
            ~init:fixity0
            ~f:(fun fixity (_, fixity', _) ->
              if Fixity.(fixity = fixity')
              then Continue fixity
              else Stop (Some (Checks.same_fixity level)))
            ~finish:(Fn.const None))
  ;;
end

module Sequence_item = struct
  type t =
    | Var of Provenance.t * string
    | Literal of Provenance.t * string
    | Space of Provenance.t

  let info = function Var (i, _) | Literal (i, _) | Space i -> i
  let vars = function Var (_, v) -> [ v ] | Literal _ | Space _ -> []
  let is_space = function Space _ -> true | _ -> false

  let keywords = function
    | Var _ | Space _ -> String.Set.empty
    | Literal (_, kw) -> String.Set.singleton kw
  ;;

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    choice
      ~failure_msg:"looking for a variable or literal"
      [ (let+ range, _ = char '_' in
         Space (Provenance.of_range range))
      ; (let+ range, str = lower_ident in
         Var (Provenance.of_range range, str))
      ; (let+ range, str = string_lit in
         Literal (Provenance.of_range range, str))
      ]
    <?> "sequence item"
  ;;

  let pp ppf t =
    let pp' ppf = function
      | Var (_, name) -> Fmt.string ppf name
      | Literal (_, str) -> Fmt.pf ppf "%S" str
      | Space _ -> Fmt.pf ppf "_"
    in
    Provenance.fmt_stag (info t) pp' ppf t
  ;;
end

module Operator_concrete_syntax_row = struct
  type t = Provenance.t * Sequence_item.t list

  let info (i, _) = i
  let vars (_, items) = items |> List.map ~f:Sequence_item.vars |> List.concat

  let keywords (_, sequence_items) =
    sequence_items |> List.map ~f:Sequence_item.keywords |> Set.union_list (module String)
  ;;

  let is_binary_operator ~operator_names (_, items) =
    match List.filter items ~f:(not << Sequence_item.is_space) with
    | [ Var (_, v1); Literal (_, l); Var (_, v2) ] when Set.mem operator_names l ->
      Some (v1, l, v2)
    | _ -> None
  ;;

  let is_prefix_row (_, items) =
    match items with Sequence_item.Literal _ :: _ -> true | _ -> false
  ;;

  let parse =
    let open Lvca_parsing in
    let p =
      let+ range, items = many1 Sequence_item.parse in
      Provenance.of_range range, items
    in
    p <?> "operator concrete syntax"
  ;;

  let pp ppf (info, sequence_items) =
    Provenance.fmt_stag info Fmt.(box (list Sequence_item.pp ~sep:sp)) ppf sequence_items
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
        (lower_ident <* char '~')
        (char '/' *> of_angstrom Regex.parse <* char '/')
    in
    p <?> "variable regex"
  ;;

  let pp ppf { info; var_name; re } =
    let pp' ppf () = Fmt.pf ppf "%s ~ /%a/" var_name Regex.pp re in
    Provenance.fmt_stag info pp' ppf ()
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
    Provenance.fmt_stag
      info
      Fmt.(box (list string ~sep:(any ".@ ")))
      ppf
      (List.snoc variable_names body_name)
  ;;

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      let+ pos, idents = sep_end_by1 (char '.') lower_ident in
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
    let pp' ppf () =
      Fmt.(pf ppf "@[%s(%a)@]" name (list Operator_pattern_slot.pp ~sep:semi) slots)
    in
    Provenance.fmt_stag info pp' ppf ()
  ;;

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      lift2
        (fun (name_range, name) (slots_range, slots) ->
          let info = Opt_range.union name_range slots_range |> Provenance.of_range in
          { info; name; slots })
        upper_ident
        (parens (sep_by (char ';') Operator_pattern_slot.parse))
    in
    p <?> "operator pattern"
  ;;
end

module Operator_syntax_row = struct
  type t =
    { info : Provenance.t
    ; pattern : Operator_pattern.t
    ; concrete_syntax : Operator_concrete_syntax_row.t
    }

  let info t = t.info

  let keywords { concrete_syntax; _ } =
    Operator_concrete_syntax_row.keywords concrete_syntax
  ;;

  let is_binary_operator ~operator_names { concrete_syntax; _ } =
    Operator_concrete_syntax_row.is_binary_operator ~operator_names concrete_syntax
  ;;

  let is_prefix_row { concrete_syntax; _ } =
    Operator_concrete_syntax_row.is_prefix_row concrete_syntax
  ;;

  let defers_to_another_sort
      ~sort_name
      ~var_sort_mapping
      { concrete_syntax = _, sequence_items; _ }
    =
    match sequence_items with
    | Var (_, name) :: _ ->
      let sort_name' = Map.find_exn var_sort_mapping name in
      String.(sort_name' <> sort_name)
    | _ -> false
  ;;

  module Checks = struct
    (*
     1. (a, b) No overlapping variable definitions (on rhs or lhs)
     2. Variables used 1-1 (lhs vs rhs)
     3. every line is either a binary operator or prefix row
   *)
    let duplicate_abstract_var sort_name pattern_name dupe =
      Fmt.str
        "Duplicate variable found in abstract pattern for sort `%s` / operator `%s`: %s"
        sort_name
        pattern_name
        dupe
    ;;

    let duplicate_concrete_var sort_name pattern_name dupe =
      Fmt.str
        "Duplicate variable found in concrete pattern for sort `%s` / operator `%s`: %s"
        sort_name
        pattern_name
        dupe
    ;;

    let vars_1_1 lhs_vars rhs_vars =
      Fmt.(
        str
          "LHS and RHS don't bind the same vars (%a vs %a)"
          pp_set
          lhs_vars
          pp_set
          rhs_vars)
    ;;

    let known_form concrete_syntax =
      Fmt.str
        "`%a` is neither a binary operator, prefix row, nor defers to another sort"
        Operator_concrete_syntax_row.pp
        concrete_syntax
    ;;
  end

  module Invariants = struct
    let expected_operator sort_name pattern_name sort_def =
      Fmt.str
        "Expected operator named %s in %a"
        pattern_name
        (Sort_def.pp ~name:sort_name)
        sort_def
    ;;
  end

  let check ~sort_name ~sort_def ~operator_names row =
    let { info = _; pattern; concrete_syntax } = row in
    let lhs_vars = pattern |> Operator_pattern.vars |> string_sort in
    let rhs_vars = concrete_syntax |> Operator_concrete_syntax_row.vars |> string_sort in
    let check_duplicate_vars () =
      match
        ( List.find_consecutive_duplicate ~equal:String.( = ) lhs_vars
        , List.find_consecutive_duplicate ~equal:String.( = ) rhs_vars )
      with
      | None, None -> None
      | Some (dupe, _), _ ->
        Some (Checks.duplicate_abstract_var sort_name pattern.name dupe)
      | _, Some (dupe, _) ->
        Some (Checks.duplicate_concrete_var sort_name pattern.name dupe)
    in
    let check_same_vars () =
      if List.equal String.( = ) lhs_vars rhs_vars
      then None
      else Some (Checks.vars_1_1 lhs_vars rhs_vars)
    in
    let check_known_form () =
      let (Operator_def (_, _, Arity (_, valences))) =
        Sort_def.find_operator_def sort_def pattern.name
        |> Option.get_invariant [%here] (fun () ->
               Invariants.expected_operator sort_name pattern.name sort_def)
      in
      let var_sort_mapping =
        List.zip_exn row.pattern.slots valences
        |> List.bind
             ~f:(fun
                  ( Operator_pattern_slot.{ info = _; variable_names; body_name }
                  , Valence (sort_slots, sort') )
                ->
               let slot_sorts =
                 List.map sort_slots ~f:(function
                     | Sort_binding s -> Sort.name s
                     | Sort_pattern _ -> failwith "TODO: check_known_form Sort_pattern")
               in
               (body_name, Sort.name sort') :: List.zip_exn variable_names slot_sorts)
        |> String.Map.of_alist_exn
      in
      match
        ( is_binary_operator ~operator_names row
        , is_prefix_row row || defers_to_another_sort ~sort_name ~var_sort_mapping row )
      with
      | Some _, false | None, true -> None
      | _ -> Some (Checks.known_form concrete_syntax)
    in
    List.find_map
      [ check_duplicate_vars; check_same_vars; check_known_form ]
      ~f:(fun check -> check ())
  ;;

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
        (Operator_pattern.parse <* char '~')
        Operator_concrete_syntax_row.parse
    in
    p <?> "operator syntax row"
  ;;

  let pp ppf { info; pattern; concrete_syntax } =
    let pp' ppf () =
      Fmt.pf
        ppf
        "%a ~ %a"
        Operator_pattern.pp
        pattern
        Operator_concrete_syntax_row.pp
        concrete_syntax
    in
    Provenance.fmt_stag info pp' ppf ()
  ;;
end

module Operator_syntax = struct
  type t = (Operator_syntax_row.t, Variable_syntax_row.t) Either.t [@@warning "-34"]

  let parse =
    let open Lvca_parsing in
    choice
      ~failure_msg:"looking for an operator or variable syntax row"
      [ Variable_syntax_row.parse >>| Either.Second.return
      ; Operator_syntax_row.parse >>| Either.First.return
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
    ; operator_ranking : Operator_ranking.t option
    }

  type operator_infos = (int * Fixity.t) String.Map.t

  let keywords { operators; _ } =
    operators
    |> List.map ~f:Operator_syntax_row.keywords
    |> Set.union_list (module String)
  ;;

  let operator_infos { operator_ranking; _ } =
    match operator_ranking with
    | None -> String.Map.empty
    | Some (_, fixity_levels) ->
      fixity_levels
      |> List.mapi ~f:(fun i level ->
             List.map level ~f:(fun (_, fixity, name) -> name, (i, fixity)))
      |> List.join
      |> String.Map.of_alist_exn
  ;;

  let operator_names operator_infos = operator_infos |> Map.keys |> String.Set.of_list

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    let p =
      let* name_info, name = lower_ident in
      let* _ = char ':' in
      let* ops_info, operators = many (char '|' *> Operator_syntax.parse) in
      let* _, operator_ranking =
        choice
          ~failure_msg:"Expected a `;` or `\\` operator ranking"
          [ char ';' *> return None; Option.some <$> char '\\' *> Operator_ranking.parse ]
      in
      let range = Opt_range.union name_info ops_info in
      let info = Provenance.of_range range in
      let name = Provenance.of_range name_info, name in
      let operators, vars = List.partition_map operators ~f:Fn.id in
      match vars with
      | [] -> return ~range { info; name; operators; variables = None; operator_ranking }
      | [ row ] ->
        return ~range { info; name; operators; variables = Some row; operator_ranking }
      | _ -> fail "Only one variable row is allowed in a sort"
    in
    p <?> "sort syntax"
  ;;

  let pp_name ppf (info, name) = Provenance.fmt_stag info Fmt.string ppf name
  let pp_row ppf = Fmt.pf ppf "| %a" Operator_syntax.pp

  let pp ppf { info; name; operators; variables; operator_ranking } =
    let open Fmt in
    let operators = List.map operators ~f:Either.First.return in
    let operators =
      match variables with
      | None -> operators
      | Some row -> List.snoc operators (Either.Second row)
    in
    let pp' ppf () =
      match operator_ranking with
      | None -> pf ppf "@[<v 2>@[<h>%a:@]@;%a@;;@]" pp_name name (list pp_row) operators
      | Some ranking ->
        pf
          ppf
          "@[<v 2>@[<h>%a:@]@;%a@;\\ %a@]"
          pp_name
          name
          (list pp_row)
          operators
          Operator_ranking.pp
          ranking
    in
    Provenance.fmt_stag info pp' ppf ()
  ;;

  module Checks = struct
    (*
     1. (a, b). Variables defined on both sides or neither
     2. 1-1 mapping between concrete and abstract operators
    *)
    let mismatched_vars_1 =
      "Abstract syntax defines variables but concrete syntax doesn't"
    ;;

    let mismatched_vars_2 =
      "Concrete syntax defines variables but abstract syntax doesn't"
    ;;

    let same_operators sort_name concrete_op_names abstract_op_names =
      Fmt.str
        "Concrete syntax definition for sort `%s` doesn't have the same operators (%a) \
         as the abstract syntax (%a)"
        sort_name
        pp_set
        concrete_op_names
        pp_set
        abstract_op_names
    ;;
  end

  let check ~sort_defs t =
    let { info = _
        ; operators = concrete_ops
        ; name = _, sort_name
        ; variables
        ; operator_ranking
        }
      =
      t
    in
    let sort_def = Map.find_exn sort_defs sort_name in
    let (Sort_def.Sort_def (_vars, abstract_ops, var_names)) = sort_def in
    let operator_names = t |> operator_infos |> operator_names in
    let check_1_1_vars () =
      match variables, var_names with
      | None, _ :: _ -> Some Checks.mismatched_vars_1
      | Some _, [] -> Some Checks.mismatched_vars_2
      | _ -> None
    in
    let check_same_ops_and_rows () =
      let abstract_op_names =
        abstract_ops
        |> List.map ~f:(fun (Operator_def.Operator_def (_, name, _)) -> name)
        |> string_sort
      in
      let concrete_op_names =
        concrete_ops
        |> List.map ~f:(function Operator_syntax_row.{ pattern = { name; _ }; _ } -> name)
        |> string_sort
      in
      if not (List.equal String.( = ) concrete_op_names abstract_op_names)
      then Some (Checks.same_operators sort_name concrete_op_names abstract_op_names)
      else
        List.find_map
          concrete_ops
          ~f:(Operator_syntax_row.check ~sort_name ~sort_def ~operator_names)
    in
    let check_operator_ranking () =
      Option.bind operator_ranking ~f:Operator_ranking.check
    in
    List.find_map
      [ check_1_1_vars; check_same_ops_and_rows; check_operator_ranking ]
      ~f:(fun check -> check ())
  ;;
end

type t = Sort_syntax.t list

let keywords t = t |> List.map ~f:Sort_syntax.keywords |> Set.union_list (module String)

let unordered_keywords t =
  t |> Map.data |> List.map ~f:Sort_syntax.keywords |> Set.union_list (module String)
;;

let build_unordered ordered =
  ordered
  |> List.map ~f:(fun (Sort_syntax.{ name = _, name; _ } as sort_syntax) ->
         name, sort_syntax)
  |> String.Map.of_alist
;;

let parse = Lvca_parsing.(many1 Sort_syntax.parse <?> "parse / pretty definition")
let pp = Fmt.(list Sort_syntax.pp ~sep:(any "@.@."))

let find_var_sort pattern_slots valences needle =
  List.zip_exn pattern_slots valences
  |> List.find_map_exn
       ~f:(fun
            ( Operator_pattern_slot.{ variable_names; body_name; _ }
            , Valence.Valence (sort_slots, body_sort) )
          ->
         if String.(body_name = needle)
         then Some (Sort_slot.Sort_binding body_sort)
         else
           List.zip_exn variable_names sort_slots
           |> List.find_map ~f:(fun (name, slot) ->
                  if String.(name = needle) then Some slot else None))
;;

let leading_sort_graph abstract_syntax concrete_syntax =
  Map.mapi abstract_syntax ~f:(fun ~key:sort_name ~data:(abstract_sort : Sort_def.t) ->
      let Sort_syntax.{ operators = rows; _ } = Map.find_exn concrete_syntax sort_name in
      let (Sort_def (_, op_defs, _)) = abstract_sort in
      List.filter_map
        rows
        ~f:(fun
             Operator_syntax_row.
               { info = _; pattern; concrete_syntax = _, sequence_items }
           ->
          let (Operator_def (_, _, Arity (_, valences))) =
            List.find_exn op_defs ~f:(fun (Operator_def (_, name, _)) ->
                String.(name = pattern.name))
          in
          match sequence_items with
          | Var (_, name) :: _ ->
            let sort_slot = find_var_sort pattern.slots valences name in
            let sort =
              match sort_slot with
              | Sort_binding sort -> sort
              | Sort_pattern { var_sort; _ } -> var_sort
            in
            Some (Sort.name sort)
          | _ -> None))
;;

module Checks = struct
  (*
   1. Concrete and abstract sorts are 1-1
   2. No left-recursive cycles between sorts
   3. No Duplicate sort definitions
  *)
  let same_sorts concrete_sort_names abstract_sort_names =
    Fmt.str
      "Concrete syntax definition doesn't have the same sorts (%a) as abstract syntax \
       (%a)"
      pp_set
      concrete_sort_names
      pp_set
      abstract_sort_names
  ;;

  let no_left_recursive_cycles = "There is unavoidable left-recursion in this grammar"

  let no_duplicate_sort_defs sort_name =
    Fmt.str "duplicate sort definition for %s" sort_name
  ;;
end

let check_sort_graph abstract_syntax concrete_syntax () =
  let graph = leading_sort_graph abstract_syntax concrete_syntax in
  match Directed_graph.topsort graph with
  | None -> Some Checks.no_left_recursive_cycles
  | Some _ -> None
;;

let check_sort_defs sort_defs ordered =
  List.find_map ordered ~f:(Sort_syntax.check ~sort_defs)
;;

(* Check that the sorts covered in the concrete / abstract syntax are 1-1 *)
let check_same_sorts sort_defs ordered () =
  let abstract_sort_names = sort_defs |> Map.keys |> string_sort in
  let concrete_sort_names =
    ordered |> List.map ~f:(fun Sort_syntax.{ name = _, name; _ } -> name) |> string_sort
  in
  if List.equal String.( = ) abstract_sort_names concrete_sort_names
  then check_sort_defs sort_defs ordered
  else Some (Checks.same_sorts concrete_sort_names abstract_sort_names)
;;

(* Run all checks. *)
let check sort_defs ordered =
  match build_unordered ordered with
  | `Duplicate_key sort_name -> Some (Checks.no_duplicate_sort_defs sort_name)
  | `Ok unordered ->
    [ check_same_sorts sort_defs ordered; check_sort_graph sort_defs unordered ]
    |> List.find_map ~f:(fun checker -> checker ())
;;

module Unordered = struct
  type t = Sort_syntax.t String.Map.t

  let build = build_unordered
  let keywords = unordered_keywords
end

let%test_module "parsing / pretty-printing" =
  (module struct
    let () = Stdlib.Format.set_tags false

    let%test_module "Operator_fixity" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_fixity.parse
        let parse_print = parse >> Operator_fixity.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|()"+"|};
          [%expect {|()"+"|}]
        ;;

        let%expect_test _ =
          parse_print {|"+"|};
          [%expect {|"+"|}]
        ;;

        let%expect_test _ =
          parse_print {|"+"()|};
          [%expect {|"+"()|}]
        ;;
      end)
    ;;

    let%test_module "Operator_ranking" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_ranking.parse
        let parse_print = parse >> Operator_ranking.pp Fmt.stdout

        let%expect_test _ =
          parse_print {|()"*" = ()"/" > ()"+" = ()"-"|};
          [%expect {|()"*" = ()"/" > ()"+" = ()"-"|}]
        ;;
      end)
    ;;

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

    let%test_module "Operator_concrete_syntax_row" =
      (module struct
        let parse =
          Lvca_parsing.parse_string_or_failwith Operator_concrete_syntax_row.parse
        ;;

        let parse_print = parse >> Operator_concrete_syntax_row.pp Fmt.stdout

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
  \ ()"*" > ()"+"
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
              \ ()"*" > ()"+"
              |}]
        ;;
      end)
    ;;

    let mk_concrete = Lvca_parsing.parse_string_or_failwith parse
    let parse_print str = pp Fmt.stdout (mk_concrete str)

    let lang_abstract_defn =
      {|expr :=
  | Add(expr; expr)
  | Mul(expr; expr)
  | Fun(val. expr)
  | Val(val)
  ;

val := True() | False();
      |}
    ;;

    let lang_concrete_defn =
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

    let%expect_test _ =
      parse_print lang_concrete_defn;
      [%expect
        {|
        expr:
          | Add(x; y) ~ x _ "+" _ y
          | Mul(x; y) ~ x _ "*" _ y
          | Fun(v. e) ~ "fun" _ v _ "->" _ e
          | Val(v) ~ v
          | x ~ /[a-z][a-zA-Z0-9_]*/
          \ ()"*" > ()"+"

        val:
          | True() ~ "true"
          | False() ~ "false"
          ;
          |}]
    ;;

    let mk_abstract str =
      match
        Lvca_parsing.parse_string_or_failwith Abstract_syntax.parse str
        |> Abstract_syntax.mk_unordered
      with
      | `Duplicate_key k -> failwith (Fmt.str "duplicate key %s" k)
      | `Ok Abstract_syntax.Unordered.{ sort_defs; _ } -> sort_defs
    ;;

    let%test_module "check" =
      (module struct
        let go abstract concrete =
          match check abstract concrete with
          | None -> Fmt.pr "okay"
          | Some msg -> Fmt.pr "%s" msg
        ;;

        let%expect_test _ =
          let abstract = mk_abstract lang_abstract_defn in
          let concrete = mk_concrete lang_concrete_defn in
          go abstract concrete;
          [%expect {| Concrete syntax defines variables but abstract syntax doesn't |}]
        ;;

        let%expect_test _ =
          let abstract = mk_abstract {|a := A(b);
b := B(a);|} in
          let concrete =
            mk_concrete
              {|a:
  | A(b) ~ b "b"
  \ "foo" > "bar"

b:
  | B(a) ~ a "a"
  ; |}
          in
          go abstract concrete;
          [%expect {| There is unavoidable left-recursion in this grammar |}]
        ;;

        let abstract = mk_abstract "foo := Foo();"

        let%expect_test _ =
          let concrete =
            mk_concrete
              {|foo:
  | Foo() ~ "foo"
  \ "foo" > "bar"

foo:
  | Foo() ~ "bar"
  ; |}
          in
          go abstract concrete;
          [%expect {| duplicate sort definition for foo |}]
        ;;

        let%expect_test _ =
          let concrete =
            mk_concrete {|foo:
  | Foo() ~ "foo"
  | Bar() ~ "bar"
  \ "foo" > "bar"|}
          in
          go abstract concrete;
          [%expect
            {|
            Concrete syntax definition for sort `foo` doesn't have the same operators (
            {Bar, Foo}) as the abstract syntax ({Foo}) |}]
        ;;

        let abstract = mk_abstract "a := A(a. a);"

        let%expect_test _ =
          let concrete = mk_concrete {|a:
  | A(x. y) ~ "x" x "y" z
  ; |} in
          go abstract concrete;
          [%expect {| LHS and RHS don't bind the same vars ({x, y} vs {x, z}) |}]
        ;;

        let%expect_test _ =
          let concrete = mk_concrete {|a:
  | A(x. x) ~ "x" x "y" z
  ; |} in
          go abstract concrete;
          [%expect
            {| Duplicate variable found in abstract pattern for sort `a` / operator `A`: x |}]
        ;;

        let%expect_test _ =
          let concrete = mk_concrete {|a:
  | A(x. y) ~ "x" x "y" x
  ; |} in
          go abstract concrete;
          [%expect
            {| Duplicate variable found in concrete pattern for sort `a` / operator `A`: x |}]
        ;;

        let%expect_test _ =
          let concrete = mk_concrete {|a:
  | A(x. y) ~ x "a" y
  ; |} in
          go abstract concrete;
          [%expect
            {|`x "a" y` is neither a binary operator, prefix row, nor defers to another sort|}]
        ;;
      end)
    ;;
  end)
;;

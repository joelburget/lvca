(* Decisions:
 * - always allow parens
 * - common whitespace definition
 *   - how to define required / optional?
 * - how to define boxes?
 *
 * TODO:
 * [x] actually parse / pretty-print with a term
 * [x] check validity
 * [ ] multiple concrete syntaxes mapping to the same abstract
 * [x] operator ranking
 * [ ] whitespace
 * [x] variables colliding with keywords
 * [ ] do all operators in the same level have to have the same fixity?
 *)
open Base
open Lvca_provenance
open Lvca_util

let pp_set = Fmt.(braces (list string ~sep:comma))
let reserved_words = Lvca_util.String.Set.empty
let lower_ident = Lvca_parsing.C_comment_parser.lower_identifier reserved_words
let upper_ident = Lvca_parsing.C_comment_parser.upper_identifier reserved_words

module Fixity = struct
  type t =
    | Left
    | None
    | Right

  (*
  let pp ppf = function
    | Left -> Fmt.pf ppf "Left"
    | None -> Fmt.pf ppf "None"
    | Right -> Fmt.pf ppf "Right"
  ;;
     *)
end

module Operator_fixity = struct
  type t = Provenance.t * Fixity.t * string

  let info (i, _, _) = i

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    option' (string "()")
    >>= fun lparens ->
    attach_pos' string_lit
    >>= fun (range, lit) ->
    option' (string "()")
    >>= fun rparens ->
    let info = Provenance.of_range range in
    match lparens, rparens with
    | Some _, None -> return (info, Fixity.Left, lit)
    | None, None -> return (info, Fixity.None, lit)
    | None, Some _ -> return (info, Fixity.Right, lit)
    | Some _, Some _ -> fail "operator can't be both left and right-infix"
  ;;

  let pp ppf (info, fixity, lit) =
    Provenance.open_stag ppf info;
    (match fixity with
    | Fixity.Left -> Fmt.pf ppf "()%S" lit
    | None -> Fmt.pf ppf "%S" lit
    | Right -> Fmt.pf ppf "%S()" lit);
    Provenance.close_stag ppf info
  ;;
end

module Operator_ranking = struct
  type t = Provenance.t * Operator_fixity.t list list

  let info (i, _) = i

  let parse =
    let open Lvca_parsing in
    let char = C_comment_parser.char in
    let level = sep_by1 (char '=') Operator_fixity.parse in
    sep_by1 (char '>') level >>~ fun range levels -> Provenance.of_range range, levels
  ;;

  let pp_level = Fmt.(hovbox (list Operator_fixity.pp ~sep:(any "@ =@ ")))

  let pp ppf (info, levels) =
    Provenance.open_stag ppf info;
    Fmt.(hovbox (list pp_level ~sep:(any "@ >@ "))) ppf levels;
    Provenance.close_stag ppf info
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

module Operator_concrete_syntax_row = struct
  type t = Provenance.t * Sequence_item.t list

  let info (i, _) = i
  let vars (_, items) = items |> List.map ~f:Sequence_item.vars |> List.concat

  let keywords (_, sequence_items) =
    sequence_items |> List.map ~f:Sequence_item.keywords |> Set.union_list (module String)
  ;;

  let is_binary_operator operator_names (_, items) =
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

  let build_exn env { info = _; variable_names; body_name } =
    Nominal.Scope.Scope
      ( List.map variable_names ~f:(fun name ->
            match Map.find_exn env name |> Nominal.Term.to_pattern with
            | Ok pat -> pat
            | Error _scope -> failwith "TODO: Operator_pattern_slot.build_exn Error")
      , Map.find_exn env body_name )
  ;;

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

  let build_exn env { info; name; slots } =
    Nominal.Term.Operator
      ( Provenance.calculated_here [%here] [ info ]
      , name
      , List.map slots ~f:(Operator_pattern_slot.build_exn env) )
  ;;

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
    ; concrete_syntax : Operator_concrete_syntax_row.t
    }

  let info t = t.info
  let name { pattern = { name; _ }; _ } = name

  let keywords { concrete_syntax; _ } =
    Operator_concrete_syntax_row.keywords concrete_syntax
  ;;

  let is_binary_operator operator_names { concrete_syntax; _ } =
    Operator_concrete_syntax_row.is_binary_operator operator_names concrete_syntax
  ;;

  let is_prefix_row { concrete_syntax; _ } =
    Operator_concrete_syntax_row.is_prefix_row concrete_syntax
  ;;

  let defers_to_another_sort
      var_sort_mapping
      sort_name
      { concrete_syntax = _, sequence_items; _ }
    =
    match sequence_items with
    | Var (_, name) :: _ ->
      let sort_name' = Map.find_exn var_sort_mapping name in
      String.(sort_name' <> sort_name)
    | _ -> false
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
        (attach_pos' Operator_pattern.parse <* char '~')
        (attach_pos' Operator_concrete_syntax_row.parse)
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
      Operator_concrete_syntax_row.pp
      concrete_syntax;
    Provenance.close_stag ppf info
  ;;
end

module Operator_syntax = struct
  type t = (Operator_syntax_row.t, Variable_syntax_row.t) Either.t [@@warning "-34"]

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
    ; operator_ranking : Operator_ranking.t option
    }

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

  type partition =
    { binary_operators : (Operator_pattern.t * string * string * string) list
    ; prefix_rows : Operator_syntax_row.t list
    }

  let partition_operator_rows sort_name operator_rows sort_def operator_names =
    let binary_operators, prefix_rows =
      List.partition_map operator_rows ~f:(fun row ->
          let (Operator_def (_, _, Arity (_, valences))) =
            Sort_def.find_operator_def sort_def (Operator_syntax_row.name row)
            |> Option.get_invariant [%here] (fun () -> "TODO: partition_operator_rows")
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
                         | Sort_pattern _ ->
                           failwith "TODO: prefix_operator_syntax_row Sort_pattern")
                   in
                   (body_name, Sort.name sort') :: List.zip_exn variable_names slot_sorts)
            |> String.Map.of_alist_exn
          in
          match
            (* TODO: Just replace with a check for operator in operator_ranking? *)
            ( Operator_syntax_row.is_binary_operator operator_names row
            , Operator_syntax_row.is_prefix_row row
              || Operator_syntax_row.defers_to_another_sort var_sort_mapping sort_name row
            )
          with
          | Some (v1, l, v2), _ ->
            Either.First (row.pattern, v1, l, v2)
            (* TODO: Just use everything which is not a binary operator? *)
          | _, true -> Either.Second row
          | _, _ ->
            Lvca_util.invariant_violation
              [%here]
              (Fmt.str
                 "we don't know how to parse rows that are neither binary operators nor \
                  prefixed by a literal (%a)"
                 Operator_syntax_row.pp
                 row))
    in
    { binary_operators; prefix_rows }
  ;;

  let level_operator_rows
      (operator_ranking : Operator_fixity.t list list)
      (binary_operators : (Operator_pattern.t * string * string * string) list)
      : (Fixity.t * Operator_pattern.t * string * string * string) list list
    =
    List.map operator_ranking ~f:(fun operator_fixities ->
        let operator_fixities =
          operator_fixities
          |> List.map ~f:(fun (_, fixity, name) -> name, fixity)
          |> String.Map.of_alist_exn
        in
        List.filter_map binary_operators ~f:(fun (pattern, v1, lit, v2) ->
            (*
            Fmt.(
              pr
                "looking for operator %s -> %a (%a)\n"
                lit
                (option ~none:(any "None") Fixity.pp)
                (Map.find operator_fixities pattern.Operator_pattern.name)
                pp_set
                (Map.keys operator_fixities));
               *)
            Map.find operator_fixities lit
            |> Option.map ~f:(fun fixity -> fixity, pattern, v1, lit, v2)))
  ;;

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
      choice
        ~failure_msg:"Expected a `;` or `\\` operator ranking"
        [ char ';' *> return None; Option.some <$> char '\\' *> Operator_ranking.parse ]
      >>= fun operator_ranking ->
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

  let pp_name ppf (info, name) =
    Provenance.open_stag ppf info;
    Fmt.string ppf name;
    Provenance.close_stag ppf info
  ;;

  let pp_row ppf = Fmt.pf ppf "| %a" Operator_syntax.pp

  let pp ppf { info; name; operators; variables; operator_ranking } =
    let open Fmt in
    let operators = List.map operators ~f:(fun row -> Either.First row) in
    let operators =
      match variables with
      | None -> operators
      | Some row -> List.snoc operators (Either.Second row)
    in
    Provenance.open_stag ppf info;
    (match operator_ranking with
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
        ranking);
    Provenance.close_stag ppf info
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

(* Check
 * [x] No overlapping variable definitions (on rhs or lhs)
 * [x] Variables used 1-1 (lhs vs rhs)
 * [x] 1-1 mapping between sorts given an abstract / concrete syntax
 * [x] every operator in a sort given a concrete syntax
 * [x] variable given concrete syntax iff allowed
 * [ ] space hygiene:
 *   - no repeated spaces
 *   - no leading / trailing spaces
 * [x] no cycles between sorts
 * [ ] every operator has a line (?)
 * [ ] will var parser be clobbered by left-recursion deferring to a var parser
   from another sort? Is this a grammar problem?
 *)
module Directed_graph = Directed_graph.Make (String)

let find_var_in_pattern
    (pattern_slots : Operator_pattern_slot.t list)
    (valences : Valence.t list)
    (needle : string)
    : Sort.t
  =
  let pairs : (Operator_pattern_slot.t * Valence.t) list =
    List.zip_exn pattern_slots valences
  in
  List.find_map_exn
    pairs
    ~f:(fun
         ( Operator_pattern_slot.{ variable_names; body_name; _ }
         , Valence (sort_slots, body_sort) )
       ->
      if String.(body_name = needle)
      then Some body_sort
      else (
        let binder_pairs : (string * Sort_slot.t) list =
          List.zip_exn variable_names sort_slots
        in
        List.find_map binder_pairs ~f:(fun (name, slot) ->
            if String.(name = needle)
            then (
              match slot with
              | Sort_binding sort -> Some sort
              | Sort_pattern _ -> failwith "TODO: Sort_pattern")
            else None)))
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
            let sort = find_var_in_pattern pattern.slots valences name in
            let name, _ = Sort.split sort in
            Some name
          | _ -> None))
;;

let check_sort_graph abstract_syntax concrete_syntax () =
  let graph = leading_sort_graph abstract_syntax concrete_syntax in
  match Directed_graph.topsort graph with
  | None -> Some "There is unavoidable left-recursion in this grammar"
  | Some _ -> None
;;

let check sort_defs ordered =
  let sort = List.sort ~compare:String.compare in
  match build_unordered ordered with
  | `Duplicate_key k -> Some (Fmt.str "duplicate sort definition for %s" k)
  | `Ok unordered ->
    let operator_syntax_row
        sort_name
        Operator_syntax_row.{ info = _; pattern; concrete_syntax }
      =
      let lhs_vars = pattern |> Operator_pattern.vars |> sort in
      let rhs_vars = concrete_syntax |> Operator_concrete_syntax_row.vars |> sort in
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
             "Duplicate variable found in abstract pattern for sort `%s` / operator \
              `%s`: %s"
             sort_name
             pattern.name
             dupe)
      | _, Some (dupe, _) ->
        Some
          (Fmt.str
             "Duplicate variable found in concrete pattern for sort `%s` / operator \
              `%s`: %s"
             sort_name
             pattern.name
             dupe)
    in
    let sort_syntax
        Sort_syntax.
          { info = _
          ; operators = concrete_ops
          ; name = _, sort_name
          ; variables
          ; operator_ranking = _
          }
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
            (Fmt.str
               "Concrete syntax definition for sort `%s` doesn't have the same operators \
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
    (* Check that the sorts covered in the concrete / abstract syntax are 1-1 *)
    let check_same_sorts () =
      if List.equal String.( = ) abstract_sort_names concrete_sort_names
      then List.find_map ordered ~f:sort_syntax
      else
        Some
          (Fmt.str
             "Concrete syntax definition doesn't have the same sorts (%a) as abstract \
              syntax (%a)"
             pp_set
             concrete_sort_names
             pp_set
             abstract_sort_names)
    in
    [ check_same_sorts; check_sort_graph sort_defs unordered ]
    |> List.find_map ~f:(fun checker -> checker ())
;;

module Pp_term = struct
  (* Precedence levels
     - Starting from 0 (highest precedence), counting up to Base.Int.max_value
       (lowest precedence)
     - We compare two precedence levels -- the environment's level and the
       current expression's level. If the environment has higher (or equal)
       precedence (a lower number), we must include parens.
   *)
  let pp_term
      (concrete_syntax : Sort_syntax.t String.Map.t)
      (start_sort : string)
      ppf
      (tm : Nominal.Term.t)
    =
    let operator_infos = Map.map concrete_syntax ~f:Sort_syntax.operator_infos in
    let rec operator_row (sort_name : string) ~env_prec row tm =
      match tm with
      | Nominal.Term.Var _ ->
        Lvca_util.invariant_violation [%here] "operator_row doesn't handle vars"
      | Primitive prim -> Ok (Some (Primitive.All.pp ppf prim))
      | Operator (_, op_name, scopes) ->
        if String.(row.Operator_syntax_row.pattern.name <> op_name)
        then Ok None
        else pp_operator sort_name row scopes ~env_prec
    and pp_operator sort_name row scopes ~env_prec =
      let Operator_syntax_row.
            { info = _; pattern = { slots; _ }; concrete_syntax = _, sequence_items }
        =
        row
      in
      let operator_infos = Map.find_exn operator_infos sort_name in
      let operator_names = Sort_syntax.operator_names operator_infos in
      match List.zip slots scopes with
      | Ok slotscopes ->
        let open Result.Let_syntax in
        let%bind var_mappings =
          slotscopes
          |> List.map
               ~f:(fun
                    ( Operator_pattern_slot.{ info = _; variable_names; body_name }
                    , Nominal.Scope.Scope (pats, body) )
                  ->
                 match
                   List.zip variable_names (List.map pats ~f:(fun p -> Either.First p))
                 with
                 | Unequal_lengths -> Error "TODO: pp_term 1"
                 | Ok varpats ->
                   Ok
                     (varpats
                     |> String.Map.of_alist_exn
                     |> Map.set ~key:body_name ~data:(Either.Second body)))
          |> Result.all
        in
        let var_mapping = String.Map.unions_left_biased var_mappings in
        let pp_sequence_items ppf get_env_prec =
          List.iter sequence_items ~f:(function
              | Sequence_item.Var (_, name) ->
                (match Map.find_exn var_mapping name with
                | First pat ->
                  pat |> Nominal.Term.of_pattern |> go sort_name (get_env_prec ())
                | Second tm -> go sort_name (get_env_prec ()) tm)
              | Literal (_, str) -> Fmt.string ppf str
              | Space _ -> Fmt.sp ppf ())
        in
        let print_it ppf () =
          match Operator_syntax_row.is_binary_operator operator_names row with
          | Some (_, op_name, _) ->
            let op_prec, fixity = Map.find_exn operator_infos op_name in
            let initial_prec_level, final_prec_level =
              (* New environment prec levels for left / right sides. Higher
                 number means lower precedence means won't show parens for same
                 level. Same number will show parens for the same level. *)
              match fixity with
              | Left -> op_prec + 1, op_prec
              | None -> op_prec, op_prec
              | Right -> op_prec, op_prec + 1
            in
            let prec_level_ref = ref initial_prec_level in
            let get_env_prec () =
              let result = !prec_level_ref in
              prec_level_ref := final_prec_level;
              result
            in
            (* Include parens when we have lower precedence than the
               environment (higher number) *)
            if op_prec >= env_prec
            then Fmt.parens pp_sequence_items ppf get_env_prec
            else pp_sequence_items ppf get_env_prec
          | None -> pp_sequence_items ppf (fun () -> Base.Int.max_value)
        in
        Ok (Some Fmt.((hovbox ~indent:2 print_it) ppf ()))
      | Unequal_lengths -> Error "TODO: pp_term 2"
    and go sort_name env_prec tm =
      match tm with
      | Nominal.Term.Var (_, var_name) -> Fmt.string ppf var_name
      | _ ->
        let operator_syntaxes : Operator_syntax_row.t list =
          match Map.find concrete_syntax start_sort (* XXX need to change sort *) with
          | Some Sort_syntax.{ operators; _ } -> operators
          | None ->
            Lvca_util.invariant_violation
              [%here]
              (Fmt.str "didn't find expected operator %S" start_sort)
        in
        (match
           List.find_map operator_syntaxes ~f:(fun row ->
               match operator_row sort_name ~env_prec row tm with
               | Error _ -> None
               | Ok v -> v)
         with
        | None ->
          Lvca_util.invariant_violation
            [%here]
            (Fmt.str "Didn't find matching operator syntax for `%a`" Nominal.Term.pp tm)
        | Some () -> ())
    in
    go start_sort Base.Int.max_value tm
  ;;
end

module Parse_term = struct
  open Lvca_parsing

  type dispatch =
    { map :
        (Provenance.Parse_input.t -> dispatch -> Nominal.Term.t Lvca_parsing.t)
        String.Map.t
    }

  let build_slot env Operator_pattern_slot.{ info = _; variable_names; body_name } =
    let pats =
      List.map variable_names ~f:(fun name ->
          match Map.find_exn env name with
          | Nominal.Term.Var (info, name) -> Pattern.Var (info, name)
          | _ -> failwith "TODO build_slot non-var")
    in
    Nominal.Scope.Scope (pats, Map.find_exn env body_name)
  ;;

  let build_operator range env Operator_pattern.{ info = _; name; slots } input =
    Nominal.Term.Operator
      (Provenance.of_range ~input range, name, List.map slots ~f:(build_slot env))
  ;;

  let sequence_items var_parsers pattern items input =
    let open C_comment_parser in
    let rec go env range = function
      | [] -> return (build_operator range env pattern input)
      | item :: items ->
        (match item with
        | Sequence_item.Space _ -> go env range items
        | Literal (_, str) ->
          attach_pos' (string str)
          >>= fun (rng', _) -> go env (Opt_range.union range rng') items
        | Var (_, name) ->
          attach_pos' (Map.find_exn var_parsers name)
          >>= fun (rng', data) ->
          let env = Map.set env ~key:name ~data in
          go env (Opt_range.union range rng') items)
    in
    go String.Map.empty None items
  ;;

  let sort sort_name self dispatch input sort =
    let sort_name' = Sort.name sort in
    if String.(sort_name' = sort_name)
    then self
    else Map.find_exn dispatch.map sort_name' input dispatch
  ;;

  let sort_mapping slots valences =
    List.zip_exn slots valences
    |> List.bind
         ~f:(fun
              ( Operator_pattern_slot.{ info = _; variable_names; body_name }
              , Valence.Valence (sort_slots, sort') )
            ->
           let slot_sorts =
             List.map sort_slots ~f:(function
                 | Sort_binding s -> s
                 | Sort_pattern _ ->
                   failwith "TODO: prefix_operator_syntax_row Sort_pattern")
           in
           (body_name, sort') :: List.zip_exn variable_names slot_sorts)
    |> String.Map.of_alist_exn
  ;;

  let prefix_operator_syntax_row
      sort
      (Operator_def.Operator_def (_, name, Arity (_, valences)))
      Operator_syntax_row.{ info = _; pattern; concrete_syntax = _, items }
      input
    =
    let f = sort input in
    let var_parsers = sort_mapping pattern.slots valences |> Map.map ~f in
    sequence_items var_parsers pattern items input <?> name
  ;;

  let var_parser keywords re =
    Lvca_parsing.(attach_pos' (of_angstrom (Regex.to_angstrom re)) <* whitespace)
    >>= fun (pos, name) ->
    if Set.mem keywords name
    then fail (Fmt.str "%S is a keyword" name)
    else return (Nominal.Term.Var (Provenance.of_range pos, name))
  ;;

  let sort_syntax keywords (sort_name, sort_def, sort_syntax) self dispatch input =
    let Sort_syntax.{ operators; operator_ranking; variables; _ } = sort_syntax in
    let failure_msg =
      let operator_names = List.map operators ~f:Operator_syntax_row.name in
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
    let operator_names =
      sort_syntax |> Sort_syntax.operator_infos |> Sort_syntax.operator_names
    in
    let Sort_syntax.{ binary_operators; prefix_rows } =
      Sort_syntax.partition_operator_rows sort_name operators sort_def operator_names
    in
    let operator_ranking =
      match operator_ranking with Some (_, ranking) -> ranking | None -> []
    in
    let sort = sort sort_name self dispatch in
    let find_op_def_exn op_name =
      Sort_def.find_operator_def sort_def op_name
      |> Option.get_invariant [%here] (fun () -> "TODO: find_op_def_exn")
    in
    let mk_op_parser op =
      let op_def = find_op_def_exn (Operator_syntax_row.name op) in
      prefix_operator_syntax_row sort op_def op input
    in
    let prefix_rows = List.map prefix_rows ~f:mk_op_parser in
    let prefix_parser =
      match variables, prefix_rows with
      | None, [] -> None
      | None, _ -> Some (choice ~failure_msg prefix_rows)
      | Some Variable_syntax_row.{ re; _ }, _ ->
        Some (choice ~failure_msg (var_parser keywords re :: prefix_rows))
    in
    let mk_level_parser higher_prec rows =
      let op_name =
        rows
        |> List.map ~f:(fun (_fixity, pat, v1_name, op_name, v2_name) ->
               C_comment_parser.string op_name >>| fun _ -> pat, v1_name, v2_name)
        |> choice ~failure_msg:"operator name"
      in
      let fixity, _, _, _, _ (* XXX what if different fixities *) = List.hd_exn rows in
      let op_rhs_pair =
        op_name >>= fun op_info -> higher_prec >>| fun tm -> op_info, tm
      in
      higher_prec
      >>= fun init ->
      many op_rhs_pair
      >>= fun pairs ->
      let fold_pairs =
        List.fold ~init ~f:(fun t1 ((pattern, v1_name, v2_name), t2) ->
            let env = String.Map.of_alist_exn [ v1_name, t1; v2_name, t2 ] in
            Operator_pattern.build_exn env pattern)
      in
      match fixity with
      | Fixity.Left -> pairs |> fold_pairs |> return
      | None ->
        (match pairs with
        | [ ((pattern, v1_name, v2_name), t2) ] ->
          let env = String.Map.of_alist_exn [ v1_name, init; v2_name, t2 ] in
          return (Operator_pattern.build_exn env pattern)
        | _ -> fail "Don't know how to parse operators with no fixity")
      | Right -> pairs |> List.rev |> fold_pairs |> return
    in
    let highest_prec =
      match prefix_parser with
      | Some p ->
        choice
          ~failure_msg:(Fmt.str "parenthesized %s, or with a literal prefix" sort_name)
          [ C_comment_parser.parens self; p ]
      | None -> C_comment_parser.parens self
    in
    let operator_levels =
      Sort_syntax.level_operator_rows operator_ranking binary_operators
    in
    List.fold operator_levels ~init:highest_prec ~f:mk_level_parser <?> sort_name
  ;;

  let lang abstract_syntax concrete_syntax =
    let keywords = unordered_keywords concrete_syntax in
    let map =
      Map.mapi
        abstract_syntax
        ~f:(fun ~key:sort_name ~data:abstract_sort input dispatch ->
          fix
          @@ fun self ->
          let concrete_sort = Map.find_exn concrete_syntax sort_name in
          sort_syntax
            keywords
            (sort_name, abstract_sort, concrete_sort)
            self
            dispatch
            input)
    in
    { map }
  ;;
end

module Unordered = struct
  type t = Sort_syntax.t String.Map.t

  let build = build_unordered
  let keywords = unordered_keywords
  let pp_term ~sort lang = Pp_term.pp_term lang sort

  let parse_term ?(input = Provenance.Parse_input.Input_unknown) ~sort abstract concrete =
    let parser = Parse_term.lang abstract concrete in
    Map.find_exn parser.map sort input parser
  ;;
end

module Make (Input : sig
  val abstract : Sort_def.t String.Map.t
  val concrete : Unordered.t
  val start_sort : string
end) =
struct
  let pp_term = Unordered.pp_term ~sort:Input.start_sort Input.concrete

  let parse_term input =
    Unordered.parse_term ~input ~sort:Input.start_sort Input.abstract Input.concrete
  ;;
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

    let lang_concrete =
      match lang_concrete_defn |> parse |> Unordered.build with
      | `Duplicate_key k -> failwith (Fmt.str "Unexpected duplicate key %s" k)
      | `Ok lang -> lang
    ;;

    let%test_module "check" =
      (module struct
        let mk_abstract str =
          match
            Lvca_parsing.parse_string_or_failwith Abstract_syntax.parse str
            |> Abstract_syntax.mk_unordered
          with
          | `Duplicate_key k -> failwith (Fmt.str "duplicate key %s" k)
          | `Ok Abstract_syntax.Unordered.{ sort_defs; _ } -> sort_defs
        ;;

        let go abstract concrete =
          match check abstract concrete with
          | None -> Fmt.pr "okay"
          | Some msg -> Fmt.pr "%s" msg
        ;;

        let%expect_test _ =
          let abstract = mk_abstract lang_abstract_defn in
          let concrete = parse lang_concrete_defn in
          go abstract concrete;
          [%expect {| Concrete syntax defines variables but abstract syntax doesn't |}]
        ;;

        let%expect_test _ =
          let abstract = mk_abstract {|a := A(b)
b := B(a)|} in
          let concrete =
            parse {|a:
  | A(b) ~ b "b"
  \ "foo" > "bar"

b:
  | B(a) ~ a "a"
  ; |}
          in
          go abstract concrete;
          [%expect {| There is unavoidable left-recursion in this grammar |}]
        ;;

        let abstract = mk_abstract "foo := Foo()"

        let%expect_test _ =
          let concrete =
            parse
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
            parse {|foo:
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
      end)
    ;;

    let%test_module "pp_term" =
      (module struct
        let go sort tm =
          let tm =
            Lvca_parsing.parse_string_or_failwith
              (Nominal.Term.parse' String.Set.empty)
              tm
          in
          Pp_term.pp_term lang_concrete sort Fmt.stdout tm
        ;;

        let%expect_test _ =
          go "expr" "z";
          [%expect {|z|}]
        ;;

        let%expect_test _ =
          go "expr" "Add(1; 1)";
          [%expect {|1 + 1|}]
        ;;

        let%expect_test _ =
          go "expr" "Add(Add(x; y); z)";
          [%expect {|x + y + z|}]
        ;;

        let%expect_test _ =
          go "expr" "Add(x; Add(y; z))";
          [%expect {|x + (y + z)|}]
        ;;

        let%expect_test _ =
          go "expr" "Add(x; Mul(y; z))";
          [%expect {|x + y * z|}]
        ;;

        let%expect_test _ =
          go "expr" "Add(Mul(x; y); z)";
          [%expect {|x * y + z|}]
        ;;

        let%expect_test _ =
          go "expr" "Mul(Mul(x; y); z)";
          [%expect {|x * y * z|}]
        ;;

        let%expect_test _ =
          go "expr" "Mul(x; Mul(y; z))";
          [%expect {|x * (y * z)|}]
        ;;

        let%expect_test _ =
          go "expr" "Fun(x. x)";
          [%expect {|fun x -> x|}]
        ;;

        let%expect_test _ =
          go "val" "True()";
          [%expect {|true|}]
        ;;

        let%expect_test _ =
          go "val" "False()";
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

        let parser ~sort =
          Unordered.parse_term
            ~input:(Buffer_name "test")
            ~sort
            lang_abstract
            lang_concrete
        ;;

        let parse sort = Lvca_parsing.parse_string_or_failwith (parser ~sort)
        let parse_print start_sort = parse start_sort >> Nominal.Term.pp Fmt.stdout

        (*
        let () =
          let open Stdlib.Format in
          set_formatter_stag_functions Provenance.stag_functions;
          set_tags true;
          set_mark_tags true
        ;;
           *)

        let%expect_test _ =
          parse_print "val" "true";
          [%expect {|True()|}]
        ;;

        let%expect_test _ =
          parse_print "val" "false";
          [%expect {|False()|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "x + x";
          [%expect {|Add(x; x)|}]
        ;;

        (* TODO
        let%expect_test _ =
          parse_print "expr" "fun x -> x";
          [%expect
            {|<{ input = Buffer_name "test"; range = {0,10} }>Fun(<{ input = Input_unknown; range = {4,5} }>x</{ input = Input_unknown; range = {4,5} }>. <{ input = Buffer_name "test"; range = {9,10} }>Val(<{ input = Input_unknown; range = {9,10} }>x</{ input = Input_unknown; range = {9,10} }>)</{ input = Buffer_name "test"; range = {9,10} }>)</{ input = Buffer_name "test"; range = {0,10} }>|}]
        ;;
        *)

        let%expect_test _ =
          parse_print "expr" "true";
          [%expect {|Val(True())|}]
        ;;

        let () =
          Stdlib.Format.set_tags false;
          Stdlib.Format.set_mark_tags false
        ;;

        let%expect_test _ =
          parse_print "expr" "x";
          [%expect {|x|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "x + y + z";
          [%expect {|Add(Add(x; y); z)|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "x + (y + z)";
          [%expect {|Add(x; Add(y; z))|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "x + y * z";
          [%expect {|Add(x; Mul(y; z))|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "x * y + z";
          [%expect {|Add(Mul(x; y); z)|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "x * y * z";
          [%expect {|Mul(Mul(x; y); z)|}]
        ;;

        let%expect_test _ =
          parse_print "expr" "x * (y * z)";
          [%expect {|Mul(x; Mul(y; z))|}]
        ;;
      end)
    ;;
  end)
;;

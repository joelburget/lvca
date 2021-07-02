open Base
open Lvca_util
module SMap = String.Map
module Unordered = Abstract_syntax.Unordered

type ('info, 'rhs) cases = ('info Pattern.t * 'rhs) list
type 'info env = 'info Nonbinding.term Lvca_util.String.Map.t

type 'info matrix_entry =
  { term_no : int
  ; path : Path.t
  ; pattern : 'info Pattern.t
  }

type ('info, 'rhs) matrix_row = 'info matrix_entry list * 'rhs
type ('info, 'rhs) matrix = ('info, 'rhs) matrix_row list

type binding_instruction =
  { term_no : int
  ; name : string
  ; path : Path.t
  }

let pp_instruction ppf = function
  | { term_no; name; path } ->
    Fmt.pf ppf "{ term_no = %n; name = %s; path = %a}" term_no name Path.pp path
;;

type ('info, 'rhs) decision_tree =
  | Operator_cases of
      ('info, 'rhs) decision_tree SMap.t * ('info, 'rhs) decision_tree option
  | Prim_cases of ('info Primitive.All.t option * ('info, 'rhs) decision_tree) list
  | Matched of binding_instruction list * 'rhs
  | Swap of int * ('info, 'rhs) decision_tree

(* TODO: Bad_sort, Redundant_pattern, and Duplicate_name are not used *)
type 'info match_compilation_error =
  | Bad_sort of 'info Pattern.t * 'info Sort.t * 'info Sort.t
  | Redundant_pattern of 'info Pattern.t
  | Non_exhaustive of unit Pattern.t list
  | Duplicate_name of 'info Pattern.t * string

let rec pp_tree ppf = function
  | Operator_cases (branches, default) ->
    let branches = Map.to_alist branches in
    let pp_branches = Fmt.(list (pair string pp_tree ~sep:(any ": ")) ~sep:sp) in
    (match default with
    | None -> Fmt.pf ppf "Operator_cases [@[<hv 2>%a@]]" pp_branches branches
    | Some tree ->
      Fmt.pf ppf "Operator_cases [@[<hv 2>%a@]](%a)" pp_branches branches pp_tree tree)
  | Prim_cases cases ->
    let cases = List.map cases ~f:(fun (_, tree) -> tree) in
    Fmt.pf ppf "Prim_cases [@[<hv 2>%a@]]" Fmt.(list pp_tree ~sep:semi) cases
  | Matched (instrs, _rhs) ->
    Fmt.pf ppf "Matched [%a]" Fmt.(list pp_instruction ~sep:comma) instrs
  | Swap (i, tree) -> Fmt.pf ppf "Swap (@[<hv 2>%n, %a@])" i pp_tree tree
;;

let is_wildcard = function Pattern.Var _ -> true | _ -> false
let sort_name = function Sort.Name (_, name) | Sort.Ap (_, name, _) -> name

let rec match_pattern tm pat =
  match tm, pat with
  | Nonbinding.Operator (_, op_name, subtms), Pattern.Operator (_, pat_name, subpats) ->
    if String.(op_name = pat_name)
    then
      if List.length subtms = List.length subpats
      then
        List.map2_exn subtms subpats ~f:match_pattern
        |> Option.all
        (* Assumption: valid patterns with no repeated variable names *)
        |> Option.map ~f:SMap.unions_right_biased
      else invariant_violation ~here:[%here] "mismatched subterms / patterns"
    else None
  | Primitive pl, Primitive pr ->
    if Primitive.All.equal ~info_eq:(fun _ _ -> true) pl pr then Some SMap.empty else None
  | _, Var (_, name) ->
    Some (if Char.(String.get name 0 = '_') then SMap.empty else SMap.singleton name tm)
  | _, _ -> None
;;

let rec simple_find_match tm cases =
  match cases with
  | [] -> None
  | (pat, rhs) :: cases ->
    (match match_pattern tm pat with
    | None -> simple_find_match tm cases
    | Some env -> Some (rhs, env))
;;

(* Given a sort, produce a mapping of sort variables to concrete sorts it's
   instantiated with. Example:

   Language: list a := nil() | cons(a; list a)
   Sort: list int
   Result:
     a: int
*)
let produce_sort_env lang sort =
  match sort with
  | Sort.Name _ -> SMap.empty
  | Sort.Ap (_, sort_name, args) ->
    (match
       ( Map.find lang.Unordered.externals sort_name
       , Map.find lang.Unordered.sort_defs sort_name )
     with
    | Some _, Some _ ->
      invariant_violation
        ~here:[%here]
        (Printf.sprintf "sort %s both in externals and defined" sort_name)
    | Some (Kind _), _ ->
      invariant_violation
        ~here:[%here]
        (Printf.sprintf "sort (%s) must be defined, not in externals" sort_name)
    | _, Some (Abstract_syntax.Sort_def.Sort_def (ty_vars, _op_defs)) ->
      let ty_vars = ty_vars |> List.map ~f:(fun (name, _kind) -> name) in
      (match List.zip ty_vars args with
      | List.Or_unequal_lengths.Unequal_lengths ->
        invariant_violation ~here:[%here] "sort / args unequal lengths"
      | Ok alist -> SMap.of_alist_exn alist)
    | None, None ->
      invariant_violation ~here:[%here] (Printf.sprintf "sort %s not found" sort_name))
;;

(* Given a sort, produce a mapping from operator name to a list of the concrete
   sorts of its children. Example

   Language: list a := nil() | cons(a; list a)
   Sort: list int
   Result:
     nil: []
     cons: [int; list int]
*)
let get_children_concrete_sorts lang sort =
  let sort_env = produce_sort_env lang sort in
  let sort_name = sort_name sort in
  let (Abstract_syntax.Sort_def.Sort_def (_ty_vars, op_defs)) =
    Map.find_exn lang.Unordered.sort_defs sort_name
  in
  op_defs
  |> List.map ~f:(fun (Operator_def (name, arity)) ->
         let subsorts =
           arity
           |> List.map ~f:(fun (Valence (binders, body_sort) as v) ->
                  if not (List.is_empty binders)
                  then
                    invariant_violation
                      ~here:[%here]
                      (Fmt.str
                         "valence is not a simple sort: %a"
                         Abstract_syntax.Valence.pp
                         v)
                  else Sort.instantiate sort_env body_sort)
         in
         name, subsorts)
  |> SMap.of_alist_exn
;;

let specialize_wildcard ctor_sorts info name wildcard =
  let num_children = List.length ctor_sorts in
  let wildcards =
    match num_children with
    | 0 -> []
    | 1 -> [ wildcard ]
    | _ ->
      let ignores =
        List.init (num_children - 1) ~f:(fun _ -> Pattern.Var (info, "_" ^ name))
      in
      wildcard :: ignores
  in
  wildcards
;;

(* Specialize the matrix for rows where the given constructor matches the first column. *)
let specialize lang ctor_sort tail_sorts ctor_name matrix =
  let concrete_sort_mapping = get_children_concrete_sorts lang ctor_sort in
  let ctor_sorts = Map.find_exn concrete_sort_mapping ctor_name in
  let sorts = ctor_sorts @ tail_sorts in
  let matrix =
    matrix
    |> List.concat_map ~f:(fun (entries, rhs) ->
           let head_entry, entries = List.split_exn entries in
           match head_entry.pattern with
           | Pattern.Operator (_, name, children) ->
             if String.(name = ctor_name)
             then (
               let new_entries =
                 children
                 |> List.mapi ~f:(fun i pattern ->
                        { term_no = head_entry.term_no
                        ; pattern
                        ; path =
                            List.snoc head_entry.path i
                            (* TODO: change paths to be quickly appendable *)
                        })
               in
               [ new_entries @ entries, rhs ])
             else []
           | Var (info, name) ->
             let wildcards =
               specialize_wildcard ctor_sorts info name head_entry.pattern
               |> List.map ~f:(fun pattern -> { head_entry with pattern })
             in
             [ wildcards @ entries, rhs ]
             (* TODO: synthetic info? *)
           | Primitive _ -> [])
  in
  matrix, sorts
;;

(* Retain rows whose first pattern is a wildcard *)
let default matrix =
  matrix
  |> List.concat_map ~f:(fun (entries, rhs) ->
         let head_entry, entries = List.split_exn entries in
         match head_entry.pattern with
         | Pattern.Operator _ -> []
         | Var _ -> [ entries, rhs ]
         | Primitive _ -> [])
;;

let swap_cols matrix i j =
  let rows, rhss = List.unzip matrix in
  let rows = rows |> List.transpose_exn |> List.swap ~i ~j |> List.transpose_exn in
  List.zip_exn rows rhss
;;

let matrix_transpose matrix =
  let rows, _rhss = List.unzip matrix in
  List.transpose_exn rows
;;

let ignore = Pattern.Var ((), "_")

(* Return a pattern vector of size `length sorts` such that all instances are non-matching values *)
let rec check_matrix lang sorts matrix =
  if List.is_empty sorts
  then (match matrix with [] -> Some [] | _ -> None)
  else (
    let transpose = matrix_transpose matrix in
    let first_col, _ = List.split_exn transpose in
    let head_ctors =
      first_col
      |> List.filter_map ~f:(fun { pattern; _ } ->
             match pattern with Pattern.Operator (_, name, _) -> Some name | _ -> None)
      |> String.Set.of_list
    in
    let head_sort, sorts' = List.split_exn sorts in
    let (Abstract_syntax.Sort_def.Sort_def (_ty_vars, op_defs)) =
      Map.find_exn lang.Unordered.sort_defs (sort_name head_sort)
    in
    let is_signature =
      List.for_all op_defs ~f:(fun (Operator_def (name, _arity)) ->
          Set.mem head_ctors name)
    in
    if is_signature
    then
      let open Option.Let_syntax in
      head_ctors
      |> Set.to_list
      |> List.find_map ~f:(fun ctor_name ->
             let (Operator_def (_, ctor_arity)) =
               op_defs
               |> List.find_exn ~f:(fun (Operator_def (name, _arity)) ->
                      String.(name = ctor_name))
             in
             let ctor_arity = List.length ctor_arity in
             let matrix, sorts = specialize lang head_sort sorts' ctor_name matrix in
             let%map pat_vec = check_matrix lang sorts matrix in
             let subpats, pat_vec = List.split_n pat_vec ctor_arity in
             Pattern.Operator ((), ctor_name, subpats) :: pat_vec)
    else
      check_matrix lang sorts' (default matrix)
      |> Option.map ~f:(fun example ->
             let head =
               if Set.is_empty head_ctors
               then ignore
               else (
                 let (Operator_def (ctor_name, ctor_arity)) =
                   op_defs
                   (* Find an operator not listed *)
                   |> List.find_exn ~f:(fun (Operator_def (name, _)) ->
                          not (Set.mem head_ctors name))
                 in
                 let wildcards = List.map ctor_arity ~f:(fun _ -> ignore) in
                 Pattern.Operator ((), ctor_name, wildcards))
             in
             head :: example))
;;

let rec compile_matrix lang sorts matrix =
  let open Result.Let_syntax in
  match matrix with
  | [] ->
    (* If the matrix has no rows, matching would always fail. Give a list of
       wildcards as an example. *)
    let example = List.map sorts ~f:(fun _ -> ignore) in
    Error (Non_exhaustive example)
  | (row_entries, rhs) :: _ ->
    (* If the first row is all wildcards (including the zero column case), match. *)
    if List.for_all row_entries ~f:(fun { pattern; _ } -> is_wildcard pattern)
    then (
      let instructions =
        List.filter_map row_entries ~f:(fun { term_no; pattern; path } ->
            match pattern with
            | Pattern.Var (_, name) when Lvca_util.String.is_ignore name -> None
            | Pattern.Var (_, name) -> Some { term_no; name; path }
            | _ -> None)
      in
      Ok (Matched (instructions, rhs))
      (* Otherwise, find the first column with a non-wildcard entry. *))
    else (
      let transpose = matrix_transpose matrix in
      let i =
        (* find first column which is not all wildcards *)
        let col_no =
          List.findi transpose ~f:(fun _ column ->
              not (List.for_all column ~f:(fun { pattern; _ } -> is_wildcard pattern)))
        in
        match col_no with
        | None -> invariant_violation ~here:[%here] "no column which is not all wildcards"
        | Some (i, _) -> i
      in
      (* the first column is a non-wildcard *)
      if Int.(i = 0)
      then (
        let non_wildcard_col = List.nth_exn transpose i in
        let head_ctors =
          non_wildcard_col
          |> List.filter_map ~f:(fun { pattern; _ } ->
                 match pattern with
                 | Pattern.Operator (_, name, _) -> Some name
                 | _ -> None)
          |> String.Set.of_list
        in
        let head_sort, tail_sorts = List.split_exn sorts in
        let (Abstract_syntax.Sort_def.Sort_def (_ty_vars, op_defs)) =
          Map.find_exn lang.Unordered.sort_defs (sort_name head_sort)
        in
        (* is every constructor covered? *)
        let is_signature =
          List.for_all op_defs ~f:(fun (Operator_def (name, _arity)) ->
              Set.mem head_ctors name)
        in
        let%bind default_case =
          if is_signature
          then Ok None
          else (
            let%map matrix = compile_matrix lang sorts (default matrix) in
            Some matrix)
        in
        let%map branches_alist =
          head_ctors
          |> Set.to_list
          |> List.map ~f:(fun ctor_name ->
                 let matrix, sorts =
                   specialize lang head_sort tail_sorts ctor_name matrix
                 in
                 let%map decision_tree = compile_matrix lang sorts matrix in
                 ctor_name, decision_tree)
          |> Result.all
        in
        Operator_cases (SMap.of_alist_exn branches_alist, default_case)
        (* swap columns so the first is a non-wildcard *))
      else (
        let matrix = swap_cols matrix 0 i in
        let sorts = List.swap sorts ~i ~j:0 in
        let%map matrix = compile_matrix lang sorts matrix in
        Swap (i, matrix)))
;;

let compile_cases lang sort cases =
  compile_matrix
    lang
    [ sort ]
    (List.map cases ~f:(fun (pattern, rhs) ->
         [ { term_no = 0; pattern; path = [] } ], rhs))
;;

let run_matches tms tree =
  let rec go tms' tree =
    match tms', tree with
    | _, Matched (instrs, rhs) ->
      let env_list =
        instrs
        |> List.map ~f:(fun { term_no; name; path } ->
               match List.nth tms term_no with
               | Some tm ->
                 (match Nonbinding.select_path tm ~path with
                 | Error msg ->
                   failwith
                     (Fmt.str
                        "trying to select invalid path (%a) in term %a: %s"
                        Path.pp
                        path
                        Nonbinding.pp
                        tm
                        msg)
                 | Ok tm -> name, tm)
               | None ->
                 failwith
                   (Printf.sprintf
                      "run_matches: trying to select invalid term number %n (of %n terms)"
                      term_no
                      (List.length tms)))
      in
      let env =
        match SMap.of_alist env_list with
        | `Ok env -> env
        | `Duplicate_key name ->
          invariant_violation
            ~here:[%here]
            Fmt.(
              str
                "duplicate key: %s (instrs: [%a], terms: (%a)"
                name
                (list ~sep:comma pp_instruction)
                instrs
                (list ~sep:comma Nonbinding.pp)
                tms)
      in
      Some (rhs, env)
    | [], _ -> invariant_violation ~here:[%here] "empty pattern but not matched"
    | Nonbinding.Operator (_, op_name, subtms) :: tms', Operator_cases (branches, default)
      ->
      let branch =
        match Map.find branches op_name, default with
        | Some branch, _ -> branch
        | None, Some branch -> branch
        | _, _ ->
          let op_names = branches |> Map.keys |> String.concat ~sep:", " in
          invariant_violation
            ~here:[%here]
            (Printf.sprintf
               "expected branch %s (found [%s]) (no default)"
               op_name
               op_names)
      in
      go (subtms @ tms') branch
    | _, Operator_cases _ ->
      invariant_violation ~here:[%here] "Operator_cases matched with non-operator"
    | Nonbinding.Primitive prim :: tms', Prim_cases branches ->
      let found =
        branches
        |> List.find ~f:(fun (prim', _) ->
               match prim' with
               | Some prim' -> Primitive.All.equal ~info_eq:(fun _ _ -> true) prim prim'
               | None -> true)
      in
      (match found with
      | None -> None (* failwith "TODO: error -- no matching primitive" *)
      | Some (_, branch) -> go tms' branch)
    | _, Prim_cases _ ->
      invariant_violation
        ~here:[%here]
        "decision_tree.Prim_cases paired with non-Primitive"
    | _, Swap (i, tree) -> go (List.swap tms' ~i ~j:0) tree
  in
  go tms tree
;;

let run_match tm tree = run_matches [ tm ] tree

module Properties = struct
  type term = unit Nonbinding.term

  let match_equivalent tm cases =
    let tmeq = Nonbinding.equal ~info_eq:Unit.( = ) in
    let ( = ) = Option.equal (Lvca_util.Tuple2.equal tmeq (Map.equal tmeq)) in
    let result1 = simple_find_match tm cases in
    let lang = failwith "TODO 5" in
    let sort = failwith "TODO 6" in
    match compile_cases lang sort cases with
    | Ok decision_tree ->
      let result2 = run_match tm decision_tree in
      if result1 = result2 then Property_result.Ok else Failed "match result not equal"
    | Error _ -> failwith "TODO: error 9"
  ;;
end

module Parse = struct
  open Lvca_parsing

  type 'info matrix_row = 'info matrix_entry list * 'info Nonbinding.term

  let branch =
    lift3 (fun pat _ tm -> pat, tm) Pattern.Parse.t (string "->") Nonbinding.Parse.term
    <?> "branch"
  ;;

  let branches = option '|' (char '|') *> sep_by1 (char '|') branch <?> "branches"

  let matrix_row =
    lift3
      (fun pats _ tm ->
        List.mapi pats ~f:(fun term_no pattern -> { term_no; pattern; path = [] }), tm)
      (sep_by1 (char ',') Pattern.Parse.t)
      (string "->")
      Nonbinding.Parse.term
    <?> "matrix_row"
  ;;

  let matrix_rows =
    option '|' (char '|') *> sep_by1 (char '|') matrix_row <?> "matrix_rows"
  ;;
end

let%test_module "Matching" =
  (module struct
    let pp_env ppf env =
      let open Fmt in
      let pp_entry ppf (key, data) = Fmt.pf ppf "%s -> %a" key Nonbinding.pp data in
      list ~sep:comma pp_entry ppf (Map.to_alist env)
    ;;

    let pp_result ppf (rhs, env) = Fmt.pf ppf "{%a} %a" pp_env env Nonbinding.pp rhs
    let parse p str = Lvca_parsing.(parse_string (whitespace *> p) str)

    let run_simple_match branches_str tm_str =
      match parse Parse.branches branches_str, parse Nonbinding.Parse.term tm_str with
      | Ok branches, Ok tm ->
        (match simple_find_match tm branches with
        | None -> Fmt.pr "no match"
        | Some result -> pp_result Fmt.stdout result)
      | _ -> failwith "something failed to parse"
    ;;

    let%expect_test _ =
      run_simple_match "_ -> 1" "foo()";
      [%expect "{} 1"]
    ;;

    let%expect_test _ =
      run_simple_match "bar() -> 1" "foo()";
      [%expect "no match"]
    ;;

    let run_compiled_matches syntax_str sorts_str matrix_str tms_str =
      match
        ( parse Abstract_syntax.Parse.t syntax_str
        , parse Lvca_parsing.(sep_by (char ',') Sort.Parse.t) sorts_str
        , parse Parse.matrix_rows matrix_str
        , parse Lvca_parsing.(sep_by (char ',') Nonbinding.Parse.term) tms_str )
      with
      | Error syntax_msg, _, _, _ -> failwith ("syntax failed to parse: " ^ syntax_msg)
      | _, Error sorts_msg, _, _ -> failwith ("sorts failed to parse: " ^ sorts_msg)
      | _, _, Error matrix_msg, _ -> failwith ("matrix failed to parse: " ^ matrix_msg)
      | _, _, _, Error tm_msg -> failwith ("term failed to parse: " ^ tm_msg)
      | Ok syntax, Ok sorts, Ok matrix_rows, Ok tms ->
        let syntax =
          match Abstract_syntax.mk_unordered syntax with
          | `Ok syntax -> syntax
          | `Duplicate_key name -> failwith (Printf.sprintf "duplicate key: %s" name)
        in
        let decision_tree =
          match compile_matrix syntax sorts matrix_rows with
          | Ok tree -> tree
          | Error _msg -> failwith "failed to compile decision tree"
        in
        (match run_matches tms decision_tree with
        | None -> Fmt.pr "no match"
        | Some result -> pp_result Fmt.stdout result)
    ;;

    let bool_lang = "bool := t() | f()"

    let list_lang = {|list a := nil() | cons(a; list a)
      unit := unit()
    |}

    let three_bool_match =
      {|| _, f(), t() -> 1
        | f(), t(), _ -> 2
        | _, _, f() -> 3
        | _, _, t() -> 4
      |}
    ;;

    let run_three_bool_match tms =
      run_compiled_matches bool_lang "bool, bool, bool" three_bool_match tms
    ;;

    let%expect_test _ =
      run_three_bool_match "t(), f(), t()";
      [%expect "{} 1"]
    ;;

    let%expect_test _ =
      run_three_bool_match "f(), t(), t()";
      [%expect "{} 2"]
    ;;

    let%expect_test _ =
      run_three_bool_match "f(), f(), f()";
      [%expect "{} 3"]
    ;;

    let%expect_test _ =
      run_three_bool_match "t(), t(), t()";
      [%expect "{} 4"]
    ;;

    let merge_match =
      {|| nil(), x -> 1
        | x, nil() -> 2
        | cons(x; y), cons(z; w) -> 3
      |}
    ;;

    let run_list_match tms =
      run_compiled_matches list_lang "list unit, list unit" merge_match tms
    ;;

    let%expect_test _ =
      run_list_match "nil(), cons(unit(); nil())";
      [%expect "{x -> cons(unit(); nil())} 1"]
    ;;

    let%expect_test _ =
      run_list_match "cons(unit(); nil()), nil()";
      [%expect "{x -> cons(unit(); nil())} 2"]
    ;;

    let%expect_test _ =
      run_list_match "cons(unit(); nil()), cons(unit(); nil())";
      [%expect {|
        {w -> nil(), x -> unit(), y -> nil(),
        z -> unit()} 3|}]
    ;;

    let print_check syntax_str sorts_str matrix_str =
      match
        ( parse Abstract_syntax.Parse.t syntax_str
        , parse Lvca_parsing.(sep_by (char ',') Sort.Parse.t) sorts_str
        , parse Parse.matrix_rows matrix_str )
      with
      | Ok syntax, Ok sorts, Ok matrix ->
        let syntax =
          match Abstract_syntax.mk_unordered syntax with
          | `Ok syntax -> syntax
          | `Duplicate_key name -> failwith (Printf.sprintf "duplicate key: %s" name)
        in
        (match check_matrix syntax sorts matrix with
        | None -> Fmt.pr "okay"
        | Some example -> Fmt.(list ~sep:comma Pattern.pp Fmt.stdout example))
      | _ -> failwith "something failed to parse"
    ;;

    let%expect_test _ =
      print_check list_lang "list unit" "nil() -> 1";
      [%expect "cons(_; _)"]
    ;;

    let%expect_test _ =
      print_check list_lang "list unit" "cons(_; _) -> 1";
      [%expect "nil()"]
    ;;

    let%expect_test _ =
      print_check list_lang "list unit" {|| nil() -> 1
        | cons(_; nil()) -> 2 |};
      [%expect "cons(_; cons(_; _))"]
    ;;

    let%expect_test _ =
      print_check bool_lang "bool, bool, bool" three_bool_match;
      [%expect "okay"]
    ;;

    let%expect_test _ =
      print_check
        bool_lang
        "bool, bool, bool"
        {|| _, f(), t() -> 1
        | f(), t(), _ -> 2
        | _, _, f() -> 3
      |};
      [%expect {|
        t(), t(),
        t()|}]
    ;;

    let%expect_test _ =
      print_check
        bool_lang
        "bool, bool, bool"
        {|| _, f(), t() -> 1
        | t(), t(), _ -> 2
        | _, _, f() -> 3
      |};
      [%expect {|
        f(), t(),
        t()|}]
    ;;
  end)
;;

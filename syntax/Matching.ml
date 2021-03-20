open Base
module Util = Lvca_util
module SMap = Lvca_util.String.Map
module Unordered = AbstractSyntax.Unordered

type ('info, 'prim, 'rhs) cases = (('info, 'prim) Pattern.t * 'rhs) list
type ('info, 'prim) env = ('info, 'prim) NonBinding.term Lvca_util.String.Map.t

type ('info, 'prim) matrix_entry =
  { term_no : int
  ; path : Path.t
  ; pattern : ('info, 'prim) Pattern.t
  }

type ('info, 'prim, 'rhs) matrix_row = ('info, 'prim) matrix_entry list * 'rhs
type ('info, 'prim, 'rhs) matrix = ('info, 'prim, 'rhs) matrix_row list

type binding_instruction =
  { term_no : int
  ; name : string
  ; path : Path.t
  }

let pp_instruction ppf = function
  | { term_no; name; path } ->
    Fmt.pf ppf "{ term_no = %n; name = %s; path = %a}" term_no name Path.pp path
;;

type ('info, 'prim, 'rhs) decision_tree =
  | OperatorCases of
      ('info, 'prim, 'rhs) decision_tree SMap.t
      * ('info, 'prim, 'rhs) decision_tree option
  | PrimCases of ('prim option * ('info, 'prim, 'rhs) decision_tree) list
  | Matched of binding_instruction list * 'rhs
  | Swap of int * ('info, 'prim, 'rhs) decision_tree

type ('info, 'prim) match_compilation_error =
  | BadSort of ('info, 'prim) Pattern.t * 'info Sort.t * 'info Sort.t
  | RedundantPattern of ('info, 'prim) Pattern.t
  | NonExhaustive of (unit, 'prim) Pattern.t list
  | DuplicateName of ('info, 'prim) Pattern.t * string

let rec pp_tree ppf = function
  | OperatorCases (branches, default) ->
    let branches = Map.to_alist branches in
    let pp_branches = Fmt.(list (pair string pp_tree ~sep:(any ": ")) ~sep:sp) in
    (match default with
    | None -> Fmt.pf ppf "OperatorCases [@[<hv 2>%a@]]" pp_branches branches
    | Some tree ->
      Fmt.pf ppf "OperatorCases [@[<hv 2>%a@]](%a)" pp_branches branches pp_tree tree)
  | PrimCases cases ->
    let cases = List.map cases ~f:(fun (_, tree) -> tree) in
    Fmt.pf ppf "PrimCases [@[<hv 2>%a@]]" Fmt.(list pp_tree ~sep:semi) cases
  | Matched (instrs, _rhs) ->
    Fmt.pf ppf "Matched [%a]" Fmt.(list pp_instruction ~sep:comma) instrs
  | Swap (i, tree) -> Fmt.pf ppf "Swap (@[<hv 2>%n, %a@])" i pp_tree tree
;;

let is_wildcard = function Pattern.Var _ | Ignored _ -> true | _ -> false
let sort_name = function Sort.Name (_, name) | Sort.Ap (_, name, _) -> name

let rec match_pattern ~prim_eq tm pat =
  match tm, pat with
  | NonBinding.Operator (_, op_name, subtms), Pattern.Operator (_, pat_name, subpats) ->
    if String.(op_name = pat_name)
    then
      if List.length subtms = List.length subpats
      then
        List.map2_exn subtms subpats ~f:(match_pattern ~prim_eq)
        |> Option.all
        (* Assumption: valid patterns with no repeated variable names *)
        |> Option.map ~f:SMap.unions_right_biased
      else Util.invariant_violation "match_pattern: mismatched subterms / patterns"
    else None
  | Primitive (_, pl), Primitive (_, pr) ->
    if prim_eq pl pr then Some SMap.empty else None
  | _, Ignored _ -> Some SMap.empty
  | _, Var (_, name) -> Some (SMap.singleton name tm)
  | _, _ -> None
;;

let rec simple_find_match ~prim_eq tm cases =
  match cases with
  | [] -> None
  | (pat, rhs) :: cases ->
    (match match_pattern ~prim_eq tm pat with
    | None -> simple_find_match ~prim_eq tm cases
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
      Util.invariant_violation
        (Printf.sprintf
           "produce_sort_env: sort %s both in externals and defined"
           sort_name)
    | Some (Kind _), _ ->
      Util.invariant_violation
        (Printf.sprintf
           "produce_sort_env: sort (%s) must be defined, not in externals"
           sort_name)
    | _, Some (AbstractSyntax.SortDef.SortDef (ty_vars, _op_defs)) ->
      let ty_vars = ty_vars |> List.map ~f:(fun (name, _kind) -> name) in
      (match List.zip ty_vars args with
      | List.Or_unequal_lengths.Unequal_lengths ->
        Util.invariant_violation "produce_sort_env: sort / args unequal lengths"
      | Ok alist -> SMap.of_alist_exn alist)
    | None, None ->
      Util.invariant_violation
        (Printf.sprintf "produce_sort_env: sort %s not found" sort_name))
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
  let (AbstractSyntax.SortDef.SortDef (_ty_vars, op_defs)) =
    Map.find_exn lang.Unordered.sort_defs sort_name
  in
  op_defs
  |> List.map ~f:(fun (OperatorDef (name, arity)) ->
         let subsorts =
           arity
           |> List.map ~f:(fun (Valence (binders, body_sort) as v) ->
                  if not (List.is_empty binders)
                  then
                    Util.invariant_violation
                      (Printf.sprintf
                         "get_children_concrete_sorts: valence is not a simple sort: %s"
                         (AbstractSyntax.Valence.to_string v))
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
        List.init (num_children - 1) ~f:(fun _ -> Pattern.Ignored (info, name))
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
           let head_entry, entries = Util.List.split_exn entries in
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
                            Util.List.snoc head_entry.path i
                            (* TODO: change paths to be quickly appendable *)
                        })
               in
               [ new_entries @ entries, rhs ])
             else []
           | Pattern.Var (info, name) | Ignored (info, name) ->
             let wildcards =
               specialize_wildcard ctor_sorts info name head_entry.pattern
               |> List.map ~f:(fun pattern -> { head_entry with pattern })
             in
             [ wildcards @ entries, rhs ]
             (* TODO: synthetic info? *)
           | Pattern.Primitive _ -> [])
  in
  matrix, sorts
;;

(* Retain rows whose first pattern is a wildcard *)
let default matrix =
  matrix
  |> List.concat_map ~f:(fun (entries, rhs) ->
         let head_entry, entries = Util.List.split_exn entries in
         match head_entry.pattern with
         | Pattern.Operator _ -> []
         | Pattern.Var _ | Ignored _ -> [ entries, rhs ]
         | Pattern.Primitive _ -> [])
;;

let swap_cols matrix i j =
  let rows, rhss = List.unzip matrix in
  let rows = rows |> List.transpose_exn |> Util.List.swap ~i ~j |> List.transpose_exn in
  List.zip_exn rows rhss
;;

let matrix_transpose matrix =
  let rows, _rhss = List.unzip matrix in
  List.transpose_exn rows
;;

(* Return a pattern vector of size `length sorts` such that all instances are non-matching values *)
let rec check_matrix lang sorts matrix =
  if List.is_empty sorts
  then (match matrix with [] -> Some [] | _ -> None)
  else (
    let transpose = matrix_transpose matrix in
    let first_col, _ = Util.List.split_exn transpose in
    let head_ctors =
      first_col
      |> List.filter_map ~f:(fun { pattern; _ } ->
             match pattern with Pattern.Operator (_, name, _) -> Some name | _ -> None)
      |> Util.String.Set.of_list
    in
    let head_sort, sorts' = Util.List.split_exn sorts in
    let (AbstractSyntax.SortDef.SortDef (_ty_vars, op_defs)) =
      Map.find_exn lang.Unordered.sort_defs (sort_name head_sort)
    in
    let is_signature =
      List.for_all op_defs ~f:(fun (OperatorDef (name, _arity)) ->
          Set.mem head_ctors name)
    in
    if is_signature
    then
      let open Option.Let_syntax in
      head_ctors
      |> Set.to_list
      |> List.find_map ~f:(fun ctor_name ->
             let (OperatorDef (_, ctor_arity)) =
               op_defs
               |> List.find_exn ~f:(fun (OperatorDef (name, _arity)) ->
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
             let ignore = Pattern.Ignored ((), "") in
             let head =
               if Set.is_empty head_ctors
               then ignore
               else (
                 let (OperatorDef (ctor_name, ctor_arity)) =
                   op_defs
                   (* Find an operator not listed *)
                   |> List.find_exn ~f:(fun (OperatorDef (name, _)) ->
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
    let ignore = Pattern.Ignored ((), "") in
    let example = List.map sorts ~f:(fun _ -> ignore) in
    Error (NonExhaustive example)
  | (row_entries, rhs) :: _ ->
    (* If the first row is all wildcards (including the zero column case), match. *)
    if List.for_all row_entries ~f:(fun { pattern; _ } -> is_wildcard pattern)
    then (
      let instructions =
        List.filter_map row_entries ~f:(fun { term_no; pattern; path } ->
            match pattern with
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
        | None ->
          Util.invariant_violation "compile_matrix: no column which is not all wildcards"
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
          |> Util.String.Set.of_list
        in
        let head_sort, tail_sorts = Util.List.split_exn sorts in
        let (AbstractSyntax.SortDef.SortDef (_ty_vars, op_defs)) =
          Map.find_exn lang.Unordered.sort_defs (sort_name head_sort)
        in
        (* is every constructor covered? *)
        let is_signature =
          List.for_all op_defs ~f:(fun (OperatorDef (name, _arity)) ->
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
        OperatorCases (SMap.of_alist_exn branches_alist, default_case)
        (* swap columns so the first is a non-wildcard *))
      else (
        let matrix = swap_cols matrix 0 i in
        let sorts = Util.List.swap sorts ~i ~j:0 in
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

let run_matches ~prim_pp ~prim_eq tms tree =
  let rec go tms' tree =
    match tms', tree with
    | _, Matched (instrs, rhs) ->
      let env_list =
        instrs
        |> List.map ~f:(fun { term_no; name; path } ->
               match List.nth tms term_no with
               | Some tm ->
                 (match NonBinding.select_path tm ~path with
                 | Error msg ->
                   failwith
                     (Printf.sprintf
                        "run_matches trying to select invalid path (%s) in term %s: %s"
                        (Fmt.to_to_string Path.pp path)
                        (Fmt.to_to_string (NonBinding.pp prim_pp) tm)
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
          Util.invariant_violation
            (Printf.sprintf
               "run_matches: duplicate key: %s (instrs: [%s], terms: (%s)"
               name
               (instrs
               |> List.map ~f:(Fmt.to_to_string pp_instruction)
               |> String.concat ~sep:", ")
               (tms
               |> List.map ~f:(Fmt.to_to_string (NonBinding.pp prim_pp))
               |> String.concat ~sep:", "))
      in
      Some (rhs, env)
    | [], _ -> Util.invariant_violation "run_matches: empty pattern but not matched"
    | NonBinding.Operator (_, op_name, subtms) :: tms', OperatorCases (branches, default)
      ->
      let branch =
        match Map.find branches op_name, default with
        | Some branch, _ -> branch
        | None, Some branch -> branch
        | _, _ ->
          let op_names = branches |> Map.keys |> String.concat ~sep:", " in
          Util.invariant_violation
            (Printf.sprintf
               "expected branch %s (found [%s]) (no default)"
               op_name
               op_names)
      in
      go (subtms @ tms') branch
    | _, OperatorCases _ ->
      Util.invariant_violation "OperatorCases matched with non-operator"
    | NonBinding.Primitive (_, prim) :: tms', PrimCases branches ->
      let found =
        branches
        |> List.find ~f:(fun (prim', _) ->
               match prim' with Some prim' -> prim_eq prim prim' | None -> true)
      in
      (match found with
      | None -> None (* failwith "TODO: error -- no matching primitive" *)
      | Some (_, branch) -> go tms' branch)
    | _, PrimCases _ ->
      Util.invariant_violation "decision_tree.PrimCases paired with non-Primitive"
    | _, Swap (i, tree) -> go (Util.List.swap tms' ~i ~j:0) tree
  in
  go tms tree
;;

let run_match ~prim_pp ~prim_eq tm tree = run_matches ~prim_pp ~prim_eq [ tm ] tree

module Properties = struct
  type term = (unit, Primitive.t) NonBinding.term

  let match_equivalent tm cases =
    let tmeq = NonBinding.equal Unit.( = ) Primitive.( = ) in
    let ( = ) = Option.equal (Lvca_util.Tuple2.equal tmeq (Map.equal tmeq)) in
    let result1 = simple_find_match ~prim_eq:Primitive.( = ) tm cases in
    let lang = failwith "TODO 5" in
    let sort = failwith "TODO 6" in
    match compile_cases lang sort cases with
    | Ok decision_tree ->
      let result2 =
        run_match ~prim_pp:Primitive.pp ~prim_eq:Primitive.( = ) tm decision_tree
      in
      if result1 = result2 then PropertyResult.Ok else Failed "match result not equal"
    | Error _ -> failwith "TODO: error 9"
  ;;
end

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Pattern = Pattern.Parse (Comment)
  module Term = NonBinding.Parse (Comment)
  module ParsePrimitive = Primitive.Parse (Comment)
  open Parsers

  let branch =
    lift3
      (fun pat _ tm -> pat, tm)
      (Pattern.t ParsePrimitive.t)
      (string "->")
      (Term.term ParsePrimitive.t)
    <?> "branch"
  ;;

  let branches = option '|' (char '|') *> sep_by1 (char '|') branch <?> "branches"

  let matrix_row =
    lift3
      (fun pats _ tm ->
        List.mapi pats ~f:(fun term_no pattern -> { term_no; pattern; path = [] }), tm)
      (sep_by1 (char ',') (Pattern.t ParsePrimitive.t))
      (string "->")
      (Term.term ParsePrimitive.t)
    <?> "matrix_row"
  ;;

  let matrix_rows =
    option '|' (char '|') *> sep_by1 (char '|') matrix_row <?> "matrix_rows"
  ;;
end

let%test_module "Matching" =
  (module struct
    module Parsers = ParseUtil.Mk (ParseUtil.NoComment)
    module Parse = Parse (ParseUtil.NoComment)
    module ParseSyntax = AbstractSyntax.Parse (ParseUtil.NoComment)
    module ParseSort = Sort.Parse (ParseUtil.NoComment)
    module ParseTerm = NonBinding.Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)

    let str_of_tm tm = Fmt.to_to_string (NonBinding.pp Primitive.pp) tm
    let str_of_pat tm = Fmt.to_to_string (Pattern.pp Primitive.pp) tm

    let str_of_env env =
      env
      |> Map.to_alist
      |> List.map ~f:(fun (key, data) -> Printf.sprintf "%s -> %s" key (str_of_tm data))
      |> String.concat ~sep:", "
    ;;

    let str_of_result (rhs, env) =
      Printf.sprintf "{%s} %s" (str_of_env env) (str_of_tm rhs)
    ;;

    let run_simple_match branches_str tm_str =
      match
        ( ParseUtil.parse_string Parse.branches branches_str
        , ParseUtil.parse_string (ParseTerm.whitespace_term ParsePrimitive.t) tm_str )
      with
      | Ok branches, Ok tm ->
        (match simple_find_match ~prim_eq:Primitive.( = ) tm branches with
        | None -> Stdio.print_string "no match"
        | Some result -> Stdio.print_string (str_of_result result))
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
        ( ParseUtil.parse_string ParseSyntax.whitespace_t syntax_str
        , ParseUtil.parse_string Parsers.(sep_by (char ',') ParseSort.t) sorts_str
        , ParseUtil.parse_string Parse.matrix_rows matrix_str
        , ParseUtil.parse_string
            Parsers.(sep_by (char ',') (ParseTerm.whitespace_term ParsePrimitive.t))
            tms_str )
      with
      | Error syntax_msg, _, _, _ -> failwith ("syntax failed to parse: " ^ syntax_msg)
      | _, Error sorts_msg, _, _ -> failwith ("sorts failed to parse: " ^ sorts_msg)
      | _, _, Error matrix_msg, _ -> failwith ("matrix failed to parse: " ^ matrix_msg)
      | _, _, _, Error tm_msg -> failwith ("term failed to parse: " ^ tm_msg)
      | Ok syntax, Ok sorts, Ok matrix_rows, Ok tms ->
        let syntax =
          match AbstractSyntax.mk_unordered syntax with
          | `Ok syntax -> syntax
          | `Duplicate_key name -> failwith (Printf.sprintf "duplicate key: %s" name)
        in
        let decision_tree =
          match compile_matrix syntax sorts matrix_rows with
          | Ok tree -> tree
          | Error _msg -> failwith "failed to compile decision tree"
        in
        (match
           run_matches ~prim_pp:Primitive.pp ~prim_eq:Primitive.( = ) tms decision_tree
         with
        | None -> Stdio.print_string "no match"
        | Some result -> Stdio.print_string (str_of_result result))
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
      [%expect "{w -> nil(), x -> unit(), y -> nil(), z -> unit()} 3"]
    ;;

    let print_check syntax_str sorts_str matrix_str =
      match
        ( ParseUtil.parse_string ParseSyntax.whitespace_t syntax_str
        , ParseUtil.parse_string Parsers.(sep_by (char ',') ParseSort.t) sorts_str
        , ParseUtil.parse_string Parse.matrix_rows matrix_str )
      with
      | Ok syntax, Ok sorts, Ok matrix ->
        let syntax =
          match AbstractSyntax.mk_unordered syntax with
          | `Ok syntax -> syntax
          | `Duplicate_key name -> failwith (Printf.sprintf "duplicate key: %s" name)
        in
        (match check_matrix syntax sorts matrix with
        | None -> Stdio.print_string "okay"
        | Some example ->
          Stdio.print_string (example |> List.map ~f:str_of_pat |> String.concat ~sep:", "))
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
      [%expect "t(), t(), t()"]
    ;;

    let%expect_test _ =
      print_check
        bool_lang
        "bool, bool, bool"
        {|| _, f(), t() -> 1
        | t(), t(), _ -> 2
        | _, _, f() -> 3
      |};
      [%expect "f(), t(), t()"]
    ;;
  end)
;;

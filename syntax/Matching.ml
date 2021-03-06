open Base
module Util = Lvca_util
module SMap = Lvca_util.String.Map

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

let string_of_instruction = function
  | { term_no; name; path } ->
    Printf.sprintf
      "{ term_no = %n; name = %s; path = %s}"
      term_no
      name
      (Fmt.to_to_string Path.pp path)
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
  | NonExhaustive of ('info, 'prim) Pattern.t
  | DuplicateName of ('info, 'prim) Pattern.t * string

(*
let rec string_of_tree = function
  | OperatorCases (branches, default) ->
    let branches =
      branches
      |> Map.to_alist
      |> List.map ~f:(fun (key, tree) ->
             Printf.sprintf "%s -> %s" key (string_of_tree tree))
      |> String.concat ~sep:", "
    in
    let default =
      match default with
      | None -> ""
      | Some tree -> Printf.sprintf "(%s)" (string_of_tree tree)
    in
    Printf.sprintf "OperatorCases [%s]%s" branches default
  | PrimCases cases ->
    let cases =
      cases
      |> List.map ~f:(fun (_, tree) -> string_of_tree tree)
      |> String.concat ~sep:", "
    in
    Printf.sprintf "PrimCases [%s]" cases
  (* of ('prim option * ('info, 'prim, 'rhs) decision_tree) list *)
  | Matched _ -> "matched"
  | Swap (i, tree) -> Printf.sprintf "Swap (%n, %s)" i (string_of_tree tree)
;;

let string_of_matrix matrix =
  matrix
  |> List.map ~f:(fun (pats, _rhs) ->
         pats |> List.map ~f:(Pattern.to_string Primitive.pp) |> String.concat ~sep:"\t")
  |> String.concat ~sep:"\n"
;;
*)

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

let rec simple_match ~prim_eq tm cases =
  match cases with
  | [] -> None
  | (pat, rhs) :: cases ->
    (match match_pattern ~prim_eq tm pat with
    | None -> simple_match ~prim_eq tm cases
    | Some env -> Some (rhs, env))
;;

let get_arity lang sort_name ctor_name =
  let (AbstractSyntax.SortDef (_vars, op_defs)) = Map.find_exn lang sort_name in
  let (OperatorDef (_name, arity)) =
    List.find_exn op_defs ~f:(fun (OperatorDef (name, _)) -> String.(name = ctor_name))
  in
  arity
;;

(* Specialize the matrix for rows where the given constructor matches the first column. *)
let specialize lang sort sorts ctor_name matrix =
  let arity = get_arity lang (sort_name sort) ctor_name in
  let arity' =
    arity
    |> List.map ~f:(fun (Valence (binders, body_sort)) ->
           if List.is_empty binders
           then body_sort
           else failwith "TODO: error non-sort valence")
  in
  let num_children = List.length arity in
  let sorts =
    match sorts with [] -> failwith "TODO: error 4" | _ :: sorts' -> arity' @ sorts'
  in
  let matrix =
    matrix
    |> List.concat_map ~f:(fun (entries, rhs) ->
           let head_entry, entries =
             match entries with x :: xs -> x, xs | [] -> failwith "TODO 1"
           in
           match head_entry.pattern with
           | Pattern.Operator (_, name, children) ->
             if String.(name = ctor_name)
             then (
               let new_entries =
                 children
                 |> List.mapi ~f:(fun i pattern ->
                        { term_no = head_entry.term_no
                        ; pattern
                        ; path = Util.List.snoc head_entry.path i
                        })
               in
               [ new_entries @ entries, rhs ])
             else []
           | Pattern.Var (info, name) | Ignored (info, name) ->
             (* TODO: synthetic info? *)
             let ignores =
               match num_children with
               | 0 -> []
               | 1 -> [ head_entry ]
               | _ ->
                 List.init num_children ~f:(fun _ ->
                     { head_entry with pattern = Pattern.Ignored (info, name) })
             in
             [ ignores @ entries, rhs ]
           | Pattern.Primitive _ -> [])
  in
  matrix, sorts
;;

(* Retain rows whose first pattern is a wildcard *)
let default matrix =
  matrix
  |> List.concat_map ~f:(fun (entries, rhs) ->
         let head_entry, entries =
           match entries with x :: xs -> x, xs | [] -> failwith "TODO 2"
         in
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

let rec compile_matrix lang sorts matrix =
  match matrix with
  | [] -> failwith "TODO: fail?"
  | first_row :: _ ->
    let entries, rhs = first_row in
    (* If the first row is all wildcards (including the zero column case), match. *)
    if List.for_all entries ~f:(fun { pattern; _ } -> is_wildcard pattern)
    then (
      let instructions' =
        List.filter_map entries ~f:(fun { term_no; pattern; path } ->
            match pattern with
            | Pattern.Var (_, name) -> Some { term_no; name; path }
            | _ -> None)
      in
      Matched (instructions', rhs))
    else (
      let transpose =
        let rows, _rhss = List.unzip matrix in
        List.transpose_exn rows
      in
      let i =
        (* find first column which is not all wildcards *)
        let col_no =
          List.findi transpose ~f:(fun _ column ->
              not (List.for_all column ~f:(fun { pattern; _ } -> is_wildcard pattern)))
        in
        match col_no with None -> failwith "TODO: error 5" | Some (i, _) -> i
      in
      let non_wildcard_col = List.nth_exn transpose i in
      if Int.(i = 0)
      then (
        let head_ctors =
          non_wildcard_col
          |> List.filter_map ~f:(fun { pattern; _ } ->
                 match pattern with
                 | Pattern.Operator (_, name, _) -> Some name
                 | _ -> None)
          |> Util.String.Set.of_list
        in
        let head_sort =
          match sorts with [] -> failwith "TODO: error 6" | sort :: _ -> sort
        in
        let (AbstractSyntax.SortDef (_ty_vars, op_defs)) =
          Map.find_exn lang (sort_name head_sort)
        in
        (* is every constructor covered? *)
        let is_signature =
          List.for_all op_defs ~f:(fun (OperatorDef (name, _arity)) ->
              Set.mem head_ctors name)
        in
        let default_case =
          let matrix = default matrix in
          if is_signature then None else Some (compile_matrix lang sorts matrix)
        in
        let branches =
          head_ctors
          |> Set.to_list
          |> List.map ~f:(fun ctor_name ->
                 let sort =
                   match sorts with [] -> failwith "TODO 3" | sort :: _ -> sort
                 in
                 (* let arity = get_arity lang (sort_name head_sort) ctor_name in *)
                 let matrix, sorts = specialize lang sort sorts ctor_name matrix in
                 let decision_tree = compile_matrix lang sorts matrix in
                 ctor_name, decision_tree)
          |> SMap.of_alist_exn
        in
        OperatorCases (branches, default_case))
      else (
        let matrix = swap_cols matrix 0 i in
        let sorts = Util.List.swap sorts ~i ~j:0 in
        Swap (i, compile_matrix lang sorts matrix)))
;;

let compile_cases lang sort cases =
  Ok
    (compile_matrix
       lang
       [ sort ]
       (List.map cases ~f:(fun (pattern, rhs) ->
            [ { term_no = 0; pattern; path = [] } ], rhs)))
;;

let run_matches ~prim_pp ~prim_eq tms tree =
  let rec go tms' tree =
    match tms', tree with
    | _, Matched (instrs, rhs) ->
      let env_list =
        instrs
        |> List.map ~f:(fun { term_no; name; path } ->
               (* let tm = List.nth_exn tms 0 (1* XXX *1) in *)
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
        (*
        with
        | Ok env_list -> env_list
        | Unequal_lengths ->
          Util.invariant_violation
            (Printf.sprintf
               "mismatched instructions [%s] and terms (%s)"
               (instrs |> List.map ~f:string_of_instruction |> String.concat ~sep:", ")
               (tms
               |> List.map ~f:(NonBinding.pp prim_pp)
               |> String.concat ~sep:", "))
            *)
      in
      let env =
        match SMap.of_alist env_list with
        | `Ok env -> env
        | `Duplicate_key name ->
          Util.invariant_violation
            (Printf.sprintf
               "run_matches: duplicate key: %s (instrs: [%s], terms: (%s)"
               name
               (instrs |> List.map ~f:string_of_instruction |> String.concat ~sep:", ")
               (tms
               |> List.map ~f:(Fmt.to_to_string (NonBinding.pp prim_pp))
               |> String.concat ~sep:", "))
      in
      Some (rhs, env)
    | [], _ -> Util.invariant_violation "empty pattern but not matched"
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
      | None -> failwith "TODO: error -- no matching primitive"
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
    let result1 = simple_match ~prim_eq:Primitive.( = ) tm cases in
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
        (match simple_match ~prim_eq:Primitive.( = ) tm branches with
        | None -> Stdio.print_string "no match"
        | Some result -> Stdio.print_string (str_of_result result))
      | _ -> failwith "something failed to parse"
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
          match AbstractSyntax.unordered syntax with
          | `Ok syntax -> syntax
          | `Duplicate_key name -> failwith (Printf.sprintf "duplicate key: %s" name)
        in
        let decision_tree = compile_matrix syntax sorts matrix_rows in
        (match
           run_matches ~prim_pp:Primitive.pp ~prim_eq:Primitive.( = ) tms decision_tree
         with
        | None -> Stdio.print_string "no match"
        | Some result -> Stdio.print_string (str_of_result result))
    ;;

    let bool_lang = "bool := t() | f()"

    (* let list_lang = "list a := nil() | cons(a; list a)" *)
    let list_lang = {|
      list := nil() | cons(unit; list)
      unit := unit()
    |}

    let three_bool_match =
      {|| _, f(), t() -> 1
        | f(), t(), _ -> 2
        | _, _, f() -> 3
        | _, _, t() -> 4
      |}
    ;;

    let%expect_test _ =
      run_simple_match "_ -> 1" "foo()";
      [%expect "{} 1"]
    ;;

    let%expect_test _ =
      run_simple_match "bar() -> 1" "foo()";
      [%expect "no match"]
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
      (* run_compiled_matches list_lang [ "list unit"; "list unit" ] merge_match "nil()"; *)
      run_compiled_matches list_lang "list, list" merge_match tms
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
  end)
;;

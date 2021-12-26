open Base
open Lvca_provenance
open Lvca_util
module Format = Stdlib.Format

type t =
  | Operator of Provenance.t * string * t list
  | Primitive of Primitive_impl.All.t
  | Var of Provenance.t * string

let mk_Operator ?(provenance = Provenance.of_here [%here]) name pats =
  Operator (provenance, name, pats)
;;

let mk_Var ?(provenance = Provenance.of_here [%here]) name = Var (provenance, name)
let mk_Primitive prim = Primitive prim

let rec equivalent ?(info_eq = fun _ _ -> true) pat1 pat2 =
  match pat1, pat2 with
  | Operator (i1, name1, pats1), Operator (i2, name2, pats2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (equivalent ~info_eq) pats1 pats2
  | Primitive p1, Primitive p2 -> Primitive_impl.All.equivalent ~info_eq p1 p2
  | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let rec vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats |> List.map ~f:vars_of_pattern |> Set.union_list (module String)
  | Primitive _ -> String.Set.empty
  | Var (_, name) -> String.Set.of_list [ name ]
;;

let rec list_vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats
    (* simpler, morally equivalent: List.concat_map ~f:(List.concat_map
       ~f:list_vars_of_pattern) *)
    |> List.map ~f:list_vars_of_pattern
    |> List.concat_no_order
  | Primitive _ -> []
  | Var (loc, name) -> [ loc, name ]
;;

let info = function
  | Operator (loc, _, _) | Var (loc, _) -> loc
  | Primitive p -> Primitive_impl.All.info p
;;

let rec pp ppf pat =
  let list, pf, semi = Fmt.(list, pf, semi) in
  match pat with
  | Operator (info, name, pats) ->
    let pp' ppf () = pf ppf "@[<2>@{%s@}(%a)@]" name (list ~sep:semi pp) pats in
    Provenance.fmt_stag info pp' ppf ()
  | Var (info, name) -> Provenance.fmt_stag info Fmt.string ppf name
  | Primitive prim -> Primitive_impl.All.pp ppf prim
;;

let rec jsonify pat =
  Lvca_util.Json.(
    match pat with
    | Operator (_, tag, tms) ->
      array
        [| string "o"; string tag; tms |> List.map ~f:jsonify |> Array.of_list |> array |]
    | Primitive p -> array [| string "p"; Primitive_impl.All.jsonify p |]
    | Var (_, name) -> array [| string "v"; string name |])
;;

let rec unjsonify =
  let open Option.Let_syntax in
  Lvca_util.Json.(
    function
    | Array [| String "o"; String tag; Array subtms |] ->
      let%map subtms' = subtms |> Array.to_list |> List.map ~f:unjsonify |> Option.all in
      Operator (Provenance.of_here [%here], tag, subtms')
    | Array [| String "p"; prim |] ->
      let%map prim = Primitive_impl.All.unjsonify prim in
      Primitive prim
    | Array [| String "v"; String name |] -> Some (Var (Provenance.of_here [%here], name))
    | _ -> None)
;;

let rec select_path ~path pat =
  match path with
  | [] -> Ok pat
  | i :: path ->
    (match pat with
    | Primitive _ | Var _ ->
      Error
        (Fmt.str
           "select_path: hit a primitive / var (%a) with path remaining %a"
           pp
           pat
           Fmt.(list int)
           (i :: path))
    | Operator (_, _, pats) ->
      (match List.nth pats i with
      | Some pat -> select_path ~path pat
      | None ->
        Error
          (Fmt.str
             "select_path: out-of-bounds index (%i) (with path %a remaining) at %a"
             i
             Fmt.(list int)
             path
             pp
             pat)))
;;

let handle_dup_error = function
  | `Ok result -> Ok result
  | `Duplicate_key k ->
    Error
      (Check_failure.err
         (Printf.sprintf
            "Did you mean to bind the same variable (%s) twice in the same pattern? \
             That's not allowed!"
            k))
;;

let check lang ~pattern_sort ~var_sort =
  let lookup_operator = Abstract_syntax.lookup_operator lang in
  let rec check sort pat =
    let result =
      match pat with
      | Var (_, name) when Lvca_util.String.is_ignore name -> Ok String.Map.empty
      | Var (_, name) ->
        if Sort.equivalent sort var_sort
        then Ok (String.Map.singleton name sort)
        else
          Error
            (Check_failure.err
               (Fmt.str
                  "Pattern %s is of sort `%a`. Expected `%a`."
                  name
                  Sort.pp
                  var_sort
                  Sort.pp
                  sort))
      | Primitive prim ->
        (match Primitive_impl.All.check prim sort with
        | None -> Ok String.Map.empty
        | Some msg -> Error (Check_failure.err msg))
      | Operator (_, op_name, subpats) ->
        let sort_name, sort_args = Sort.split sort in
        (match lookup_operator sort_name op_name with
        | Error lookup_err ->
          Error
            (Check_failure.err
               (Fmt.str
                  "Pattern.check:@ failed to find operator %s in sort %s:@;<1 2>@[%a@]"
                  op_name
                  sort_name
                  Abstract_syntax.Lookup_error.pp
                  lookup_err))
        | Ok (sort_vars, Operator_def (_, _, arity)) ->
          (* TODO: kind check *)
          let sort_vars = List.map sort_vars ~f:Tuple2.get1 in
          let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
          check_slots (Arity.instantiate sort_env arity) subpats)
    in
    Result.map_error result ~f:(fun Check_failure.{ message; stack } ->
        Check_failure.{ message; stack = { term = pat; sort } :: stack })
  and check_slots (Arity (_, valences)) pats =
    match List.zip pats valences with
    | Unequal_lengths ->
      Error
        (Check_failure.err
           Fmt.(
             str
               "Wrong number of subterms (%u) for this arity (%a)"
               (List.length pats)
               (list ~sep:comma Valence.pp)
               valences))
    | Ok pat_valences ->
      pat_valences
      |> List.map ~f:(fun (pat, valence) ->
             match valence with
             (* The pattern is a single var of the given sort *)
             | Valence ([], sort) -> check sort pat
             | _ ->
               Error
                 (Check_failure.err
                    (Fmt.str
                       "Invalid pattern (%a) binding a non-sort valence (%a)"
                       pp
                       pat
                       Valence.pp
                       valence)))
      |> Result.all
      |> Result.map ~f:String.Map.strict_unions
      |> Result.bind ~f:handle_dup_error
  in
  check pattern_sort
;;

let parse reserved_word =
  let open Lvca_parsing in
  let open C_comment_parser in
  fix (fun pat ->
      choice
        ~failure_msg:"looking for a primitive or identifier (for a var or operator)"
        [ (Primitive_impl.All.parse >>| fun prim -> Primitive prim)
        ; (let%map range, ident = lower_identifier reserved_word in
           Var (Provenance.of_range range, ident))
        ; (let%bind range, ident = upper_identifier reserved_word in
           let%map range', children = parens (sep_end_by (char ';' <* whitespace) pat) in
           let range = Opt_range.union range range' in
           Operator (Provenance.of_range range, ident, children))
        ])
  <?> "pattern"
;;

let%test_module "Parsing" =
  (module struct
    let () =
      Format.set_formatter_stag_functions Provenance.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let print_parse tm =
      match Lvca_parsing.parse_string (parse String.Set.empty) tm with
      | Ok pat -> Fmt.pr "%a" pp pat
      | Error msg -> Fmt.pr "failed: %s\n" msg
    ;;

    let%expect_test _ =
      print_parse {|"str"|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,5} }>"str"</{ input = Input_unknown; range = {0,5} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A()|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,3} }>A()</{ input = Input_unknown; range = {0,3} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|x|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,1} }>x</{ input = Input_unknown; range = {0,1} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|_|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,1} }>_</{ input = Input_unknown; range = {0,1} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|_x|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,2} }>_x</{ input = Input_unknown; range = {0,2} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b)|};
      [%expect
        {|
      <{ input = Input_unknown; range = {0,4} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>)</{ input = Input_unknown; range = {0,4} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b;c)|};
      (*0123456*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,6} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>; <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>)</{ input = Input_unknown; range = {0,6} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b;c;)|};
      (*01234567*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,7} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>; <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>)</{ input = Input_unknown; range = {0,7} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b;c;d;e;)|};
      (*012345678901*)
      [%expect
        {|
      <{ input = Input_unknown; range = {0,11} }>A(<{ input = Input_unknown; range = {2,3} }>b</{ input = Input_unknown; range = {2,3} }>; <{ input = Input_unknown; range = {4,5} }>c</{ input = Input_unknown; range = {4,5} }>; <{ input = Input_unknown; range = {6,7} }>d</{ input = Input_unknown; range = {6,7} }>; <{ input = Input_unknown; range = {8,9} }>e</{ input = Input_unknown; range = {8,9} }>)</{ input = Input_unknown; range = {0,11} }>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|A(b;;c)|};
      [%expect
        {| failed: pattern: looking for a primitive or identifier (for a var or operator) |}]
    ;;
  end)
;;

module Properties = struct
  open Property_result

  let parse = Lvca_parsing.parse_string (parse String.Set.empty)
  let to_string = Fmt.to_to_string pp

  let json_round_trip1 t =
    match t |> jsonify |> unjsonify with
    | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
    | Some t' -> Property_result.check (t = t') (Fmt.str "%a <> %a" pp t' pp t)
  ;;

  let json_round_trip2 json =
    match json |> unjsonify with
    | None -> Uninteresting
    | Some t ->
      Property_result.check
        Lvca_util.Json.(jsonify t = json)
        (Fmt.str "jsonify %a <> json (TODO: print)" pp t)
  ;;

  let string_round_trip1 t =
    match t |> to_string |> parse with
    | Ok t' -> Property_result.check (t' = t) (Fmt.str "%a <> %a" pp t' pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = to_string t in
      if Base.String.(str' = str)
      then Ok
      else (
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = to_string t' in
          Property_result.check
            String.(str'' = str')
            (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end

let%test_module "check" =
  (module struct
    let parse_lang lang_str =
      Lvca_parsing.(parse_string (whitespace *> Abstract_syntax.parse) lang_str)
      |> Result.ok_or_failwith
    ;;

    let parse_pattern str =
      Lvca_parsing.parse_string (parse String.Set.empty) str |> Result.ok_or_failwith
    ;;

    let parse_sort str = Lvca_parsing.(parse_string (Sort.parse String.Set.empty) str)

    let lang_desc =
      {|
value :=
  | Unit()
  | Lit_int(integer)
  | Lit_str(string)
  | List(list value)
  ;

list a :=
  | Nil()
  | Cons(a; list a)
  ;

match_line :=
  | Match_line(value[value]. term)
  ;

term :=
  | Lambda(value. term)
  | Alt_lambda(term. term)
  | Match(match_line)
  | Value(value)
  ;

test := Foo(term[term]. term);
      |}
    ;;

    let language = parse_lang lang_desc

    let print_check_pattern ?var_sort_str sort_str pat_str =
      let sort = sort_str |> parse_sort |> Result.ok_or_failwith in
      let var_sort =
        match var_sort_str with
        | None -> sort
        | Some str -> str |> parse_sort |> Result.ok_or_failwith
      in
      let pat = parse_pattern pat_str in
      let pp ppf pat = Fmt.pf ppf "pattern: %a" pp pat in
      match check language ~pattern_sort:sort ~var_sort pat with
      | Error failure -> Fmt.epr "%a" (Check_failure.pp pp) failure
      | Ok _ -> ()
    ;;

    let%expect_test _ =
      print_check_pattern "value" "Unit()";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "Lit_int(1)";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "value" {|Lit_str("str")|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "list value" {|Nil()|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "list value" {|Cons(Unit(); Nil())|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern
        ~var_sort_str:"value"
        "term"
        {|Value(List(Cons(Unit(); Nil())))|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern ~var_sort_str:"value" "term" {|Value(List(Cons(a; Nil())))|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "term" {|Value(List(Cons(a; Nil())))|};
      [%expect
        {|
        Pattern a is of sort `term`. Expected `value`.
        stack:
        - pattern: Value(List(Cons(a; Nil()))), sort: term
        - pattern: List(Cons(a; Nil())), sort: value
        - pattern: Cons(a; Nil()), sort: list value
        - pattern: a, sort: value
      |}]
    ;;

    let%expect_test _ =
      print_check_pattern
        ~var_sort_str:"value"
        "term"
        {|Value(List(Cons(a; Cons(a; _))))|};
      [%expect
        {|
      Did you mean to bind the same variable (a) twice in the same pattern? That's not allowed!
      stack:
      - pattern: Value(List(Cons(a; Cons(a; _)))), sort: term
      - pattern: List(Cons(a; Cons(a; _))), sort: value
      - pattern: Cons(a; Cons(a; _)), sort: list value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "Lambda(a)";
      [%expect
        {|
        Invalid pattern (a) binding a non-sort valence (value. term)
        stack:
        - pattern: Lambda(a), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "match_line" "Match_line(a)";
      [%expect
        {|
        Invalid pattern (a) binding a non-sort valence (value[value]. term)
        stack:
        - pattern: Match_line(a), sort: match_line |}]
    ;;

    let%expect_test _ =
      print_check_pattern "integer" {|"foo"|};
      [%expect
        {|
      Unexpected sort (integer) for a primitive ("foo")
      stack:
      - pattern: "foo", sort: integer |}]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "Foo()";
      [%expect
        {|
      Pattern.check: failed to find operator Foo in sort value:
        operator not found (options: {List, Lit_int, Lit_str, Unit})
      stack:
      - pattern: Foo(), sort: value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "Unit(1)";
      [%expect
        {|
      Wrong number of subterms (1) for this arity ()
      stack:
      - pattern: Unit(1), sort: value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "Lambda(1)";
      [%expect
        {|
      Invalid pattern (1) binding a non-sort valence (value. term)
      stack:
      - pattern: Lambda(1), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "Match(a; b)";
      [%expect
        {|
      Wrong number of subterms (2) for this arity (match_line)
      stack:
      - pattern: Match(a; b), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "integer" "1";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "float" "1.";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "char" "'a'";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "string" {|"str"|};
      [%expect]
    ;;
  end)
;;

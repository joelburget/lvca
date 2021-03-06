module Unit = Base.Unit
module List = Base.List
module Option = Base.Option
module Queue = Base.Queue
module Set = Base.Set
module Util = Lvca_util
module String = Util.String
module SMap = Lvca_util.String.Map
module Result = Base.Result

type ('info, 'prim) pattern =
  | Operator of 'info * string * ('info, 'prim) pattern list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

type ('info, 'prim) t = ('info, 'prim) pattern

let rec equal info_eq prim_eq pat1 pat2 =
  match pat1, pat2 with
  | Operator (i1, name1, pats1), Operator (i2, name2, pats2) ->
    info_eq i1 i2
    && String.(name1 = name2)
    && List.equal (equal info_eq prim_eq) pats1 pats2
  | Primitive (i1, p1), Primitive (i2, p2) -> info_eq i1 i2 && prim_eq p1 p2
  | Var (i1, name1), Var (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | Ignored (i1, name1), Ignored (i2, name2) -> info_eq i1 i2 && String.(name1 = name2)
  | _, _ -> false
;;

let rec vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats |> List.map ~f:vars_of_pattern |> Set.union_list (module String)
  | Primitive _ -> String.Set.empty
  | Var (_, name) -> String.Set.of_list [ name ]
  | Ignored _ -> String.Set.empty
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
  | Ignored _ -> []
;;

let info = function
  | Operator (loc, _, _) | Primitive (loc, _) | Var (loc, _) | Ignored (loc, _) -> loc
;;

let rec pp pp_prim ppf =
  let list, pf, semi = Fmt.(list, pf, semi) in
  function
  | Operator (_, name, pats) ->
    pf ppf "@[<2>%s(%a)@]" name (pp pp_prim |> list ~sep:semi) pats
  | Primitive (_, prim) -> pp_prim ppf prim
  | Var (_, name) -> pf ppf "%s" name
  | Ignored (_, name) -> pf ppf "_%s" name
;;

let rec pp_range_generic ~opener ~closer pp_prim ppf pat =
  let list, pf, semi = Fmt.(list, pf, semi) in
  opener ppf (info pat);
  (match pat with
  | Operator (_, name, pats) ->
    pf
      ppf
      "@[<2>@{%s@}(%a)@]"
      name
      (pp_range_generic ~opener ~closer pp_prim |> list ~sep:semi)
      pats
  | Primitive (_, prim) -> pf ppf "%a" pp_prim prim
  | Var (_, name) -> pf ppf "%s" name
  | Ignored (_, name) -> pf ppf "_%s" name);
  closer ppf (info pat)
;;

let pp_range pp_prim ppf pat =
  pp_range_generic pp_prim ppf pat ~opener:OptRange.open_stag ~closer:OptRange.close_stag
;;

let pp_ranges pp_prim ppf pat =
  pp_range_generic
    pp_prim
    ppf
    pat
    ~opener:(fun ppf loc -> Format.pp_open_stag ppf (SourceRanges.Stag loc))
    ~closer:(fun ppf _loc -> Format.pp_close_stag ppf ())
;;

let rec jsonify prim_jsonify pat =
  Util.Json.(
    match pat with
    | Operator (_, tag, tms) ->
      array
        [| string "o"
         ; string tag
         ; tms |> List.map ~f:(jsonify prim_jsonify) |> Array.of_list |> array
        |]
    | Primitive (_, p) -> array [| string "p"; prim_jsonify p |]
    | Var (_, name) -> array [| string "v"; string name |]
    | Ignored (_, name) -> array [| string "_"; string name |])
;;

let rec unjsonify prim_unjsonify =
  let open Option.Let_syntax in
  Util.Json.(
    function
    | Array [| String "o"; String tag; Array subtms |] ->
      let%map subtms' =
        subtms |> Array.to_list |> List.map ~f:(unjsonify prim_unjsonify) |> Option.all
      in
      Operator ((), tag, subtms')
    | Array [| String "p"; prim |] ->
      let%map prim' = prim_unjsonify prim in
      Primitive ((), prim')
    | Array [| String "v"; String name |] -> Some (Var ((), name))
    | Array [| String "_"; String name |] -> Some (Ignored ((), name))
    | _ -> None)
;;

let rec map_info ~f = function
  | Operator (loc, tag, subpats) ->
    Operator (f loc, tag, subpats |> List.map ~f:(map_info ~f))
  | Primitive (loc, prim) -> Primitive (f loc, prim)
  | Var (loc, name) -> Var (f loc, name)
  | Ignored (loc, name) -> Ignored (f loc, name)
;;

let erase pat = map_info ~f:(fun _ -> ()) pat

let rec select_path ~path pat =
  match path with
  | [] -> Ok pat
  | i :: path ->
    (match pat with
    | Primitive _ | Var _ | Ignored _ -> Error "TODO: message"
    | Operator (_, _, pats) ->
      let pat = List.nth pats i in
      (match pat with Some pat -> select_path ~path pat | None -> Error "TODO: message"))
;;

let handle_dup_error = function
  | `Ok result -> Ok result
  | `Duplicate_key k ->
    Error
      (CheckFailure.err
         (Printf.sprintf
            "Did you mean to bind the same variable (%s) twice in the same pattern? \
             That's not allowed!"
            k))
;;

let check pp_prim check_prim lang ~pattern_sort ~var_sort =
  let lookup_operator = AbstractSyntax.lookup_operator lang in
  let rec check sort pat =
    let result =
      match pat with
      | Var (_, name) ->
        if Sort.equal Unit.( = ) (Sort.erase_info sort) (Sort.erase_info var_sort)
        then Ok (SMap.singleton name sort)
        else (
          let sort_to_string = Fmt.to_to_string Sort.pp in
          Error
            (CheckFailure.err
               (Printf.sprintf
                  "Pattern %s is of sort `%s`. Expected `%s`."
                  name
                  (sort_to_string var_sort)
                  (sort_to_string sort))))
      | Ignored _ -> Ok SMap.empty
      | Primitive (info, prim) ->
        (match check_prim info prim sort with
        | None -> Ok SMap.empty
        | Some msg -> Error (CheckFailure.err msg))
      | Operator (_, op_name, subpats) ->
        let sort_name, sort_args =
          match sort with
          | Sort.Name (_, sort_name) -> sort_name, []
          | Sort.Ap (_, sort_name, sort_args) -> sort_name, sort_args
        in
        (match lookup_operator sort_name op_name with
        | None ->
          Error
            (CheckFailure.err
               (Printf.sprintf
                  "Pattern.check: failed to find operator %s in sort %s"
                  op_name
                  sort_name))
        | Some (sort_vars, OperatorDef (_, arity)) ->
          let sort_env = SMap.of_alist_exn (List.zip_exn sort_vars sort_args) in
          check_slots (AbstractSyntax.instantiate_arity sort_env arity) subpats)
    in
    Result.map_error result ~f:(fun CheckFailure.{ message; stack } ->
        CheckFailure.{ message; stack = { term = pat; sort } :: stack })
  and check_slots valences pats =
    match List.zip pats valences with
    | Unequal_lengths ->
      Error
        (CheckFailure.err
           (Printf.sprintf
              "Wrong number of subterms (%u) for this arity (%s)"
              (List.length pats)
              (valences
              |> List.map ~f:AbstractSyntax.string_of_valence
              |> String.concat ~sep:", ")))
    | Ok pat_valences ->
      pat_valences
      |> List.map ~f:(fun (pat, valence) ->
             match valence with
             (* The pattern is a single var of the given sort *)
             | Valence ([], sort) -> check sort pat
             | _ ->
               Error
                 (CheckFailure.err
                    (Printf.sprintf
                       "Invalid pattern (%s) binding a non-sort valence (%s)"
                       (Fmt.to_to_string (pp pp_prim) pat)
                       (AbstractSyntax.string_of_valence valence))))
      |> Result.all
      |> Result.map ~f:SMap.strict_unions
      |> Result.bind ~f:handle_dup_error
  in
  check pattern_sort
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Primitive = Primitive.Parse (Comment)

  let t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t =
   fun parse_prim ->
    let open Parsers in
    fix (fun pat ->
        choice
          [ (parse_prim >>|| fun ~pos prim -> Primitive (pos, prim), pos)
          ; (identifier
            >>== fun ~pos ident ->
            if ident.[0] = '_'
            then return ~pos (Ignored (pos, String.subo ~pos:1 ident))
            else
              choice
                [ (parens (sep_end_by (char ';') pat)
                  >>|| fun ~pos:finish children ->
                  let pos = OptRange.union pos finish in
                  Operator (pos, ident, children), pos)
                ; return ~pos (Var (pos, ident))
                ]
              <?> "pattern body")
          ])
    <?> "pattern"
 ;;

  let whitespace_t prim = Parsers.(junk *> t prim)
end

let%test_module "Parsing" =
  (module struct
    module Parser = Parse (ParseUtil.NoComment)
    module PrimParser = Primitive.Parse (ParseUtil.NoComment)

    let () =
      Format.set_formatter_stag_functions Range.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let print_parse tm =
      match ParseUtil.parse_string (Parser.t PrimParser.t) tm with
      | Ok pat -> Fmt.pr "%a\n%a" (pp Primitive.pp) pat (pp_range Primitive.pp) pat
      | Error msg -> Fmt.pr "failed: %s\n" msg
    ;;

    let%expect_test _ =
      print_parse {|"str"|};
      [%expect {|
      "str"
      <{0,5}>"str"</{0,5}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      [%expect {|
      a()
      <{0,3}>a()</{0,3}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|x|};
      [%expect {|
      x
      <{0,1}>x</{0,1}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|_|};
      [%expect {|
      _
      <{0,1}>_</{0,1}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|_x|};
      [%expect {|
      _x
      <{0,2}>_x</{0,2}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      [%expect {|
      a(b)
      <{0,4}>a(<{2,3}>b</{2,3}>)</{0,4}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c)|};
      (*0123456*)
      [%expect
        {|
      a(b; c)
      <{0,6}>a(<{2,3}>b</{2,3}>; <{4,5}>c</{4,5}>)</{0,6}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;)|};
      (*01234567*)
      [%expect
        {|
      a(b; c)
      <{0,7}>a(<{2,3}>b</{2,3}>; <{4,5}>c</{4,5}>)</{0,7}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;d;e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b; c; d; e)
      <{0,11}>a(<{2,3}>b</{2,3}>; <{4,5}>c</{4,5}>; <{6,7}>d</{6,7}>; <{8,9}>e</{8,9}>)</{0,11}>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;;c)|};
      [%expect {| failed: : end_of_input |}]
    ;;
  end)
;;

module Properties = struct
  module ParsePattern = Parse (ParseUtil.NoComment)
  module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)
  open PropertyResult

  let parse = ParseUtil.parse_string (ParsePattern.t ParsePrimitive.t)
  let pp = pp Primitive.pp
  let to_string = Fmt.to_to_string pp

  let json_round_trip1 t =
    match t |> jsonify Primitive.jsonify |> unjsonify Primitive.unjsonify with
    | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
    | Some t' -> PropertyResult.check Caml.(t = t') (Fmt.str "%a <> %a" pp t' pp t)
  ;;

  let json_round_trip2 json =
    match json |> unjsonify Primitive.unjsonify with
    | None -> Uninteresting
    | Some t ->
      PropertyResult.check
        Lvca_util.Json.(jsonify Primitive.jsonify t = json)
        "jsonify t <> json (TODO: print)"
  ;;

  let string_round_trip1 t =
    match t |> to_string |> parse with
    | Ok t' ->
      let t'' = erase t' in
      PropertyResult.check Caml.(t'' = t) (Fmt.str "%a <> %a" pp t'' pp t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string t) msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = t |> erase |> to_string in
      if Base.String.(str' = str)
      then Ok
      else (
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = t' |> erase |> to_string in
          PropertyResult.check String.(str'' = str') (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end

module Primitive' = Primitive

module Primitive = struct
  let check lang ~pattern_sort ~var_sort =
    check Primitive.pp Primitive.check lang ~pattern_sort ~var_sort
  ;;
end

let%test_module "check" =
  (module struct
    module AbstractSyntaxParse = AbstractSyntax.Parse (ParseUtil.NoComment)

    let parse_lang lang_str =
      ParseUtil.parse_string AbstractSyntaxParse.whitespace_t lang_str
      |> Result.ok_or_failwith
    ;;

    module Parser = Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive'.Parse (ParseUtil.NoComment)

    let parse_pattern str =
      ParseUtil.parse_string (Parser.t ParsePrimitive.t) str |> Result.ok_or_failwith
    ;;

    module SortParse = Sort.Parse (ParseUtil.NoComment)

    let parse_sort str = ParseUtil.parse_string SortParse.t str

    let lang_desc =
      {|
value :=
  | unit()
  | lit_int(integer)
  | lit_str(string)
  | list(list value)

list a :=
  | nil()
  | cons(a; list a)

match_line :=
  | match_line(value[value]. term)

term :=
  | lambda(value. term)
  | alt_lambda(term. term)
  | match(match_line)
  | value(value)

test := foo(term[term]. term)
      |}
    ;;

    let language = parse_lang lang_desc

    let print_check_pattern ?var_sort_str sort_str pat_str =
      let sort = parse_sort sort_str |> Result.ok_or_failwith in
      let var_sort =
        match var_sort_str with
        | None -> sort
        | Some str -> parse_sort str |> Result.ok_or_failwith
      in
      let pat = parse_pattern pat_str in
      let pp ppf CheckFailure.{ term = pat; sort } =
        Fmt.pf ppf "- @[pattern: %a,@ sort: %a@]" (pp Primitive'.pp) pat Sort.pp sort
      in
      match Primitive.check language ~pattern_sort:sort ~var_sort pat with
      | Error failure -> Fmt.epr "%a" (CheckFailure.pp pp) failure
      | Ok _ -> ()
    ;;

    let%expect_test _ =
      print_check_pattern "value" "unit()";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "lit_int(1)";
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "value" {|lit_str("str")|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "list value" {|nil()|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "list value" {|cons(unit(); nil())|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern
        ~var_sort_str:"value"
        "term"
        {|value(list(cons(unit(); nil())))|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern ~var_sort_str:"value" "term" {|value(list(cons(a; nil())))|};
      [%expect]
    ;;

    let%expect_test _ =
      print_check_pattern "term" {|value(list(cons(a; nil())))|};
      [%expect
        {|
        Pattern a is of sort `term`. Expected `value`.
        stack:
        - pattern: value(list(cons(a; nil()))), sort: term
        - pattern: list(cons(a; nil())), sort: value
        - pattern: cons(a; nil()), sort: list value
        - pattern: a, sort: value
      |}]
    ;;

    let%expect_test _ =
      print_check_pattern
        ~var_sort_str:"value"
        "term"
        {|value(list(cons(a; cons(a; _))))|};
      [%expect
        {|
      Did you mean to bind the same variable (a) twice in the same pattern? That's not allowed!
      stack:
      - pattern: value(list(cons(a; cons(a; _)))), sort: term
      - pattern: list(cons(a; cons(a; _))), sort: value
      - pattern: cons(a; cons(a; _)), sort: list value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "lambda(a)";
      [%expect
        {|
        Invalid pattern (a) binding a non-sort valence (value. term)
        stack:
        - pattern: lambda(a), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "match_line" "match_line(a)";
      [%expect
        {|
        Invalid pattern (a) binding a non-sort valence (value[value]. term)
        stack:
        - pattern: match_line(a), sort: match_line |}]
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
      print_check_pattern "value" "foo()";
      [%expect
        {|
      Pattern.check: failed to find operator foo in sort value
      stack:
      - pattern: foo(), sort: value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "value" "unit(1)";
      [%expect
        {|
      Wrong number of subterms (1) for this arity ()
      stack:
      - pattern: unit(1), sort: value |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "lambda(1)";
      [%expect
        {|
      Invalid pattern (1) binding a non-sort valence (value. term)
      stack:
      - pattern: lambda(1), sort: term |}]
    ;;

    let%expect_test _ =
      print_check_pattern "term" "match(a; b)";
      [%expect
        {|
      Wrong number of subterms (2) for this arity (match_line)
      stack:
      - pattern: match(a; b), sort: term |}]
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

module Unit = Base.Unit
module List = Base.List
module Option = Base.Option
module Queue = Base.Queue
module Result = Base.Result
module Set = Base.Set
module Util = Lvca_util
module String = Util.String
module SMap = Util.String.Map
module Tuple2 = Util.Tuple2
open Lvca_provenance

type 'info t =
  | Operator of 'info * string * 'info t list
  | Primitive of 'info Primitive.t
  | Var of 'info * string
  | Ignored of 'info * string

module Plain = struct
  type t =
    | Operator of string * t list
    | Primitive of Primitive.Plain.t
    | Var of string
    | Ignored of string
end

let rec to_plain = function
  | Operator (_, name, pats) -> Plain.Operator (name, List.map ~f:to_plain pats)
  | Primitive prim -> Plain.Primitive (Primitive.to_plain prim)
  | Var (_, name) -> Plain.Var name
  | Ignored (_, name) -> Plain.Ignored name
;;

let rec of_plain = function
  | Plain.Operator (name, pats) -> Operator ((), name, List.map ~f:of_plain pats)
  | Primitive prim -> Primitive (Primitive.of_plain prim)
  | Var name -> Var ((), name)
  | Ignored name -> Ignored ((), name)
;;

let rec equal ~info_eq pat1 pat2 =
  match pat1, pat2 with
  | Operator (i1, name1, pats1), Operator (i2, name2, pats2) ->
    info_eq i1 i2 && String.(name1 = name2) && List.equal (equal ~info_eq) pats1 pats2
  | Primitive p1, Primitive p2 -> Primitive.equal ~info_eq p1 p2
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
  | Operator (loc, _, _) | Var (loc, _) | Ignored (loc, _) -> loc
  | Primitive p -> Primitive.info p
;;

let rec pp_generic ~open_loc ~close_loc ppf pat =
  let list, pf, semi = Fmt.(list, pf, semi) in
  open_loc ppf (info pat);
  (match pat with
  | Operator (_, name, pats) ->
    pf
      ppf
      "@[<2>@{%s@}(%a)@]"
      name
      (pp_generic ~open_loc ~close_loc |> list ~sep:semi)
      pats
  | Primitive prim ->
    Primitive.pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf prim
  | Var (_, name) -> Fmt.string ppf name
  | Ignored (_, name) -> pf ppf "_%s" name);
  close_loc ppf (info pat)
;;

let pp ppf = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf

let pp_range ppf pat =
  pp_generic ppf pat ~open_loc:OptRange.open_stag ~close_loc:OptRange.close_stag
;;

let pp_ranges ppf pat =
  pp_generic
    ppf
    pat
    ~open_loc:(fun ppf loc -> Format.pp_open_stag ppf (SourceRanges.Stag loc))
    ~close_loc:(fun ppf _loc -> Format.pp_close_stag ppf ())
;;

let rec jsonify pat =
  Util.Json.(
    match pat with
    | Operator (_, tag, tms) ->
      array
        [| string "o"; string tag; tms |> List.map ~f:jsonify |> Array.of_list |> array |]
    | Primitive p -> array [| string "p"; Primitive.jsonify p |]
    | Var (_, name) -> array [| string "v"; string name |]
    | Ignored (_, name) -> array [| string "_"; string name |])
;;

let rec unjsonify =
  let open Option.Let_syntax in
  Util.Json.(
    function
    | Array [| String "o"; String tag; Array subtms |] ->
      let%map subtms' = subtms |> Array.to_list |> List.map ~f:unjsonify |> Option.all in
      Operator ((), tag, subtms')
    | Array [| String "p"; prim |] ->
      let%map prim = Primitive.unjsonify prim in
      Primitive prim
    | Array [| String "v"; String name |] -> Some (Var ((), name))
    | Array [| String "_"; String name |] -> Some (Ignored ((), name))
    | _ -> None)
;;

let rec map_info ~f = function
  | Operator (loc, tag, subpats) ->
    Operator (f loc, tag, subpats |> List.map ~f:(map_info ~f))
  | Primitive prim -> Primitive (Primitive.map_info ~f prim)
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

let valence_to_string v = Fmt.to_to_string AbstractSyntax.Valence.pp v

let check lang ~pattern_sort ~var_sort =
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
      | Primitive prim ->
        (match Primitive.check prim sort with
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
          (* TODO: kind check *)
          let sort_vars = sort_vars |> List.map ~f:Tuple2.get1 in
          let sort_env = SMap.of_alist_exn (List.zip_exn sort_vars sort_args) in
          check_slots (AbstractSyntax.Arity.instantiate sort_env arity) subpats)
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
              (valences |> List.map ~f:valence_to_string |> String.concat ~sep:", ")))
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
                       (Fmt.to_to_string pp pat)
                       (valence_to_string valence))))
      |> Result.all
      |> Result.map ~f:SMap.strict_unions
      |> Result.bind ~f:handle_dup_error
  in
  check pattern_sort
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Prim = Primitive.Parse (Comment)

  let t : OptRange.t t ParseUtil.t =
    let open Parsers in
    fix (fun pat ->
        choice
          [ (Prim.t >>| fun prim -> Primitive prim)
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

  let whitespace_t = Parsers.(junk *> t)
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
      match ParseUtil.parse_string Parser.t tm with
      | Ok pat -> Fmt.pr "%a\n%a" pp pat pp_range pat
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
  open PropertyResult
  module ParsePattern = Parse (ParseUtil.NoComment)
  module Prim = Primitive.Parse (ParseUtil.NoComment)

  let parse = ParseUtil.parse_string ParsePattern.t
  let to_string = Fmt.to_to_string pp
  let ( = ) = equal ~info_eq:Unit.( = )

  let json_round_trip1 t =
    match t |> jsonify |> unjsonify with
    | None -> Failed (Fmt.str "Failed to unjsonify %a" pp t)
    | Some t' -> PropertyResult.check (t = t') (Fmt.str "%a <> %a" pp t' pp t)
  ;;

  let json_round_trip2 json =
    match json |> unjsonify with
    | None -> Uninteresting
    | Some t ->
      PropertyResult.check
        Lvca_util.Json.(jsonify t = json)
        (Fmt.str "jsonify %a <> json (TODO: print)" pp t)
  ;;

  let string_round_trip1 t =
    match t |> to_string |> parse with
    | Ok t' ->
      let t'' = erase t' in
      PropertyResult.check (t'' = t) (Fmt.str "%a <> %a" pp t'' pp t)
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

let%test_module "check" =
  (module struct
    module AbstractSyntaxParse = AbstractSyntax.Parse (ParseUtil.NoComment)

    let parse_lang lang_str =
      ParseUtil.parse_string AbstractSyntaxParse.whitespace_t lang_str
      |> Result.ok_or_failwith
    ;;

    module Parser = Parse (ParseUtil.NoComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)

    let parse_pattern str = ParseUtil.parse_string Parser.t str |> Result.ok_or_failwith

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
        Fmt.pf ppf "- @[pattern: %a,@ sort: %a@]" pp pat Sort.pp sort
      in
      match check language ~pattern_sort:sort ~var_sort pat with
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

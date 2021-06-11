open Base
open Lvca_provenance
open Lvca_util
module Format = Stdlib.Format

type 'info t =
  | Operator of 'info * string * 'info t list
  | Primitive of 'info Primitive_impl.t
  | Var of 'info * string
  | Ignored of 'info * string

module Plain = struct
  type t =
    | Operator of string * t list
    | Primitive of Primitive_impl.Plain.t
    | Var of string
    | Ignored of string
end

let rec to_plain = function
  | Operator (_, name, pats) -> Plain.Operator (name, List.map ~f:to_plain pats)
  | Primitive prim -> Plain.Primitive (Primitive_impl.to_plain prim)
  | Var (_, name) -> Plain.Var name
  | Ignored (_, name) -> Plain.Ignored name
;;

let rec of_plain = function
  | Plain.Operator (name, pats) -> Operator ((), name, List.map ~f:of_plain pats)
  | Primitive prim -> Primitive (Primitive_impl.of_plain prim)
  | Var name -> Var ((), name)
  | Ignored name -> Ignored ((), name)
;;

let rec equal ~info_eq pat1 pat2 =
  match pat1, pat2 with
  | Operator (i1, name1, pats1), Operator (i2, name2, pats2) ->
    info_eq i1 i2 && String.(name1 = name2) && List.equal (equal ~info_eq) pats1 pats2
  | Primitive p1, Primitive p2 -> Primitive_impl.equal ~info_eq p1 p2
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
  | Primitive p -> Primitive_impl.info p
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
    Primitive_impl.pp_generic
      ~open_loc:(fun _ _ -> ())
      ~close_loc:(fun _ _ -> ())
      ppf
      prim
  | Var (_, name) -> Fmt.string ppf name
  | Ignored (_, name) -> pf ppf "_%s" name);
  close_loc ppf (info pat)
;;

let pp ppf = pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf

let pp_range ppf pat =
  pp_generic ppf pat ~open_loc:Opt_range.open_stag ~close_loc:Opt_range.close_stag
;;

let pp_ranges ppf pat =
  pp_generic
    ppf
    pat
    ~open_loc:(fun ppf loc -> Format.pp_open_stag ppf (Source_ranges.Stag loc))
    ~close_loc:(fun ppf _loc -> Format.pp_close_stag ppf ())
;;

let rec jsonify pat =
  Lvca_util.Json.(
    match pat with
    | Operator (_, tag, tms) ->
      array
        [| string "o"; string tag; tms |> List.map ~f:jsonify |> Array.of_list |> array |]
    | Primitive p -> array [| string "p"; Primitive_impl.jsonify p |]
    | Var (_, name) -> array [| string "v"; string name |]
    | Ignored (_, name) -> array [| string "_"; string name |])
;;

let rec unjsonify =
  let open Option.Let_syntax in
  Lvca_util.Json.(
    function
    | Array [| String "o"; String tag; Array subtms |] ->
      let%map subtms' = subtms |> Array.to_list |> List.map ~f:unjsonify |> Option.all in
      Operator ((), tag, subtms')
    | Array [| String "p"; prim |] ->
      let%map prim = Primitive_impl.unjsonify prim in
      Primitive prim
    | Array [| String "v"; String name |] -> Some (Var ((), name))
    | Array [| String "_"; String name |] -> Some (Ignored ((), name))
    | _ -> None)
;;

let rec map_info ~f = function
  | Operator (loc, tag, subpats) ->
    Operator (f loc, tag, subpats |> List.map ~f:(map_info ~f))
  | Primitive prim -> Primitive (Primitive_impl.map_info ~f prim)
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
      | Var (_, name) ->
        if Sort.equal Unit.( = ) (Sort.erase_info sort) (Sort.erase_info var_sort)
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
      | Ignored _ -> Ok String.Map.empty
      | Primitive prim ->
        (match Primitive_impl.check prim sort with
        | None -> Ok String.Map.empty
        | Some msg -> Error (Check_failure.err msg))
      | Operator (_, op_name, subpats) ->
        let sort_name, sort_args =
          match sort with
          | Sort.Name (_, sort_name) -> sort_name, []
          | Sort.Ap (_, sort_name, sort_args) -> sort_name, sort_args
        in
        (match lookup_operator sort_name op_name with
        | None ->
          Error
            (Check_failure.err
               (Printf.sprintf
                  "Pattern.check: failed to find operator %s in sort %s"
                  op_name
                  sort_name))
        | Some (sort_vars, Operator_def (_, arity)) ->
          (* TODO: kind check *)
          let sort_vars = sort_vars |> List.map ~f:Tuple2.get1 in
          let sort_env = String.Map.of_alist_exn (List.zip_exn sort_vars sort_args) in
          check_slots (Abstract_syntax.Arity.instantiate sort_env arity) subpats)
    in
    Result.map_error result ~f:(fun Check_failure.{ message; stack } ->
        Check_failure.{ message; stack = { term = pat; sort } :: stack })
  and check_slots valences pats =
    match List.zip pats valences with
    | Unequal_lengths ->
      Error
        (Check_failure.err
           Fmt.(
             str
               "Wrong number of subterms (%u) for this arity (%a)"
               (List.length pats)
               (list ~sep:comma Abstract_syntax.Valence.pp)
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
                       Abstract_syntax.Valence.pp
                       valence)))
      |> Result.all
      |> Result.map ~f:String.Map.strict_unions
      |> Result.bind ~f:handle_dup_error
  in
  check pattern_sort
;;

let mk_var range ident =
  if Char.(ident.[0] = '_')
  then Ignored (range, String.subo ident ~pos:1)
  else Var (range, ident)
;;

module Parse = struct
  open Lvca_parsing

  let t =
    fix (fun pat ->
        choice
          [ (Primitive_impl.Parse.t >>| fun prim -> Primitive prim)
          ; (identifier
            >>== fun { value = ident; range } ->
            choice
              [ (parens (sep_end_by (char ';') pat)
                >>|| fun Parse_result.{ value = children; range = finish } ->
                let range = Opt_range.union range finish in
                { value = Operator (range, ident, children); range })
              ; return ~range (mk_var range ident)
              ]
            <?> "pattern body")
          ])
    <?> "pattern"
  ;;

  let whitespace_t = Lvca_parsing.(whitespace *> t)
end

let%test_module "Parsing" =
  (module struct
    let () =
      Format.set_formatter_stag_functions Range.stag_functions;
      Format.set_tags true;
      Format.set_mark_tags true
    ;;

    let print_parse tm =
      match Lvca_parsing.parse_string Parse.t tm with
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
  open Property_result

  let parse = Lvca_parsing.parse_string Parse.t
  let to_string = Fmt.to_to_string pp
  let ( = ) = equal ~info_eq:Unit.( = )

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
    | Ok t' ->
      let t'' = erase t' in
      Property_result.check (t'' = t) (Fmt.str "%a <> %a" pp t'' pp t)
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
          Property_result.check
            String.(str'' = str')
            (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end

let%test_module "check" =
  (module struct
    let parse_lang lang_str =
      Lvca_parsing.parse_string Abstract_syntax.Parse.whitespace_t lang_str
      |> Result.ok_or_failwith
    ;;

    let parse_pattern str = Lvca_parsing.parse_string Parse.t str |> Result.ok_or_failwith
    let parse_sort str = Lvca_parsing.parse_string Sort.Parse.t str

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
      let pp ppf pat = Fmt.pf ppf "pattern: %a" pp pat in
      match check language ~pattern_sort:sort ~var_sort pat with
      | Error failure -> Fmt.epr "%a" (Check_failure.pp pp) failure
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

module List = Base.List
module Option = Base.Option
module Queue = Base.Queue
module Set = Base.Set
module Util = Lvca_util
module String = Util.String

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

let location = function
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
  opener ppf (location pat);
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
  closer ppf (location pat)
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

let to_string pp_prim pat = Fmt.str "%a" (pp pp_prim) pat

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

let rec map_loc : f:('a -> 'b) -> ('a, 'prim) pattern -> ('b, 'prim) pattern =
 fun ~f -> function
  | Operator (loc, tag, subpats) ->
    Operator (f loc, tag, subpats |> List.map ~f:(map_loc ~f))
  | Primitive (loc, prim) -> Primitive (f loc, prim)
  | Var (loc, name) -> Var (f loc, name)
  | Ignored (loc, name) -> Ignored (f loc, name)
;;

let erase pat = map_loc ~f:(fun _ -> ()) pat

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

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Primitive = Primitive.Parse (Comment)

  type 'prim pat_or_sep =
    | Pat of (OptRange.t, 'prim) pattern
    | Semi

  let t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t =
   fun parse_prim ->
    let open Parsers in
    fix (fun pat ->
        let t_or_sep : 'prim pat_or_sep Parsers.t =
          choice [ (fun _ -> Semi) <$> choice [ char ';' ]; (fun pat -> Pat pat) <$> pat ]
        in
        let accumulate
            :  OptRange.t -> string -> 'prim pat_or_sep list
            -> (OptRange.t, 'prim) pattern Parsers.t
          =
         fun range tag tokens ->
          (* patterns encountered between ';'s *)
          let slot_queue : (OptRange.t, 'prim) pattern Queue.t = Queue.create () in
          (* Move the current list to the slot queue *)
          let rec go = function
            | [] -> return ~pos:range (Operator (range, tag, Queue.to_list slot_queue))
            | Pat pat :: Semi :: rest (* Note: allow trailing ';' *)
            | Pat pat :: ([] as rest) ->
              Queue.enqueue slot_queue pat;
              go rest
            | _ -> fail "Malformed term"
          in
          go tokens
        in
        choice
          [ (parse_prim >>|| fun ~pos prim -> Primitive (pos, prim), pos)
          ; (identifier
            >>== fun ~pos:rng ident ->
            if ident.[0] = '_'
            then return ~pos:rng (Ignored (rng, String.subo ~pos:1 ident))
            else
              choice
                [ (parens (many t_or_sep)
                  >>= fun tokens ->
                  pos
                  >>= fun finish ->
                  accumulate (OptRange.extend_to rng finish) ident tokens)
                ; return ~pos:rng (Var (rng, ident))
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
      <0-5>"str"</0-5>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      [%expect {|
      a()
      <0-3>a()</0-3>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      [%expect {|
      a(b)
      <0-4>a(<2-3>b</2-3>)</0-4>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c)|};
      (*0123456*)
      [%expect {|
      a(b; c)
      <0-6>a(<2-3>b</2-3>; <4-5>c</4-5>)</0-6>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;)|};
      (*01234567*)
      [%expect {|
      a(b; c)
      <0-7>a(<2-3>b</2-3>; <4-5>c</4-5>)</0-7>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b;c;d;e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b; c; d; e)
      <0-11>a(<2-3>b</2-3>; <4-5>c</4-5>; <6-7>d</6-7>; <8-9>e</8-9>)</0-11>
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
  let pp' = pp Primitive.pp
  let to_string' = to_string Primitive.pp

  let json_round_trip1 t =
    match t |> jsonify Primitive.jsonify |> unjsonify Primitive.unjsonify with
    | None -> Failed (Fmt.str "Failed to unjsonify %a" pp' t)
    | Some t' -> PropertyResult.check Caml.(t = t') (Fmt.str "%a <> %a" pp' t' pp' t)
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
    match t |> to_string' |> parse with
    | Ok t' ->
      let t'' = erase t' in
      PropertyResult.check Caml.(t'' = t) (Fmt.str "%a <> %a" pp' t'' pp' t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (to_string' t) msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = t |> erase |> to_string' in
      if Base.String.(str' = str)
      then Ok
      else (
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = t' |> erase |> to_string' in
          PropertyResult.check String.(str'' = str') (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end

module List = Base.List
module Option = Base.Option
module Queue = Base.Queue
module Set = Base.Set
module Util = Lvca_util
module String = Util.String

type ('info, 'prim) pattern =
  | Operator of 'info * string * ('info, 'prim) pattern list list
  | Primitive of 'info * 'prim
  | Var of 'info * string
  | Ignored of 'info * string

type ('info, 'prim) t = ('info, 'prim) pattern

let rec vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats |> List.map ~f:vars_of_patterns |> Set.union_list (module String)
  | Primitive _ -> String.Set.empty
  | Var (_, name) -> String.Set.of_list [ name ]
  | Ignored _ -> String.Set.empty

and vars_of_patterns pats =
  pats
  |> List.map ~f:vars_of_pattern
  |> List.fold_right ~init:String.Set.empty ~f:Set.union
;;

let rec list_vars_of_pattern = function
  | Operator (_, _, pats) ->
    pats
    (* simpler, morally equivalent: List.concat_map ~f:(List.concat_map
       ~f:list_vars_of_pattern) *)
    |> List.map ~f:(fun pats' ->
           pats' |> List.map ~f:list_vars_of_pattern |> List.concat_no_order)
    |> List.concat_no_order
  | Primitive _ -> []
  | Var (loc, name) -> [ loc, name ]
  | Ignored _ -> []
;;

let location = function
  | Operator (loc, _, _) | Primitive (loc, _) | Var (loc, _) | Ignored (loc, _) -> loc
;;

let rec pp pp_prim ppf =
  let comma, list, pf, semi = Fmt.(comma, list, pf, semi) in
  function
  | Operator (_, name, pats) ->
    pf ppf "@[<2>%s(%a)@]" name (list ~sep:semi (list ~sep:comma (pp pp_prim))) pats
  | Primitive (_, prim) -> pp_prim ppf prim
  | Var (_, name) -> pf ppf "%s" name
  | Ignored (_, name) -> pf ppf "_%s" name
;;

let rec pp_range pp_prim ppf pat =
  let comma, list, pf, semi = Fmt.(comma, list, pf, semi) in
  OptRange.open_stag ppf (location pat);
  (match pat with
  | Operator (_, name, pats) ->
    pf
      ppf
      "@[<2>@{<test>%s@}(%a)@]"
      name
      (list ~sep:semi (list ~sep:comma (pp_range pp_prim)))
      pats
  | Primitive (_, prim) -> pf ppf "%a" pp_prim prim
  | Var (_, name) -> pf ppf "%s" name
  | Ignored (_, name) -> pf ppf "_%s" name);
  OptRange.close_stag ppf (location pat)
;;

let to_string pp_prim pat = Fmt.str "%a" (pp pp_prim) pat

let rec jsonify prim_jsonify pat =
  Util.Json.(
    match pat with
    | Operator (_, tag, tms) ->
      array
        [| string "o"
         ; string tag
         ; tms
           |> List.map ~f:(fun tms' ->
                  tms' |> List.map ~f:(jsonify prim_jsonify) |> Array.of_list |> array)
           |> Array.of_list
           |> array
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
        subtms
        |> Array.to_list
        |> List.map ~f:(function
               | Array subtms' ->
                 subtms'
                 |> Array.to_list
                 |> List.map ~f:(unjsonify prim_unjsonify)
                 |> Option.all
               | _ -> None)
        |> Option.all
      in
      Operator ((), tag, subtms')
    | Array [| String "p"; prim |] ->
      let%map prim' = prim_unjsonify prim in
      Primitive ((), prim')
    | Array [| String "v"; String name |] -> Some (Var ((), name))
    | Array [| String "_"; String name |] -> Some (Ignored ((), name))
    | _ -> None)
;;

let rec erase = function
  | Operator (_, tag, subpats) ->
    Operator ((), tag, subpats |> List.map ~f:(List.map ~f:erase))
  | Primitive (_, prim) -> Primitive ((), prim)
  | Var (_, name) -> Var ((), name)
  | Ignored (_, name) -> Ignored ((), name)
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Primitive = Primitive.Parse (Comment)

  type 'prim pat_or_sep =
    | Pat of (OptRange.t, 'prim) pattern
    | Sep of char

  let t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t =
   fun parse_prim ->
    let open Parsers in
    fix (fun pat ->
        let t_or_sep : 'prim pat_or_sep Parsers.t =
          choice
            [ (fun c -> Sep c) <$> choice [ char ','; char ';' ]
            ; (fun pat -> Pat pat) <$> pat
            ]
        in
        let accumulate
            :  OptRange.t -> string -> 'prim pat_or_sep list
            -> (OptRange.t, 'prim) pattern Parsers.t
          =
         fun range tag tokens ->
          (* patterns encountered between ','s, before hitting ';' *)
          let list_queue : (OptRange.t, 'prim) pattern Queue.t = Queue.create () in
          (* patterns encountered between ';'s *)
          let slot_queue : (OptRange.t, 'prim) pattern list Queue.t = Queue.create () in
          (* Move the current list to the slot queue *)
          let list_to_slot () =
            if not (Queue.is_empty list_queue)
            then list_queue |> Queue.to_list |> Queue.enqueue slot_queue;
            Queue.clear list_queue
          in
          let rec go = function
            | [] ->
              list_to_slot ();
              return ~pos:range (Operator (range, tag, Queue.to_list slot_queue))
            | Pat pat :: Sep ',' :: Sep ';' :: rest
            | Pat pat :: Sep ',' :: rest (* Note: allow trailing ',' *) ->
              Queue.enqueue list_queue pat;
              go rest
            | Pat pat :: Sep ';' :: rest (* Note: allow trailing ';' *)
            | Pat pat :: ([] as rest) ->
              Queue.enqueue list_queue pat;
              list_to_slot ();
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
      print_parse {|a(b,c)|};
      (*0123456*)
      [%expect {|
      a(b, c)
      <0-6>a(<2-3>b</2-3>, <4-5>c</4-5>)</0-6>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b,c,)|};
      (*01234567*)
      [%expect {|
      a(b, c)
      <0-7>a(<2-3>b</2-3>, <4-5>c</4-5>)</0-7>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b,c;d,e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b, c; d, e)
      <0-11>a(<2-3>b</2-3>, <4-5>c</4-5>; <6-7>d</6-7>, <8-9>e</8-9>)</0-11>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b,,c)|};
      [%expect {| failed: : end_of_input |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b,;c)|};
      (*01234567*)
      [%expect {|
      a(b, c)
      <0-7>a(<2-3>b</2-3>, <5-6>c</5-6>)</0-7>
    |}]
    ;;
  end)
;;

module Properties = struct
  let json_round_trip1 : (unit, Primitive.t) t -> bool =
   fun t ->
    match t |> jsonify Primitive.jsonify |> unjsonify Primitive.unjsonify with
    | None -> false
    | Some t' -> t = t'
 ;;

  let json_round_trip2 : Util.Json.t -> bool =
   fun json ->
    match json |> unjsonify Primitive.unjsonify with
    | None -> true (* malformed input *)
    | Some t -> Util.Json.(jsonify Primitive.jsonify t = json)
 ;;

  module Parse' = Parse (ParseUtil.NoComment)
  module ParsePrimitive = Primitive.Parse (ParseUtil.NoComment)

  let string_round_trip1 : (unit, Primitive.t) t -> bool =
   fun t ->
    match
      t |> to_string Primitive.pp |> ParseUtil.parse_string (Parse'.t ParsePrimitive.t)
    with
    | Ok prim -> erase prim = t
    | Error _ -> false
 ;;

  let string_round_trip2 : string -> bool =
   fun str ->
    match ParseUtil.parse_string (Parse'.t ParsePrimitive.t) str with
    | Ok prim ->
      let str' = to_string Primitive.pp prim in
      Base.String.(str' = str)
    | Error _ -> true
 ;;

  (* malformed input *)
end

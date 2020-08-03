module List = Base.List
module Option = Base.Option
module Queue = Base.Queue
module Set = Base.Set
module Util = Lvca_util
module String = Util.String

type 'a pattern =
  | Operator of 'a * string * 'a pattern list list
  | Primitive of 'a * Primitive.t
  | Var of 'a * string
  | Ignored of 'a * string

type 'a t = 'a pattern

let rec vars_of_pattern : 'a pattern -> String.Set.t = function
  | Operator (_, _, pats) -> pats
    |> List.map ~f:vars_of_patterns
    |> Set.union_list (module String)
  | Primitive _ -> String.Set.empty
  | Var (_, name) -> String.Set.of_list [ name ]
  | Ignored _ -> String.Set.empty

and vars_of_patterns pats =
  pats
  |> List.map ~f:vars_of_pattern
  |> List.fold_right ~init:String.Set.empty ~f:Set.union
;;

let rec list_vars_of_pattern = function
  | Operator (_, _, pats) -> pats
    (* simpler, morally equivalent:
      List.concat_map ~f:(List.concat_map ~f:list_vars_of_pattern)
    *)
    |> List.map ~f:(fun pats' -> pats'
      |> List.map ~f:list_vars_of_pattern
      |> List.concat_no_order)
    |> List.concat_no_order
  | Primitive _ -> []
  | Var (loc, name) -> [ loc, name ]
  | Ignored _ -> []
;;

let rec to_string : 'a pattern -> string = function
  | Operator (_, name, pats) ->
    Printf.sprintf "%s(%s)" name
    (pats
      |> List.map ~f:(fun pats' -> pats'
        |> List.map ~f:to_string
        |> String.concat ~sep:", ")
      |> String.concat ~sep:"; ")
  | Primitive (_, prim) -> Primitive.to_string prim
  | Var (_, name) -> name
  | Ignored (_, name) -> "_" ^ name
;;

let rec pp : Format.formatter -> 'a pattern -> unit
  = fun ppf ->
  let comma, list, pf, semi = Fmt.(comma, list, pf, semi) in
  function
    | Operator (_, name, pats)
    -> pf ppf "@[<2>%s(%a)@]"
      name
      (list ~sep:semi (list ~sep:comma pp)) pats
    | Primitive (_, prim)
    -> Primitive.pp ppf prim
    | Var (_, name)
    -> pf ppf "%s" name
    | Ignored (_, name)
    -> pf ppf "_%s" name

let rec jsonify pat =
  Util.Json.(
    match pat with
    | Operator (_, tag, tms) -> array [|
      string "o";
      string tag;
      tms
        |> List.map ~f:(fun tms' -> tms'
          |> List.map ~f:jsonify
          |> Array.of_list
          |> array)
        |> Array.of_list
        |> array
    |]
    | Primitive (_, p) -> array [| string "p"; Primitive.jsonify p |]
    | Var (_, name) -> array [| string "v"; string name |]
    | Ignored (_, name) -> array [| string "_"; string name |])

let rec unjsonify =
  let open Option.Let_syntax in
  Util.Json.(function
  | Array [| String "o"; String tag; Array subtms |] ->
    let%map subtms' = subtms
      |> Array.to_list
      |> List.map ~f:(function
        | Array subtms' -> subtms'
          |> Array.to_list
          |> List.map ~f:unjsonify
          |> Option.all
        | _ -> None)
      |> Option.all
    in
    Operator ((), tag, subtms')
  | Array [| String "p"; prim |] ->
    let%map prim' = Primitive.unjsonify prim in
    Primitive ((), prim')
  | Array [| String "v"; String name |]
  -> Some (Var ((), name))
  | Array [| String "_"; String name |]
  -> Some (Ignored ((), name))
  | _ -> None
  )

let rec erase = function
  | Operator (_, tag, subpats)
  -> Operator ((), tag, subpats |> List.map ~f:(List.map ~f:erase))
  | Primitive (_, prim) -> Primitive ((), prim)
  | Var (_, name) -> Var ((), name)
  | Ignored (_, name) -> Ignored ((), name)

let location = function
  | Operator (loc, _, _)
  | Primitive (loc, _)
  | Var (loc, _)
  | Ignored (loc, _)
  -> loc

module Parse (Comment : Util.Angstrom.Comment_int) = struct
  module Parsers = Util.Angstrom.Mk(Comment)
  module Primitive = Primitive.Parse(Comment)

  type pat_or_sep =
    | Pat of Range.t pattern
    | Sep of char

  let t : Range.t t Angstrom.t
    = let open Angstrom in
      let char, identifier, parens = Parsers.(char, identifier, parens) in

      fix (fun pat ->
        let t_or_sep : pat_or_sep Angstrom.t
          = choice
          [ (fun c -> Sep c) <$> (choice [char ','; char ';'])
          ; (fun pat -> Pat pat) <$> pat
          ]
        in

        let accumulate : string -> pat_or_sep list -> Range.t pattern Angstrom.t
          = fun tag tokens ->
            (* patterns encountered between ','s, before hitting ';' *)
            let list_queue : Range.t pattern Queue.t = Queue.create () in
            (* patterns encountered between ';'s *)
            let slot_queue : Range.t pattern list Queue.t = Queue.create () in

            (* Move the current list to the slot queue *)
            let list_to_slot () =
              if not (Queue.is_empty list_queue)
              then
                list_queue
                  |> Queue.to_list
                  |> Queue.enqueue slot_queue;
                Queue.clear list_queue
            in

            let rec go = function
              | []
              -> list_to_slot ();
                 return (Operator (Range.mk 0 0 (* TODO *), tag, Queue.to_list slot_queue))
              | Pat pat :: Sep ',' :: Sep ';' :: rest
              | Pat pat :: Sep ',' :: rest (* Note: allow trailing ',' *)
              -> Queue.enqueue list_queue pat;
                 go rest
              | Pat pat :: Sep ';' :: rest (* Note: allow trailing ';' *)
              | Pat pat :: ([] as rest)
              -> Queue.enqueue list_queue pat;
                 list_to_slot ();
                 go rest

              | _ -> fail "Malformed term"
            in

            go tokens
        in

        choice
          [ Primitive.t >>| (fun prim -> Primitive (Range.mk 0 0 (* TODO *), prim))
          ; identifier >>= fun ident ->
            if String.get ident 0 = '_'
            then return (Ignored (Range.mk 0 0 (* TODO *), String.subo ~pos:1 ident))
            else
              choice
              [ parens (many t_or_sep) >>= accumulate ident
              ; return (Var (Range.mk 0 0 (* TODO *), ident))
              ] <?> "pattern body"
          ]
      ) <?> "pattern"
end

let%test_module "Parsing" = (module struct
  module Parser = Parse(Util.Angstrom.NoComment)

  let print_parse = fun str -> match Angstrom.parse_string ~consume:All Parser.t str with
    | Error msg -> Caml.print_string ("failed: " ^ msg)
    | Ok pat -> Caml.print_string (to_string pat)
  ;;

  let%expect_test _ =
    print_parse {|"str"|};
    [%expect{| "str" |}]
  let%expect_test _ =
    print_parse {|a()|};
    [%expect{| a() |}]
  let%expect_test _ =
    print_parse {|a(b)|};
    [%expect{| a(b) |}]
  let%expect_test _ =
    print_parse {|a(b,c)|};
    [%expect{| a(b, c) |}]
  let%expect_test _ =
    print_parse {|a(b,c,)|};
    [%expect{| a(b, c) |}]
  let%expect_test _ =
    print_parse {|a(b,c;d,e;)|};
    [%expect{| a(b, c; d, e) |}]
  let%expect_test _ =
    print_parse {|a(b,,c)|};
    [%expect{| failed: : end_of_input |}]
  let%expect_test _ =
    print_parse {|a(b,;c)|};
    [%expect{| a(b, c) |}]
end);;

module Properties = struct
  let json_round_trip1 : unit t -> bool
    = fun t -> match t |> jsonify |> unjsonify with
      | None -> false
      | Some t' -> t = t'

  let json_round_trip2 : Util.Json.t -> bool
    = fun json -> match json |> unjsonify with
      | None -> true (* malformed input *)
      | Some t -> Util.Json.(jsonify t = json)

  module Parse' = Parse(Util.Angstrom.NoComment)

  let string_round_trip1 : unit t -> bool
    = fun t -> match t |> to_string |> Angstrom.parse_string ~consume:All Parse'.t with
      | Ok prim -> erase prim = t
      | Error _ -> false

  let string_round_trip2 : string -> bool
    = fun str -> match Angstrom.parse_string ~consume:All Parse'.t str with
      | Ok prim -> let str' = to_string prim in Base.String.(str' = str)
      | Error _ -> true (* malformed input *)
end

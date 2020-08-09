open Base
module Format = Caml.Format
module Printf = Caml.Printf
module Util = Lvca_util
module Cbor = Util.Cbor
module Json = Util.Json
module String = Util.String

module rec DeBruijn : sig
  type 'loc term =
    | Operator of 'loc * string * 'loc scope list
    | Var of 'loc * int * int
    | Primitive of 'loc * Primitive.t

  and 'loc scope = Scope of 'loc Pattern.t list * 'loc term list

  val to_nominal : 'loc term -> 'loc Nominal.term option
  val from_nominal : 'loc Nominal.term -> ('loc term, string) Result.t

  val from_nominal_with_bindings
    :  (int * int) String.Map.t
    -> 'loc Nominal.term
    -> ('loc term, string) Result.t

  val alpha_equivalent : 'loc term -> 'b term -> bool

  (* val open_scope : scope -> term list -> (term, string) Result.t *)
end = struct
  type 'loc term =
    | Operator of 'loc * string * 'loc scope list
    | Var of 'loc * int * int
    | Primitive of 'loc * Primitive.t

  and 'loc scope = Scope of 'loc Pattern.t list * 'loc term list

  let rec to_nominal' ctx = function
    | Var (loc, ix1, ix2) ->
      List.nth ctx ix1
      |> Option.bind ~f:(Fn.flip List.nth ix2)
      |> Option.map ~f:(fun name -> Nominal.Var (loc, name))
    | Operator (loc, tag, subtms) ->
      subtms
      |> List.map ~f:(scope_to_nominal ctx)
      |> Option.all
      |> Option.map ~f:(fun subtms' -> Nominal.Operator (loc, tag, subtms'))
    | Primitive (loc, prim) -> Some (Nominal.Primitive (loc, prim))

  and scope_to_nominal ctx (Scope (binders, body)) =
    let ctx' = binders
      |> List.map ~f:(fun pat -> pat
        |> Pattern.list_vars_of_pattern
        |> List.map ~f:snd)
      |> List.append ctx
    in
    body
      |> List.map ~f:(to_nominal' ctx')
      |> Option.all
      |> Option.map ~f:(fun body' -> Nominal.Scope (binders, body'))
  ;;

  let to_nominal tm = to_nominal' [] tm

  exception FailedFromNominal of string

  let rec from_nominal_with_bindings' env = function
    | Nominal.Operator (loc, tag, subtms) ->
      Operator (loc, tag, List.map subtms ~f:(scope_from_nominal' env))
    | Var (loc, name) ->
      (match Map.find env name with
      | None -> raise (FailedFromNominal ("couldn't find variable " ^ name))
      | Some (i, j) -> Var (loc, i, j))
    | Primitive (loc, prim) -> Primitive (loc, prim)

  and scope_from_nominal' env (Nominal.Scope (pats, body)) =
    let n = List.length pats in
    let var_nums : (string * (int * int)) list =
      pats
      |> List.mapi ~f:(fun i pat -> pat
        |> Pattern.list_vars_of_pattern
        |> List.mapi ~f:(fun j (_, var) -> var, (i, j)))
      |> List.join
    in
    match String.Map.of_alist var_nums with
    | `Ok var_map ->
      let env' : (int * int) String.Map.t = env
        |> Map.map ~f:(fun (i, j) -> i + n, j)
        |> Util.Map.union_right_biased var_map
      in
      let body' = List.map body ~f:(from_nominal_with_bindings' env') in
      Scope (pats, body')
    | `Duplicate_key _key -> failwith "TODO: raise error"
  ;;

  let from_nominal_with_bindings bindings tm =
    try Ok (from_nominal_with_bindings' bindings tm) with
    | FailedFromNominal msg -> Error msg
  ;;

  let from_nominal tm = from_nominal_with_bindings String.Map.empty tm

  let rec alpha_equivalent = fun t1 t2 ->
    match t1, t2 with
      | Operator (_, h1, subtms1), Operator (_, h2, subtms2)
      -> String.(h1 = h2) && (match List.zip subtms1 subtms2 with
        | Ok zipped -> List.for_all zipped ~f:(fun (Scope (_, body1), Scope (_, body2)) ->
            match List.zip body1 body2 with
              | Ok bodies -> List.for_all
                ~f:(fun (b1, b2) -> alpha_equivalent b1 b2)
                bodies
              | _ -> false)
        | Unequal_lengths -> false
      )
      | Var (_, i1, j1), Var (_, i2, j2)
      -> i1 = i2 && j1 = j2
      | Primitive (_, p1), Primitive (_, p2)
      -> Primitive.(p1 = p2)
      | _, _
      -> false
end

and Nominal : sig
  type 'loc scope = Scope of 'loc Pattern.t list * 'loc term list

  and 'loc term =
    | Operator of 'loc * string * 'loc scope list
    | Var of 'loc * string
    | Primitive of 'loc * Primitive.t

  val location : 'loc term -> 'loc

  val pp_term : Format.formatter -> 'loc Nominal.term -> unit
  val pp_term_range : Format.formatter -> Range.t Nominal.term -> unit
  val pp_term_str : 'loc Nominal.term -> string

  val pp_scope : Format.formatter -> 'loc Nominal.scope -> unit
  val pp_scope_range : Format.formatter -> Range.t Nominal.scope -> unit
  val pp_scope_str : 'loc Nominal.scope -> string

  val serialize : _ Nominal.term -> Bytes.t
  val deserialize : Bytes.t -> unit term option

  val jsonify : _ Nominal.term -> Json.t
  val unjsonify : Json.t -> unit Nominal.term option

  val hash : _ Nominal.term -> string
  val erase : 'loc term -> unit term
  val erase_scope : 'loc scope -> unit scope

  exception ToPatternFailure of unit scope

  val to_pattern_exn : 'loc Nominal.term -> 'loc Pattern.t
  val to_pattern : 'loc Nominal.term -> ('loc Pattern.t, unit scope) Result.t
  val pattern_to_term : 'loc Pattern.t -> 'loc Nominal.term

  module Parse (Comment : ParseUtil.Angstrom.Comment_int) : sig
    val t : Range.t term Angstrom.t
  end
end = struct
  type 'loc scope = Scope of 'loc Pattern.t list * 'loc term list

  and 'loc term =
    | Operator of 'loc * string * 'loc scope list
    | Var of 'loc * string
    | Primitive of 'loc * Primitive.t

  let any, comma, list, str, string, semi, pf =
    Fmt.(any, comma, list, str, string, semi, pf)

  let location = function
    | Operator (loc, _, _)
    | Var (loc, _)
    | Primitive (loc, _)
    -> loc

  let rec pp_term ppf = function
    | Operator (_, tag, subtms)
    -> pf ppf "@[<2>%s(%a)@]"
      tag
      (list ~sep:semi pp_scope) subtms
    | Var (_, v)
    -> string ppf v
    | Primitive (_, p)
    -> Primitive.pp ppf p

  and pp_scope ppf (Scope (bindings, body)) =
    let pp_body = list ~sep:comma pp_term in
    match bindings with
    | [] -> pp_body ppf body
    | _ -> pf ppf "%a.@ %a"
      (list ~sep:(any ".@ ") Pattern.pp) bindings
      pp_body body
  ;;

  let rec pp_term_range ppf =
    let open Range in
    function
    | Operator ({ start; finish }, tag, subtms)
    -> pf ppf "@[<2>%s{%u,%u}(%a)@]"
      tag start finish
      (list ~sep:semi pp_scope_range) subtms
    | Var ({ start; finish }, v)
    -> pf ppf "%a{%u,%u}" string v start finish
    | Primitive ({ start; finish }, p)
    -> pf ppf "%a{%u,%u}" Primitive.pp p start finish

  and pp_scope_range ppf (Scope (bindings, body)) =
    let pp_body = list ~sep:comma pp_term_range in
    match bindings with
    | [] -> pp_body ppf body
    | _ -> pf ppf "%a.@ %a"
      (list ~sep:(any ".@ ") Pattern.pp_range) bindings
      pp_body body
  ;;

  let pp_term_str tm = str "%a" pp_term tm
  let pp_scope_str scope = str "%a" pp_scope scope
  let array_map f args = args |> List.map ~f |> Array.of_list |> Json.array

  let rec jsonify tm =
    let array, string = Json.(array, string) in
    match tm with
      | Operator (_, tag, tms) ->
        array [| string "o"; string tag; array_map jsonify_scope tms |]
      | Var (_, name) -> array [| string "v"; string name |]
      | Primitive (_, p) -> array [| string "p"; Primitive.jsonify p |]

  and jsonify_scope (Scope (pats, body)) : Json.t =
    let body' = body
      |> List.map ~f:jsonify
      |> List.to_array
      |> Json.array
    in
    Json.array [| array_map Pattern.jsonify pats; body' |]
  ;;

  let rec unjsonify =
    let open Option.Let_syntax in
    Json.(function
    | Array [| String "o"; String tag; Array scopes |]
    -> let%map scopes' = scopes
         |> Array.to_list
         |> List.map ~f:unjsonify_scope
         |> Option.all
      in
      Operator ((), tag, scopes')
    | Array [| String "v"; String name |]
    -> Some (Var ((), name))
    | Array [| String "p"; prim |]
    -> let%map prim' = Primitive.unjsonify prim in
       Primitive ((), prim')
    | _ -> None)

  and unjsonify_scope = Json.(function
    | Array [||]
    -> None
    | Array arr
    -> let open Option.Let_syntax in
       let binders, body = arr |> Array.to_list |> Util.List.unsnoc in
       let%bind binders' = binders
         |> List.map ~f:Pattern.unjsonify
         |> Option.all
       in
       let%bind body' = match body with
         | Array bodies -> bodies
           |> Array.to_list
           |> List.map ~f:unjsonify
           |> Option.all
         | _ -> None
       in
       Some (Scope (binders', body'))
    | _ -> None
  )
  ;;

  let serialize = fun tm -> tm |> jsonify |> Cbor.encode

  let deserialize = fun buf -> buf |> Cbor.decode |> Option.bind ~f:unjsonify

  let hash = fun tm -> tm |> serialize |> Util.Sha256.hash

  let rec erase : 'loc term -> unit term
    = function
      | Operator (_, name, scopes)
      -> Operator ((), name, List.map scopes ~f:erase_scope)
      | Var (_, name) -> Var ((), name)
      | Primitive (_, p) -> Primitive ((), p)

  and erase_scope : 'loc scope -> unit scope
    = fun (Scope (pats, tms)) ->
      Scope (List.map pats ~f:Pattern.erase, List.map tms ~f:erase)
  ;;

  exception ToPatternFailure of unit scope

  (** @raise ToPatternFailure *)
  let rec to_pattern_exn : 'loc term -> 'loc Pattern.t = function
    | Var (loc, name) -> if String.is_substring_at name ~pos:0 ~substring:"_"
      then Ignored (loc, String.slice name 1 0)
      else Var (loc, name)
    | Operator (loc, name, tms)
    -> Operator (loc, name, List.map tms ~f:scope_to_patterns_exn)
    | Primitive (loc, prim) -> Primitive (loc, prim)

  (** @raise ToPatternFailure *)
  and scope_to_patterns_exn : 'loc scope -> 'loc Pattern.t list = function
    | Scope ([], tms) -> List.map tms ~f:to_pattern_exn
    | Scope (binders, tms) ->
        let binders' = List.map binders ~f:Pattern.erase in
        let tms' = List.map tms ~f:erase in
        raise (ToPatternFailure (Scope (binders', tms')))
  ;;

  let to_pattern : 'loc term -> ('loc Pattern.t, unit scope) Result.t
    = fun tm ->
      try
        Ok (to_pattern_exn tm)
      with
        ToPatternFailure scope -> Error scope

  let rec pattern_to_term : 'loc Pattern.t -> 'loc Nominal.term = function
    | Operator (loc, name, pats)
    -> Operator
      ( loc
      , name
      (* TODO: should the scope really inherit the 'loc? *)
      , List.map pats ~f:(fun pats' -> Scope ([], List.map pats' ~f:pattern_to_term))
      )
    | Primitive (loc, prim) -> Primitive (loc, prim)
    | Var (loc, name) -> Var (loc, name)
    | Ignored (loc, name) -> Var (loc, "_" ^ name)
  ;;

  module Parse (Comment : ParseUtil.Angstrom.Comment_int) = struct
    open Angstrom
    module Parsers = ParseUtil.Angstrom.Mk(Comment)
    module Primitive = Primitive.Parse(Comment)

    type tm_or_sep =
      | Tm of Range.t Nominal.term
      | Sep of char

    type parse_state =
      | PossiblyBinding
      | DefinitelyTerm

    let t : Range.t Nominal.term Angstrom.t
      = let char, identifier, parens = Parsers.(char, identifier, parens) in
        fix (fun term ->

          let t_or_sep : tm_or_sep Angstrom.t
            = choice
            [ (fun c -> Sep c) <$> (choice [char '.'; char ','; char ';'])
            ; (fun tm -> Tm tm) <$> term
            ]
          in

          (* (b11. ... b1n. t11, ... t1n; b21. ... b2n. t21, ... t2n) *)
          let accumulate
            : Range.t -> string -> tm_or_sep list -> Range.t Nominal.term Angstrom.t
            = fun range tag tokens ->

              (* terms encountered between '.'s, before hitting ',' / ';' *)
              let binding_queue : Range.t Nominal.term Queue.t = Queue.create () in
              (* terms encountered between ','s, before hitting ';' *)
              let list_queue : Range.t Nominal.term Queue.t = Queue.create () in
              (* scopes encountered *)
              let scope_queue : Range.t Nominal.scope Queue.t = Queue.create () in

              let rec go parse_state = function
                | [] -> return (Operator (range, tag, Queue.to_list scope_queue))
                | Tm tm :: Sep '.' :: rest
                -> if Caml.(parse_state = DefinitelyTerm)
                   then fail "Unexpected '.' when parsing a list"
                   else
                     begin
                       Queue.enqueue binding_queue tm;
                       go parse_state rest
                     end
                | Tm tm :: Sep ',' :: rest
                -> Queue.enqueue list_queue tm;
                   go DefinitelyTerm rest
                | Tm tm :: Sep ';' :: rest (* Note: allow trailing ';' *)
                | Tm tm :: ([] as rest)
                -> let binders = binding_queue
                     |> Queue.to_list
                     |> List.map ~f:to_pattern_exn
                   in
                   Queue.clear binding_queue;
                   Queue.enqueue list_queue tm;
                   let tms = Queue.to_list list_queue in
                   Queue.clear list_queue;
                   Queue.enqueue scope_queue (Scope (binders, tms));
                   go PossiblyBinding rest

                | _ -> fail "Malformed term"
              in

              go PossiblyBinding tokens
          in

          pos >>= fun p1 ->
          choice
            [ Primitive.t >>|
              (fun (prim, { finish; _ }) -> Primitive (Range.mk p1 finish, prim))
            ; identifier >>= fun (ident, _) -> choice
              [ begin
                  parens (many t_or_sep) >>= fun tokens ->
                  pos >>= fun p2 ->
                  accumulate (Range.mk p1 p2) ident tokens
                end
              ; pos >>| fun p2 -> (Var (Range.mk p1 p2, ident))
              ]
            ]
        ) <?> "term"
  end

end

module Properties = struct
  open Nominal

  let json_round_trip1 : unit term -> bool
    = fun t -> match t |> jsonify |> unjsonify with
      | None -> false
      | Some t' -> Caml.(t = t')

  let json_round_trip2 : Util.Json.t -> bool
    = fun json -> match json |> unjsonify with
      | None -> false
      | Some t -> Util.Json.(jsonify t = json)

  module Parse = Parse(ParseUtil.Angstrom.NoComment)

  let string_round_trip1 : unit term -> bool
    = fun t -> match t |> pp_term_str |> Angstrom.parse_string ~consume:All Parse.t with
      | Ok prim -> Caml.(erase prim = t)
      | Error _ -> false

  let string_round_trip2 : string -> bool
    = fun str -> match Angstrom.parse_string ~consume:All Parse.t str with
      | Ok prim -> let str' = pp_term_str prim in Base.String.(str' = str)
      | Error _ -> true (* malformed input *)
end

let%test_module "Nominal" =
  (module struct
    open Nominal

    let print_serialize tm =
      let bytes = serialize tm in
      bytes
      |> Bytes.to_array
      |> Array.iter ~f:(fun char -> Printf.printf "%02x" (Char.to_int char))
    ;;

    let print_hash tm = Printf.printf "%s" (hash tm)
    let ( = ) = Caml.( = )
    let tm = Var ((), "x")
    let j_tm = Json.(Array [| String "v"; String "x" |])

    let%test _ = jsonify tm = j_tm

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 8261766178 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| bbc37ed1e26f8481398dc797bd0b3ec2e3ae954e1c3f69b461a487d8703ec3d6 |}]
    ;;

    let tm = Operator ((), "Z", [])

    let%test _ =
      jsonify tm = Json.(Array [| String "o"; String "Z"; Array [||] |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615a80 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c |}]
    ;;

    let tm = Operator ((), "S", [ Scope ([ Var ((), "x")  ], [Var ((), "x") ]) ])

    let%test _ =
      jsonify tm
      = Json.(
          Array
            [| String "o"
             ; String "S"
             ; Array [| (* scopes *)
                 Array [| (* scope *)
                   Array [| j_tm |]; (* binders *)
                   Array [| j_tm |];  (* children *)
                 |]
               |]
            |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f61538182818261766178818261766178 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 5898e2f5a6af65e5d43539edd0fa5c539fa4dce2869b4a70e793c5f796658192 |}]
    ;;

    let tm = Primitive ((), PrimInteger (Bigint.of_string "12345"))

    let%test _ =
      jsonify tm
      = Json.(Array [| String "p"; Array [| String "i"; String "12345" |] |])
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 826170826169653132333435 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5 |}]
    ;;

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 826170826169653132333435 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| e69505a495d739f89cf515c31cf3a2cca4e29a1a4fede9a331b45207a6fb33e5 |}]
    ;;

    let%test _ = to_pattern_exn (Var (1, "abc")) = Var (1, "abc")
    let%test _ = to_pattern_exn (Var (2, "_abc")) = Ignored (2, "abc")
    let%test _ = to_pattern_exn (Var (3, "_")) = Ignored (3, "")
    let%test _ = pattern_to_term (Ignored (4, "abc")) = Var (4, "_abc")
    let%test _ = pattern_to_term (Ignored (5, "")) = Var (5, "_")
  end)
;;

let%test_module "TermParser" = (module struct
  let (=) = Caml.(=)
  open Nominal
  module Parse = Nominal.Parse(ParseUtil.Angstrom.NoComment)

  let parse = Angstrom.(parse_string ~consume:All
    (ParseUtil.Angstrom.whitespace *> Parse.t))

  let print_parse = fun str -> match parse str with
    | Error msg -> Caml.print_string ("failed: " ^ msg)
    | Ok tm ->
      Nominal.pp_term Caml.Format.std_formatter tm;
      Fmt.pr "\n";
      Nominal.pp_term_range Caml.Format.std_formatter tm
  ;;

  let%test _ =
    parse "x" |> Result.map ~f:erase =
    Ok (Var ((), "x"))
  let%test _ =
    parse "123" |> Result.map ~f:erase =
    Ok (Primitive ((), PrimInteger (Bigint.of_int 123)))
  let%test _ =
    parse "\"abc\"" |> Result.map ~f:erase =
    Ok (Primitive ((), PrimString "abc"))

  let x = Var ((), "x")
  let t = Operator ((), "true", [])

  let%test _ =
    parse "lam(x. x)" |> Result.map ~f:erase =
    Ok (Operator ((), "lam", [ Scope ([ Var ((), "x") ], [x]) ]))

  let match_line a b = Operator ((), "match_line", [ Scope ([a], [b]) ])
  let match_lines subtms = Operator
    ( ()
    , "match_lines"
    , Base.List.map subtms ~f:(fun tm -> Scope ([], [tm]))
    )

  let%test _ = parse {| match() |} |> Result.map ~f:erase =
    Ok (Operator ((), "match", []))
  let%test _ = parse {| match(x; x) |} |> Result.map ~f:erase =
    Ok (Operator ((), "match", [ Scope ([], [x]); Scope ([], [x]) ]))
  let%test _ = parse {| match(true(); true()) |} |> Result.map ~f:erase =
    Ok (Operator ((), "match", [ Scope ([], [t]); Scope ([], [t]) ]))

  let%test _ = parse {| match(x;) |} |> Result.map ~f:erase =
    Ok (Operator ((), "match", [ Scope ([], [x]) ]))

  let%test _ =
    parse {|
    match(x; match_lines(
      match_line(foo(). true());
      match_line(bar(_; _x; y). y)
    )) |}
    |> Result.map ~f:erase
    =
    Ok (Operator ((), "match", [
      Scope ([], [x]);
      Scope ([], [match_lines [
        match_line (Pattern.Operator ((), "foo", [])) (Operator ((), "true", []));
        match_line
          (Pattern.Operator
            ( ()
            , "bar"
            , [[Ignored ((), "")]; [Ignored ((), "x")]; [Var ((), "y")]]
            ))
          (Var ((), "y"));
      ]]);
    ]))

  let%expect_test _ =
    print_parse {|"str"|};
    [%expect{|
      "str"
      "str"{0,5}
    |}]
  let%expect_test _ =
    print_parse {|a()|};
    [%expect{|
      a()
      a{0,3}()
    |}]
  let%expect_test _ =
    print_parse {|a(b)|};
    [%expect{|
      a(b)
      a{0,4}(b{2,3})
    |}]
  let%expect_test _ =
    print_parse {|a(b,c)|};
    [%expect{|
      a(b, c)
      a{0,6}(b{2,3}, c{4,5})
    |}]
  let%expect_test _ =
    print_parse {|a(b,c;d,e;)|};
    [%expect{|
      a(b, c; d, e)
      a{0,11}(b{2,3}, c{4,5}; d{6,7}, e{8,9})
    |}]
  let%expect_test _ =
    print_parse {|a(b.c;d,e;)|};
    [%expect{|
      a(b. c; d, e)
      a{0,11}(b{2,3}. c{4,5}; d{6,7}, e{8,9})
    |}]

end);;

open Base
module Printf = Caml.Printf
module Cbor = Lvca_util.Cbor
module Json = Lvca_util.Json
module String = Lvca_util.String

type 'loc term =
  | Operator of 'loc * string * 'loc scope list
  | Var of 'loc * string
  | Primitive of 'loc * Primitive.t

and 'loc scope = Scope of ('loc, Primitive.t) Pattern.t list * 'loc term list

let location = function Operator (loc, _, _) | Var (loc, _) | Primitive (loc, _) -> loc

let any, comma, list, str, string, semi, pf =
  Fmt.(any, comma, list, str, string, semi, pf)
;;

let rec pp_term ppf = function
  | Operator (_, tag, subtms) ->
    pf ppf "@[<2>%s(%a)@]" tag (list ~sep:semi pp_scope) subtms
  | Var (_, v) -> string ppf v
  | Primitive (_, p) -> Primitive.pp ppf p

and pp_scope ppf (Scope (bindings, body)) =
  let pp_body = list ~sep:comma pp_term in
  match bindings with
  | [] -> pp_body ppf body
  | _ ->
    pf
      ppf
      "%a.@ %a"
      (list ~sep:(any ".@ ") (Pattern.pp Primitive.pp))
      bindings
      pp_body
      body
;;

let rec pp_term_range ppf tm =
  OptRange.open_stag ppf (location tm);
  (match tm with
  | Operator (_, tag, subtms) ->
    pf ppf "@[<hv>%s(%a)@]" tag (list ~sep:semi pp_scope_range) subtms
  | Var (_, v) -> pf ppf "%a" string v
  | Primitive (_, p) -> pf ppf "%a" Primitive.pp p);
  OptRange.close_stag ppf (location tm)

and pp_scope_range ppf (Scope (bindings, body)) =
  let pp_body = list ~sep:comma pp_term_range in
  match bindings with
  | [] -> pp_body ppf body
  | _ ->
    pf
      ppf
      "%a.@ %a"
      (list ~sep:(any ".@ ") (Pattern.pp_range Primitive.pp))
      bindings
      pp_body
      body
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
  let body' = body |> List.map ~f:jsonify |> List.to_array |> Json.array in
  Json.array [| array_map (Pattern.jsonify Primitive.jsonify) pats; body' |]
;;

let rec unjsonify =
  let open Option.Let_syntax in
  Json.(
    function
    | Array [| String "o"; String tag; Array scopes |] ->
      let%map scopes' =
        scopes |> Array.to_list |> List.map ~f:unjsonify_scope |> Option.all
      in
      Operator ((), tag, scopes')
    | Array [| String "v"; String name |] -> Some (Var ((), name))
    | Array [| String "p"; prim |] ->
      let%map prim' = Primitive.unjsonify prim in
      Primitive ((), prim')
    | _ -> None)

and unjsonify_scope =
  Json.(
    function
    | Array [||] -> None
    | Array arr ->
      let open Option.Let_syntax in
      let binders, body = arr |> Array.to_list |> Lvca_util.List.unsnoc in
      let%bind binders' =
        binders |> List.map ~f:(Pattern.unjsonify Primitive.unjsonify) |> Option.all
      in
      let%bind body' =
        match body with
        | Array bodies -> bodies |> Array.to_list |> List.map ~f:unjsonify |> Option.all
        | _ -> None
      in
      Some (Scope (binders', body'))
    | _ -> None)
;;

let serialize tm = tm |> jsonify |> Cbor.encode
let deserialize buf = buf |> Cbor.decode |> Option.bind ~f:unjsonify
let hash tm = tm |> serialize |> Lvca_util.Sha256.hash

let rec erase : 'loc term -> unit term = function
  | Operator (_, name, scopes) -> Operator ((), name, List.map scopes ~f:erase_scope)
  | Var (_, name) -> Var ((), name)
  | Primitive (_, p) -> Primitive ((), p)

and erase_scope : 'loc scope -> unit scope =
 fun (Scope (pats, tms)) -> Scope (List.map pats ~f:Pattern.erase, List.map tms ~f:erase)
;;

exception ToPatternFailure of unit scope

(** @raise ToPatternFailure *)
let rec to_pattern_exn : 'loc term -> ('loc, Primitive.t) Pattern.t = function
  | Var (loc, name) ->
    if String.is_substring_at name ~pos:0 ~substring:"_"
    then Ignored (loc, String.slice name 1 0)
    else Var (loc, name)
  | Operator (loc, name, tms) ->
    Operator (loc, name, List.map tms ~f:scope_to_patterns_exn)
  | Primitive (loc, prim) -> Primitive (loc, prim)

(** @raise ToPatternFailure *)
and scope_to_patterns_exn : 'loc scope -> ('loc, Primitive.t) Pattern.t list = function
  | Scope ([], tms) -> List.map tms ~f:to_pattern_exn
  | Scope (binders, tms) ->
    let binders' = List.map binders ~f:Pattern.erase in
    let tms' = List.map tms ~f:erase in
    raise (ToPatternFailure (Scope (binders', tms')))
;;

let to_pattern : 'loc term -> (('loc, Primitive.t) Pattern.t, unit scope) Result.t =
 fun tm -> try Ok (to_pattern_exn tm) with ToPatternFailure scope -> Error scope
;;

let rec pattern_to_term : ('loc, Primitive.t) Pattern.t -> 'loc term = function
  | Operator (loc, name, pats) ->
    Operator
      ( loc
      , name (* TODO: should the scope really inherit the 'loc? *)
      , List.map pats ~f:(fun pats' -> Scope ([], List.map pats' ~f:pattern_to_term)) )
  | Primitive (loc, prim) -> Primitive (loc, prim)
  | Var (loc, name) -> Var (loc, name)
  | Ignored (loc, name) -> Var (loc, "_" ^ name)
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Primitive = Primitive.Parse (Comment)

  type tm_or_sep =
    | Tm of OptRange.t term
    | Sep of char

  type parse_state =
    | PossiblyBinding
    | DefinitelyTerm

  let t : OptRange.t term ParseUtil.t =
    let open Parsers in
    fix (fun term ->
        let t_or_sep : tm_or_sep ParseUtil.t =
          choice
            [ (fun c -> Sep c) <$> choice [ char '.'; char ','; char ';' ]
            ; (fun tm -> Tm tm) <$> term
            ]
        in
        (* (b11. ... b1n. t11, ... t1n; b21. ... b2n. t21, ... t2n) *)
        let accumulate
            : OptRange.t -> string -> tm_or_sep list -> OptRange.t term ParseUtil.t
          =
         fun range tag tokens ->
          (* terms encountered between '.'s, before hitting ',' / ';' *)
          let binding_queue : OptRange.t term Queue.t = Queue.create () in
          (* terms encountered between ','s, before hitting ';' *)
          let list_queue : OptRange.t term Queue.t = Queue.create () in
          (* scopes encountered *)
          let scope_queue : OptRange.t scope Queue.t = Queue.create () in
          let rec go parse_state = function
            | [] -> return ~pos:range (Operator (range, tag, Queue.to_list scope_queue))
            | Tm tm :: Sep '.' :: rest ->
              if Caml.(parse_state = DefinitelyTerm)
              then fail "Unexpected '.' when parsing a list"
              else (
                Queue.enqueue binding_queue tm;
                go parse_state rest)
            | Tm tm :: Sep ',' :: rest ->
              Queue.enqueue list_queue tm;
              go DefinitelyTerm rest
            | Tm tm :: Sep ';' :: rest (* Note: allow trailing ';' *)
            | Tm tm :: ([] as rest) ->
              let binders =
                binding_queue |> Queue.to_list |> List.map ~f:to_pattern_exn
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
        pos
        >>= fun p1 ->
        choice
          [ (Primitive.t
            >>|| fun ~pos prim -> Primitive (OptRange.extend_to pos p1, prim), pos)
          ; (identifier
            >>= fun ident ->
            choice
              [ (parens (many t_or_sep)
                >>= fun tokens ->
                pos >>= fun p2 -> accumulate (OptRange.mk p1 p2) ident tokens)
              ; (pos >>| fun p2 -> Var (OptRange.mk p1 p2, ident))
              ])
          ])
    <?> "term"
  ;;

  let whitespace_t = Parsers.(junk *> t)
end

module Properties = struct
  let json_round_trip1 : unit term -> bool =
   fun t ->
    match t |> jsonify |> unjsonify with None -> false | Some t' -> Caml.(t = t')
 ;;

  let json_round_trip2 : Lvca_util.Json.t -> bool =
   fun json ->
    match json |> unjsonify with
    | None -> false
    | Some t -> Lvca_util.Json.(jsonify t = json)
 ;;

  module Parse = Parse (ParseUtil.NoComment)

  let string_round_trip1 : unit term -> bool =
   fun t ->
    match t |> pp_term_str |> ParseUtil.parse_string Parse.t with
    | Ok prim -> Caml.(erase prim = t)
    | Error _ -> false
 ;;

  let string_round_trip2 : string -> bool =
   fun str ->
    match ParseUtil.parse_string Parse.t str with
    | Ok prim ->
      let str' = pp_term_str prim in
      Base.String.(str' = str)
    | Error _ -> true
 ;;

  (* malformed input *)

  (* XXX add de bruijn properties *)
end

let%test_module "Nominal" =
  (module struct
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

    let%test _ = jsonify tm = Json.(Array [| String "o"; String "Z"; Array [||] |])

    let%expect_test _ =
      print_serialize tm;
      [%expect {| 83616f615a80 |}]
    ;;

    let%expect_test _ =
      print_hash tm;
      [%expect {| 2380ed848a0c5ce3d0ad7420e841578e4068f394b37b9b11bd3c34cea391436c |}]
    ;;

    let tm = Operator ((), "S", [ Scope ([ Var ((), "x") ], [ Var ((), "x") ]) ])

    let%test _ =
      jsonify tm
      = Json.(
          Array
            [| String "o"
             ; String "S"
             ; Array
                 [| (* scopes *)
                    Array
                      [| (* scope *)
                         Array [| j_tm |]
                       ; (* binders *)
                         Array [| j_tm |] (* children *)
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
      jsonify tm = Json.(Array [| String "p"; Array [| String "i"; String "12345" |] |])
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

let%test_module "TermParser" =
  (module struct
    let ( = ) = Caml.( = )

    module Parse' = Parse (ParseUtil.NoComment)

    let parse = ParseUtil.parse_string Parse'.whitespace_t

    let print_parse str =
      match parse str with
      | Error msg -> Caml.print_string ("failed: " ^ msg)
      | Ok tm ->
        Fmt.pr "%a\n" pp_term tm;
        Fmt.pr "%a" pp_term_range tm
    ;;

    let%test _ = parse "x" |> Result.map ~f:erase = Ok (Var ((), "x"))

    let%test _ =
      parse "123"
      |> Result.map ~f:erase
      = Ok (Primitive ((), PrimInteger (Bigint.of_int 123)))
    ;;

    let%test _ =
      parse "\"abc\"" |> Result.map ~f:erase = Ok (Primitive ((), PrimString "abc"))
    ;;

    let x = Var ((), "x")
    let t = Operator ((), "true", [])

    let%test _ =
      parse "lam(x. x)"
      |> Result.map ~f:erase
      = Ok (Operator ((), "lam", [ Scope ([ Var ((), "x") ], [ x ]) ]))
    ;;

    let match_line a b = Operator ((), "match_line", [ Scope ([ a ], [ b ]) ])

    let match_lines subtms =
      Operator ((), "match_lines", Base.List.map subtms ~f:(fun tm -> Scope ([], [ tm ])))
    ;;

    let%test _ =
      parse {| match() |} |> Result.map ~f:erase = Ok (Operator ((), "match", []))
    ;;

    let%test _ =
      parse {| match(x; x) |}
      |> Result.map ~f:erase
      = Ok (Operator ((), "match", [ Scope ([], [ x ]); Scope ([], [ x ]) ]))
    ;;

    let%test _ =
      parse {| match(true(); true()) |}
      |> Result.map ~f:erase
      = Ok (Operator ((), "match", [ Scope ([], [ t ]); Scope ([], [ t ]) ]))
    ;;

    let%test _ =
      parse {| match(x;) |}
      |> Result.map ~f:erase
      = Ok (Operator ((), "match", [ Scope ([], [ x ]) ]))
    ;;

    let%test _ =
      parse
        {|
    match(x; match_lines(
      match_line(foo(). true());
      match_line(bar(_; _x; y). y)
    )) |}
      |> Result.map ~f:erase
      = Ok
          (Operator
             ( ()
             , "match"
             , [ Scope ([], [ x ])
               ; Scope
                   ( []
                   , [ match_lines
                         [ match_line
                             (Pattern.Operator ((), "foo", []))
                             (Operator ((), "true", []))
                         ; match_line
                             (Pattern.Operator
                                ( ()
                                , "bar"
                                , [ [ Ignored ((), "") ]
                                  ; [ Ignored ((), "x") ]
                                  ; [ Var ((), "y") ]
                                  ] ))
                             (Var ((), "y"))
                         ]
                     ] )
               ] ))
    ;;

    let%expect_test _ =
      print_parse {|"str"|};
      (*012345*)
      [%expect {|
      "str"
      <0-5>"str"</0-5>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a()|};
      (*0123*)
      [%expect {|
      a()
      <0-3>a()</0-3>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b)|};
      (*01234*)
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
      print_parse {|a(b,c;d,e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b, c; d, e)
      <0-11>a(<2-3>b</2-3>, <4-5>c</4-5>; <6-7>d</6-7>, <8-9>e</8-9>)</0-11>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c;d,e;)|};
      (*012345678901*)
      [%expect
        {|
      a(b. c; d, e)
      <0-11>a(<2-3>b</2-3>. <4-5>c</4-5>; <6-7>d</6-7>, <8-9>e</8-9>)</0-11>
    |}]
    ;;

    let%expect_test _ =
      print_parse {|a(b.c;d,e;f,g)|};
      (*012345678901234*)
      [%expect
        {|
      a(b. c; d, e; f, g)
      <0-14>a(<2-3>b</2-3>. <4-5>c</4-5>; <6-7>d</6-7>, <8-9>e</8-9>; <10-11>f</10-11>, <12-13>g</12-13>)</0-14>
    |}]
    ;;
  end)
;;

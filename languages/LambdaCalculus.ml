open Base
open Lvca_syntax

let eval tm =
  let open Result.Let_syntax in
  let tm_str tm = tm |> DeBruijn.to_nominal |> Option.value_exn |> Nominal.pp_term_str in
  let rec eval' tm =
    match tm with
    | DeBruijn.Operator (_, "app", [ Second [ t1 ]; Second [ t2 ] ]) ->
      let%bind t1' = eval' t1 in
      let%bind t2' = eval' t2 in
      (match t1' with
      | DeBruijn.Operator (_, "lam", [ First body ]) ->
        let%bind body' =
          match DeBruijn.open_scope t2' body with
          | [ tm ] -> Ok tm
          | tms ->
            let tms_str = tms |> List.map ~f:tm_str |> String.concat ~sep:", " in
            Error (Printf.sprintf "Unexpected terms %s" tms_str)
        in
        eval' body'
      | _ -> Error (Printf.sprintf "Unexpected term (1) %s" (tm_str tm)))
    | Operator (_, "lam", _) -> Ok tm
    | BoundVar (_, _) -> Error "bound variable encountered"
    | FreeVar _ -> Ok tm
    | _ -> Error (Printf.sprintf "Unexpected term (2) %s" (tm_str tm))
  in
  let%bind db_tm = DeBruijn.of_nominal tm in
  let%bind db_tm' = eval' db_tm in
  match DeBruijn.to_nominal db_tm' with
  | None -> Error "Failed to convert result back to nominal representation."
  | Some tm' -> Ok tm'
;;

module AngstromParse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  let location = Nominal.location

  let t_var : OptRange.t Nominal.term Parsers.t =
    Parsers.identifier >>|| fun ~pos name -> Nominal.Var (pos, name), pos
  ;;

  let p_var : OptRange.t Pattern.t Parsers.t =
    Parsers.identifier >>|| fun ~pos name -> Pattern.Var (pos, name), pos
  ;;

  (* Precedence 0: lam (right-associative) 1: app (left-associative) *)

  let t : OptRange.t Nominal.term Parsers.t =
    fix (fun t ->
        let atom = t_var <|> parens t in
        let lam : OptRange.t Nominal.term Parsers.t =
          pos
          >>= fun start ->
          lift4
            (fun _lam var _arr body ->
              let range = OptRange.extend_to (location body) start in
              let tm = Nominal.Operator (range, "lam", [ Scope ([ var ], [ body ]) ]) in
              tm)
            (char '\\')
            p_var
            (string "->")
            t
        in
        let f (x, rng1) (y, rng2) =
          let range = OptRange.(rng1 <> rng2) in
          let tm =
            Nominal.Operator (range, "app", [ Scope ([], [ x ]); Scope ([], [ y ]) ])
          in
          tm, range
        in
        let atom_or_lam = attach_pos (atom <|> lam) in
        atom_or_lam
        >>= fun init -> many atom_or_lam >>| fun atoms -> List.fold atoms ~init ~f |> fst)
  ;;

  let whitespace_t = junk *> t
end

module ParseNoComment = AngstromParse (ParseUtil.NoComment)

let pp : OptRange.t Nominal.term Fmt.t =
  let rec pp' prec ppf tm =
    let module Format = Caml.Format in
    Format.pp_open_stag ppf (Format.String_tag (Nominal.hash tm));
    OptRange.open_stag ppf (Nominal.location tm);
    (match tm with
    | Nominal.Operator (_, "app", [ Scope ([], [ a ]); Scope ([], [ b ]) ]) ->
      if prec > 1
      then Fmt.pf ppf "(%a %a)" (pp' 1) a (pp' 2) b
      else Fmt.pf ppf "%a %a" (pp' 1) a (pp' 2) b
    | Var (_, name) -> Fmt.pf ppf "%s" name
    | Operator (_, "lam", [ Scope ([ Pattern.Var (_range, name) ], [ body ]) ]) ->
      if prec > 0
      then Fmt.pf ppf {|(\%s -> %a)|} name (pp' 0) body
      else Fmt.pf ppf {|\%s -> %a|} name (pp' 0) body
    | tm -> Fmt.failwith "Invalid Lambda term %a" Nominal.pp_term tm);
    OptRange.close_stag ppf (Nominal.location tm);
    (* range tag *)
    Format.pp_close_stag ppf ()
    (* hash tag *)
  in
  pp' 0
;;

let%test_module "Lambda Calculus" =
  (module struct
    let () = Caml.Format.set_tags false
    let parse str = ParseUtil.parse_string ParseNoComment.whitespace_t str
    let pretty_parse str = parse str |> Result.ok_or_failwith |> Fmt.pr "%a" pp

    let pretty_eval_parse str =
      parse str
      |> Result.ok_or_failwith
      |> eval
      |> Result.ok_or_failwith
      |> Fmt.pr "%a" pp
    ;;

    let%expect_test _ =
      pretty_parse "a";
      [%expect {| a |}]
    ;;

    let%expect_test _ =
      pretty_parse {|\a -> a|};
      [%expect {| \a -> a |}]
    ;;

    let%expect_test _ =
      pretty_parse {|f g|};
      [%expect {| f g |}]
    ;;

    let%expect_test _ =
      pretty_parse {|f g x|};
      [%expect {| f g x |}]
    ;;

    let%expect_test _ =
      pretty_parse {|(f g) (h x)|};
      [%expect {| f g (h x) |}]
    ;;

    let%expect_test _ =
      pretty_parse {|\f -> \g -> \x -> f (g x)|};
      [%expect {| \f -> \g -> \x -> f (g x) |}]
    ;;

    (* let%expect_test _ = pretty_parse {|\x -> x z \y -> x y|}; [%expect{|\x -> x z \y ->
       x y|}] *)

    let%expect_test _ =
      pretty_eval_parse {|\x -> x|};
      [%expect {|\x -> x|}]
    ;;

    let%expect_test _ =
      pretty_eval_parse {|(\x -> x) (\y -> y)|};
      [%expect {|\y -> y|}]
    ;;

    let%expect_test _ =
      pretty_eval_parse {|(\x -> \y -> x) z|};
      [%expect {|\y -> z|}]
    ;;

    (* let%expect_test _ = pretty_eval_parse {|(\x -> \y -> x) y|}; [%expect{|\y' -> y|}]
       (* ??? *) *)

    let%expect_test _ =
      pretty_eval_parse {|(\x -> \y -> x) y x|};
      [%expect {|y|}]
    ;;
  end)
;;

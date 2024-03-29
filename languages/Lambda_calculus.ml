open Base
open Lvca_syntax
open Lvca_provenance

let language = [%lvca.abstract_syntax "tm := App(tm; tm) | Lam(tm. tm);"]

let eval tm =
  let open Result.Let_syntax in
  let tm_str tm =
    tm |> DeBruijn.to_nominal |> Option.value_exn |> Fmt.to_to_string Nominal.Term.pp
  in
  let rec eval' tm =
    match tm with
    | DeBruijn.Operator (_, "App", [ Second t1; Second t2 ]) ->
      let%bind t1' = eval' t1 in
      let%bind t2' = eval' t2 in
      (match t1' with
      | DeBruijn.Operator (_, "Lam", [ First body ]) ->
        eval' (DeBruijn.open_scope t2' body)
      | _ -> Error (Printf.sprintf "Unexpected term (1) %s" (tm_str tm)))
    | Operator (_, "Lam", _) -> Ok tm
    | Bound_var (_, _) -> Error "bound variable encountered"
    | Free_var _ -> Ok tm
    | _ -> Error (Printf.sprintf "Unexpected term (2) %s" (tm_str tm))
  in
  let%bind db_tm =
    tm |> DeBruijn.of_nominal |> Result.map_error ~f:(Fmt.to_to_string Nominal.Scope.pp)
  in
  let%bind db_tm' = eval' db_tm in
  match DeBruijn.to_nominal db_tm' with
  | None -> Error "Failed to convert result back to nominal representation."
  | Some tm' -> Ok tm'
;;

module Parse = struct
  open Lvca_parsing
  open C_comment_parser

  let info = Nominal.Term.info
  let identifier = lower_identifier Lvca_util.String.Set.empty

  let t_var : Nominal.Term.t Lvca_parsing.t =
    let%map range, name = identifier in
    Nominal.Term.Var (Provenance.of_range range, name)
  ;;

  let p_var : Pattern.t Lvca_parsing.t =
    let%map range, name = identifier in
    Pattern.Var (Provenance.of_range range, name)
  ;;

  (* Precedence 0: Lam (right-associative) 1: App (left-associative) *)

  let t : Nominal.Term.t Lvca_parsing.t =
    fix (fun t ->
        let atom = t_var <|> parens t in
        let lam : Nominal.Term.t Lvca_parsing.t =
          make4
            (fun ~info _lam var _arr body ->
              Nominal.Term.Operator
                (Provenance.of_range info, "Lam", [ Scope ([ var ], body) ]))
            (char '\\')
            p_var
            (string "->")
            t
        in
        let f (rng1, x) (rng2, y) =
          let range = Opt_range.union rng1 rng2 in
          let tm =
            Nominal.Term.Operator
              (Provenance.of_range range, "App", [ Scope ([], x); Scope ([], y) ])
          in
          range, tm
        in
        let atom_or_lam = atom <|> lam in
        let%bind init = atom_or_lam in
        let%map _, list = many (attach_pos atom_or_lam) in
        list |> List.fold ~init ~f |> snd)
  ;;
end

let pp =
  let rec pp' prec ppf tm =
    let module Format = Stdlib.Format in
    Format.pp_open_stag ppf (Format.String_tag (Nominal.Term.hash tm));
    Provenance.open_stag ppf (Nominal.Term.info tm);
    (match tm with
    | Nominal.Term.Operator (_, "App", [ Scope ([], a); Scope ([], b) ]) ->
      if prec > 1
      then Fmt.pf ppf "(%a %a)" (pp' 1) a (pp' 2) b
      else Fmt.pf ppf "%a %a" (pp' 1) a (pp' 2) b
    | Var (_, name) -> Fmt.string ppf name
    | Operator (_, "Lam", [ Scope ([ Pattern.Var (_range, name) ], body) ]) ->
      if prec > 0
      then Fmt.pf ppf {|(\%s -> %a)|} name (pp' 0) body
      else Fmt.pf ppf {|\%s -> %a|} name (pp' 0) body
    | tm -> Fmt.failwith "Invalid Lambda term %a" Nominal.Term.pp tm);
    Provenance.close_stag ppf (Nominal.Term.info tm);
    (* range tag *)
    Format.pp_close_stag ppf ()
    (* hash tag *)
  in
  pp' 0
;;

let%test_module "Lambda Calculus" =
  (module struct
    let () = Stdlib.Format.set_tags false
    let parse str = Lvca_parsing.(parse_string (whitespace *> Parse.t) str)
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

open Base

type t = Arity of Provenance.t * Valence.t list

let mk ?(provenance = Provenance.of_here [%here]) valences = Arity (provenance, valences)

let pp ppf (Arity (info, valences)) =
  Provenance.fmt_stag info Fmt.(parens (list ~sep:semi Valence.pp)) ppf valences
;;

let equivalent
    ?(info_eq = fun _ _ -> true)
    (Arity (i1, valences1))
    (Arity (i2, valences2))
  =
  info_eq i1 i2 && List.equal Valence.(equivalent ~info_eq) valences1 valences2
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let instantiate env (Arity (info, valences)) =
  Arity (info, List.map ~f:(Valence.instantiate env) valences)
;;

let parse =
  let open Lvca_parsing.Parser in
  let open Construction in
  let p =
    let+ valences = parens (sep_by (symbol ";") Valence.parse) in
    Arity (Provenance.of_here [%here], valences)
  in
  p <?> "arity"
;;

let%test_module "parsing" =
  (module struct
    let none = Provenance.of_here [%here]
    let tm = Sort.Name (none, "tm")
    let tm_v = Valence.Valence ([], tm)
    let integer = Sort.Name (none, "integer")
    let integer_v = Valence.Valence ([], integer)
    let ( = ) = equivalent
    let go = Lvca_parsing.Parser.(parse_string_or_failwith parse)

    let%test_unit _ = assert (go "(integer)" = Arity (none, [ integer_v ]))
    let%test_unit _ = assert (go "(tm; tm)" = Arity (none, [ tm_v; tm_v ]))

    let%test_unit _ =
      assert (
        go "(tm. tm)  // comment"
        = Arity (none, [ Valence.Valence ([ Sort_binding tm ], tm) ]))
    ;;

    let%test_unit _ = assert (go "(tm)" = Arity (none, [ Valence.Valence ([], tm) ]))

    let%test_unit _ =
      assert (
        go "(tm[tm]. tm)"
        = Arity
            ( none
            , [ Valence.Valence ([ Sort_pattern { pattern_sort = tm; var_sort = tm } ], tm)
              ] ))
    ;;

    let expect_okay str =
      match Lvca_parsing.Parser.parse_string parse str with
      | Ok _ -> ()
      | Error msg -> Stdio.print_string msg
    ;;

    let%expect_test _ = expect_okay "(tm[tm]. tm[tm]. tm)"
    let%expect_test _ = expect_okay "((foo bar)[baz quux]. tm)"
    let%expect_test _ = expect_okay "((foo bar)[baz quux]. tm)"
    let%expect_test _ = expect_okay "((foo bar)[baz quux]. tm)  // comment"
    let%test_unit _ = assert (go "()" = Arity (none, []))
    let%test_unit _ = assert (go "()" = Arity (none, []))
  end)
;;

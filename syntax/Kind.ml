open Base

type t = Kind of Provenance.t * int

let mk ?(provenance = Provenance.of_here [%here]) n = Kind (provenance, n)

let equivalent ?(info_eq = fun _ _ -> true) (Kind (i1, k1)) (Kind (i2, k2)) =
  info_eq i1 i2 && Int.(k1 = k2)
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )
let info (Kind (i, _)) = i

let pp ppf (Kind (info, k)) =
  Provenance.fmt_stag
    info
    Fmt.(list ~sep:(any " -> ") (any "*"))
    ppf
    (List.init k ~f:(Fn.const ()))
;;

module Parse = struct
  open Lvca_parsing.Parser
  open Construction

  let t =
    let p =
      let+ stars = sep_by1 (symbol "->") (symbol "*") in
      let location =
        stars
        |> List.map ~f:Lvca_parsing.Token_stream.Token.range
        |> Lvca_provenance.Opt_range.list_range
      in
      Kind (Provenance.of_range location, List.length stars)
    in
    p <?> "kind"
  ;;

  let decl =
    let p =
      let+ ident = lower_identifier
      and+ _ = symbol ":"
      and+ kind = t in
      ident, kind
    in
    p <?> "kind declaration"
  ;;

  let%test_module "parsing" =
    (module struct
      let pp_decl ppf (name, decl) = Fmt.pf ppf "%s: %a" name pp decl

      let%expect_test _ =
        let x = parse_string_or_failwith decl "foo: * -> *" in
        Fmt.pr "%a" pp_decl x;
        [%expect {|foo: * -> *|}]
      ;;

      let%expect_test _ =
        let x =
          parse_string_or_failwith
            (star decl)
            {|
            foo: * -> *  // comment
            bar: * -> *
            |}
        in
        Fmt.(pr "%a" (list pp_decl) x);
        [%expect {|
          foo: * -> *
          bar: * -> *
          |}]
      ;;
    end)
  ;;
end

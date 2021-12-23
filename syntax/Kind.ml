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
  open Lvca_parsing
  open C_comment_parser

  let t =
    sep_by1 (string "->") (char '*')
    >>~ (fun location stars -> Kind (Provenance.of_range location, List.length stars))
    <?> "kind"
  ;;

  let decl =
    lift3
      (fun ident _colon kind -> ident, kind)
      (lower_identifier Lvca_util.String.Set.empty)
      (char ':')
      t
    <?> "kind declaration"
  ;;

  let%test_module "parsing" =
    (module struct
      let pp_decl ppf (name, decl) = Fmt.pf ppf "%s: %a" name pp decl

      let%expect_test _ =
        let x = parse_string_or_failwith (junk *> decl) "foo: * -> *" in
        Fmt.pr "%a" pp_decl x;
        [%expect {|foo: * -> *|}]
      ;;

      let%expect_test _ =
        let x =
          parse_string_or_failwith
            (junk *> many decl)
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

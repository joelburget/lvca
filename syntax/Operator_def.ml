open Base
open Lvca_provenance

type t = Operator_def of Provenance.t * string * Arity.t

let mk ?(provenance = Provenance.of_here [%here]) name arity =
  Operator_def (provenance, name, arity)
;;

let equivalent
    ?(info_eq = fun _ _ -> true)
    (Operator_def (info1, name1, arity1))
    (Operator_def (info2, name2, arity2))
  =
  info_eq info1 info2 && String.(name1 = name2) && Arity.equivalent ~info_eq arity1 arity2
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let kind_check env (Operator_def (_info, _name, Arity (_, valences))) =
  List.fold valences ~init:env ~f:Valence.kind_check
;;

let pp ppf (Operator_def (info, name, arity)) =
  let pp' ppf () = Fmt.pf ppf "%s%a" name Arity.pp arity in
  Provenance.fmt_stag info pp' ppf ()
;;

let parse =
  let open Lvca_parsing.Parser in
  let open Construction in
  let p =
    (* TODO: include range in provenance *)
    let+ ident = upper_identifier
    and+ (Arity (prov, _) as arity) = Arity.parse in
    Operator_def (prov, ident, arity)
  in
  p <?> "operator definition"
;;

let%test_module "parsing" =
  (module struct
    let ( = ) = equivalent

    let%test_unit _ =
      let info1 = Provenance.of_range (Opt_range.mk 0 5) in
      let info2 = Provenance.of_range (Opt_range.mk 3 5) in
      let parsed =
        Lvca_parsing.Parser.(parse_string_or_failwith parse) "Foo()  // comment"
      in
      assert (parsed = Operator_def (info1, "Foo", Arity (info2, [])))
    ;;
  end)
;;

open Base
open Result.Let_syntax
open Lvca_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
integer : *

exp :=
  | Num(integer)
  | Zero()
  | Succ(exp)
  | Ifz(exp; exp. exp; exp)
  | Fun(exp. exp)
  | Ap(exp; exp)
  | Fix(exp. exp)
  ;
|}
, { integer = "Primitive.Integer" }]

module Exp = Nominal.Convertible.Extend (Lang.Exp)

(** Values are num and fun *)
let is_val = function Lang.Exp.Num _ | Fun _ -> true | _ -> false

let rec subst v name exp =
  match exp with
  | Lang.Exp.Num _ | Zero _ -> exp
  | Succ (info, exp) -> Succ (info, subst v name exp)
  | Ifz (info, e0, (x, e1), e) ->
    Ifz
      ( info
      , subst v name e0
      , (x, if String.(x.name = name) then e1 else subst v name e1)
      , subst v name e )
  | Fun (info, (x, e)) ->
    Fun (info, (x, if String.(x.name = name) then e else subst v name e))
  | Ap (info, e1, e2) -> Ap (info, subst v name e1, subst v name e2)
  | Fix (info, (x, e)) ->
    Fix (info, (x, if String.(x.name = name) then e else subst v name e))
  | Exp_var (_info, name') -> if String.(name = name') then v else exp
;;

let rec transition tm =
  match tm with
  | Lang.Exp.Zero info -> Ok (Lang.Exp.Num (info, (info, Z.of_int 0))) (* 22.4a *)
  | Succ (info1, d) ->
    (match d with
    | Lang.Exp.Num (info2, (_, d)) ->
      (* 22.4d *)
      let info = Provenance.calculated_here [%here] [ info1; info2 ] in
      Ok (Lang.Exp.Num (info, (info, Z.succ d)))
    | _ ->
      let%map d = transition d in
      let info = Provenance.calculated_here [%here] [ info1 ] in
      Lang.Exp.Succ (info, d))
  | Ifz (info1, d0, (x, d1), d) ->
    (* 22.4f-j *)
    (match d with
    | Lang.Exp.Num (info2, (_, d)) ->
      let info = Provenance.calculated_here [%here] [ info1; info2 ] in
      if Z.(equal zero d)
      then Ok d0 (* 22.4h *)
      else Ok (subst (Num (info, (info, Z.pred d))) x.name d1)
      (* 22.4i *)
    | _ ->
      let%map d = transition d in
      let info = Provenance.calculated_here [%here] [ info1 ] in
      Lang.Exp.Ifz (info, d0, (x, d1), d))
  | Ap (info, d1, d2) ->
    (* 22.4k-n *)
    (match d1 with
    | Fun (_, (x, d)) -> (* 22.4m *) Ok (subst d2 x.name d)
    | _ ->
      let%map d1 = transition d1 in
      Lang.Exp.Ap (info, d1, d2))
  | Fix (_info, (x, d)) -> Ok (subst tm x.name d) (* 22.4o *)
  | Lang.Exp.Num _ -> Ok tm
  | Fun _ | Exp_var _ -> Error ("stuck", tm)
;;

let rec eval tm =
  if is_val tm
  then Ok tm
  else (
    let%bind tm = transition tm in
    eval tm)
;;

let%test_module _ =
  (module struct
    let go str =
      match Lvca_parsing.(parse_string (whitespace *> Exp.parse)) str with
      | Error msg -> Fmt.pr "%s" msg
      | Ok tm ->
        (match eval tm with
        | Error (msg, tm) -> Fmt.pr "%s: %a" msg Exp.pp tm
        | Ok tm -> Exp.pp Fmt.stdout tm)
    ;;

    let%expect_test _ =
      go "Zero()";
      [%expect "Num(0)"]
    ;;

    let%expect_test _ =
      go "Succ(Succ(Zero()))";
      [%expect "Num(2)"]
    ;;

    let%expect_test _ =
      go "Ifz(Zero(); x. x; Zero())";
      [%expect "Num(0)"]
    ;;

    let%expect_test _ =
      go "Succ(Ifz(Zero(); x. x; Zero()))";
      [%expect "Num(1)"]
    ;;

    let%expect_test _ =
      go "Ap(Fun(x. x); Succ(Zero()))";
      [%expect "Num(1)"]
    ;;

    let add a b =
      Printf.sprintf
        {|
      Ap(
        Ap(
          Fun(x.
            Fix(p.
              Fun(y.
                Ifz(x; y'. Succ(Ap(p; y')); y)
              )
            )
          );
          %s
        );
        %s
      )
        |}
        a
        b
    ;;

    let%expect_test _ =
      go (add "Num(2)" "Num(2)");
      [%expect "Num(4)"]
    ;;

    let%expect_test _ =
      go (add (add "Num(1)" "Num(2)") (add "Num(3)" "Num(4)"));
      [%expect "Num(10)"]
    ;;
  end)
;;

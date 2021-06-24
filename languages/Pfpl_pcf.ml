open Base
open Result.Let_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
typ := Nat() | Parr(typ; typ)

exp :=
  | Zero()
  | Succ(exp)
  | Ifz(exp; exp. exp; exp)
  | Fun(typ; exp. exp)
  | Ap(exp; exp)
  | Fix(typ; exp. exp)
|}]

let rec is_val ~eager = function
  | Lang.Plain.Zero | Fun _ -> true
  | Succ e -> (not eager) || is_val ~eager e
  | _ -> false
;;

let is_val' ~eager tm = tm |> Lang.Exp.to_plain |> is_val ~eager

let rec subst v name exp =
  match exp with
  | Lang.Types.Zero _ -> exp
  | Succ (info, exp) -> Succ (info, subst v name exp)
  | Ifz (info, e0, (x, e1), e) ->
    Ifz
      ( info
      , subst v name e0
      , (x, if String.(x = name) then e1 else subst v name e1)
      , subst v name e )
  | Fun (info, typ, (x, e)) ->
    Fun (info, typ, (x, if String.(x = name) then e else subst v name e))
  | Ap (info, e1, e2) -> Ap (info, subst v name e1, subst v name e2)
  | Fix (info, typ, (x, e)) ->
    Fix (info, typ, (x, if String.(x = name) then e else subst v name e))
  | Exp_var (_info, name') -> if String.(name = name') then v else exp
;;

module Provenance = struct
  type 'info t =
    | Root of 'info
    | Derived of 'info t Lang.Exp.t

  let rec equal ~info_eq p1 p2 =
    match p1, p2 with
    | Root i1, Root i2 -> info_eq i1 i2
    | Derived e1, Derived e2 -> Lang.Exp.equal ~info_eq:(equal ~info_eq) e1 e2
    | _, _ -> false
  ;;

  let rec get_root_info = function
    | Root info -> info
    | Derived tm -> tm |> Lang.Exp.info |> get_root_info
  ;;
end

let rec transition ~eager tm =
  let info = Provenance.Derived tm in
  let set_info = Lang.Exp.map_info ~f:(fun _ -> info) in
  match tm with
  | Lang.Types.Zero _ | Fun _ | Exp_var _ -> Error ("stuck", tm)
  | Succ (_, tm) ->
    if eager
    then (
      let%map tm = transition ~eager tm in
      Lang.Types.Succ (info, tm))
    else Error ("stuck", tm)
  | Ifz (_, e0, (x, e1), e) ->
    if is_val' ~eager e
    then (
      match e with
      | Zero _ -> Ok (set_info e0)
      | Succ (_, e) -> Ok (subst e x e1 |> set_info)
      | _ -> Error ("expected either Zero or Succ(e) in the discriminee", tm))
    else (
      let%map e = transition ~eager e in
      Lang.Types.Ifz (info, e0, (x, e1), e))
  | Ap (_, Fun (_, _, (x, e)), e2) ->
    Ok (subst e2 x e |> set_info) (* TODO: check e2 is_val *)
  | Ap (_, e1, e2) ->
    let%map e1 = transition ~eager e1 in
    Lang.Types.Ap (info, e1, e2)
  | Fix (_, _typ, (x, e)) -> Ok (subst tm x e |> set_info)
;;

let eval ?(eager = true) ?(step_limit = 50) tm =
  let rec go ~step_limit steps tm =
    if Int.(step_limit = 0)
    then steps, Error ("ran out of steps", tm)
    else if is_val' ~eager tm
    then steps, Ok tm
    else (
      let steps' = tm :: steps in
      match transition ~eager tm with
      | Ok tm' -> go ~step_limit:(step_limit - 1) steps' tm'
      | Error (msg, tm) -> steps', Error (msg, tm))
  in
  go ~step_limit [] tm
;;

let%test_module _ =
  (module struct
    let go ?(eager = true) str =
      match Lvca_parsing.(parse_string (whitespace *> Lang.Exp.Parse.t) str) with
      | Error msg -> Fmt.pr "%s" msg
      | Ok tm ->
        let tm = Lang.Exp.map_info ~f:(fun info -> Provenance.Root info) tm in
        (match eval ~eager tm with
        | _, Error (msg, tm) -> Fmt.pr "%s: %a" msg Lang.Exp.pp tm
        | _, Ok tm -> Lang.Exp.pp Fmt.stdout tm)
    ;;

    let%expect_test _ =
      go "Zero()";
      [%expect "Zero()"]
    ;;

    let%expect_test _ =
      go "Succ(Zero())";
      [%expect "Succ(Zero())"]
    ;;

    let%expect_test _ =
      go "Ifz(Zero(); x. x; Zero())";
      [%expect "Zero()"]
    ;;

    let%expect_test _ =
      go "Succ(Ifz(Zero(); x. x; Zero()))";
      [%expect "Succ(Zero())"]
    ;;

    let%expect_test _ =
      go "Ap(Fun(Parr(Nat(); Nat()); x. x); Succ(Zero()))";
      [%expect "Succ(Zero())"]
    ;;

    let add a b =
      Printf.sprintf
        {|
      Ap(
        Ap(
          Fun(
            Parr(Nat(); Nat());
            x.
            Fix(
              Nat();
              p.
              Fun(
                Parr(Nat(); Nat());
                y.
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
      go (add "Succ(Zero())" "Succ(Zero())");
      [%expect "Succ(Succ(Zero()))"]
    ;;

    let%expect_test _ =
      go (add (add "Succ(Zero())" "Succ(Zero())") (add "Succ(Zero())" "Succ(Zero())"));
      [%expect "Succ(Succ(Succ(Succ(Zero()))))"]
    ;;
  end)
;;

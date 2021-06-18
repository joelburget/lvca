open Base
open Result.Let_syntax

module Lang =
[%lvca.abstract_syntax_module
{|
exp :=
  | Zero()
  | Succ(exp)
  | Ifz(exp; exp. exp; exp)
  | Fun(exp. exp)
  | Ap(exp; exp)
  | Fix(exp. exp)
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
  | Fun (info, (x, e)) -> Fun (info, (x, if String.(x = name) then e else subst v name e))
  | Ap (info, e1, e2) -> Ap (info, subst v name e1, subst v name e2)
  | Fix (info, (x, e)) -> Fix (info, (x, if String.(x = name) then e else subst v name e))
  | Exp_var (_info, name') -> if String.(name = name') then v else exp
;;

let rec eval_step ~eager tm =
  match tm with
  | Lang.Types.Zero _ -> Ok tm
  | Succ (info, tm) ->
    if eager
    then (
      let%map tm = eval_step ~eager tm in
      Lang.Types.Succ (info, tm))
    else Error ("stuck", tm)
  | Ifz (info, e0, (x, e1), e) ->
    if is_val' ~eager e
    then (
      match e with
      | Zero _ -> Ok e0
      | Succ (_, e) -> Ok (subst e x e1)
      | _ -> Error ("expected either Zero or Succ(e) in the discriminee", tm))
    else (
      let%map e = eval_step ~eager e in
      Lang.Types.Ifz (info, e0, (x, e1), e))
  | Ap (_info, Fun (_, (x, e)), e2) -> Ok (subst e2 x e) (* TODO: check e2 is_val *)
  | Ap (info, e1, e2) ->
    let%map e1 = eval_step ~eager e1 in
    Lang.Types.Ap (info, e1, e2)
  | Fix (_info, (x, e)) -> Ok (subst tm x e)
  | Fun _ -> Error ("unexpected evaluation request for plain function", tm)
  | Exp_var _ -> failwith "TODO: eval_step var"
;;

let rec eval ?(eager = true) ?(steps = 3) tm =
  if Int.(steps = 0)
  then Error ("ran out of steps", tm)
  else if is_val' ~eager tm
  then Ok tm
  else (
    let%bind tm' = eval_step ~eager tm in
    eval ~steps:(steps - 1) tm')
;;

let%test_module _ =
  (module struct
    let go ?(eager = true) str =
      match Lvca_parsing.parse_string Lang.Exp.Parse.t str with
      | Error msg -> Fmt.pr "%s" msg
      | Ok tm ->
        (match eval ~eager tm with
        | Error (msg, tm) -> Fmt.pr "%s: %a" msg Lang.Exp.pp tm
        | Ok tm -> Lang.Exp.pp Fmt.stdout tm)
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
      go "Ap(Fun(x. x); Succ(Zero()))";
      [%expect "Succ(Zero())"]
    ;;

    (*
    let%expect_test _ =
      go ~eager:true "Fix(x. Ifz(Zero(); pred. pred; x))";
      [%expect "Zero()"]
    ;;

    let%expect_test _ =
      go ~eager:false "Fix(x. Ifz(Zero(); pred. pred; x))";
      [%expect "Zero()"]
    ;;
    *)
  end)
;;

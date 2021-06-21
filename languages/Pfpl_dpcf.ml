open Base
open Result.Let_syntax

module Mk_lang =
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
|}]

module Lang = Mk_lang (Lvca_syntax.Primitive.Integer)

(** Values are num and fun *)
let is_val = function Lang.Types.Num _ | Fun _ -> true | _ -> false

let is_num = function Lang.Types.Num _ -> true | _ -> false
let is_fun = function Lang.Types.Fun _ -> true | _ -> false

(** A value that isn't a fun, ie a num *)
let isnt_fun = function Lang.Types.Num _ -> true | _ -> false

(** A value that isn't a num, ie a fun *)
let isnt_num = function Lang.Types.Fun _ -> true | _ -> false

let rec subst v name exp =
  match exp with
  | Lang.Types.Num _ | Zero _ -> exp
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

let rec transition tm =
  match tm with
  | Lang.Types.Zero info -> Ok (Lang.Types.Num (info, (info, Z.zero))) (* 22.4a *)
  | Succ (info, d) ->
    (match d with
    | Lang.Types.Num (_, (info, z)) ->
      (* 22.4d *)
      Ok (Lang.Types.Num (info, (info, Z.(z + one))))
    | _ ->
      (match transition d with
      (* 22.4b, c *)
      | Ok d -> Ok (Lang.Types.Succ (info, d))
      | other -> other))
  | Ifz (info, d0, (x, d1), d) ->
    (* 22.4f-j *)
    (match d with
    | Lang.Types.Num (_, (info, z)) ->
      if Z.(equal z zero)
      then Ok d0 (* 22.4h *)
      else Ok (subst (Num (info, (info, Z.pred z))) x d1) (* 22.4i *)
    | _ ->
      (match transition d with Ok d -> Ok (Ifz (info, d0, (x, d1), d)) | other -> other))
  | Ap (info, d1, d2) ->
    (* 22.4k-n *)
    (match d1 with
    | Fun (_, (x, d)) -> (* 22.4m *) Ok (subst d2 x d)
    | _ ->
      (match transition d1 with
      | Ok d1 -> Ok (Lang.Types.Ap (info, d1, d2))
      | other -> other))
  | Fix (_info, (x, d)) -> Ok (subst tm x d) (* 22.4o *)
  | Lang.Types.Num _ -> Ok tm
  | _ -> Error ("stuck", tm)

and err = function
  | Lang.Types.Succ (_, d) -> err d (* 22.4c *) || isnt_num d (* 22.4e *)
  | Ifz (_, _, _, d) -> isnt_num d (* 22.4j *) || err d (* 22.4g *)
  | Ap (_, d1, _d2) -> err d1 (* 22.4l *) || isnt_fun d1 (* 22.4n *)
  | _ -> failwith "TODO"
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
      match Lvca_parsing.parse_string Lang.Exp.Parse.t str with
      | Error msg -> Fmt.pr "%s" msg
      | Ok tm ->
        (match eval tm with
        | Error (msg, tm) -> Fmt.pr "%s: %a" msg Lang.Exp.pp tm
        | Ok tm -> Lang.Exp.pp Fmt.stdout tm)
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
  end)
;;

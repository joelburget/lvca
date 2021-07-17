open Base
open Lvca_syntax
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

module Exp = Nominal.Convertible.Extend (Lang.Exp)

let rec to_tex = function
  | Lang.Exp.Zero info ->
    Nominal.Term.Primitive (info, Primitive_impl.All_plain.String "Z")
  | Succ (info, exp) -> Nominal.Term.Operator (info, "S", [ Scope ([], to_tex exp) ])
  | _ -> failwith "TODO(to_tex)"
;;

module Doc = Jyp_pretty.Doc

let rec typ_to_pretty = function
  | Lang.Typ.Nat info -> Jyp_pretty.Lang.Doc.Text (info, (info, "nat"))
  | Parr (info, t1, t2) ->
    Cat (info, typ_to_pretty t1, Cat (info, Text (info, (info, "->")), typ_to_pretty t2))
;;

let cat ~info docs =
  match docs with
  | [] -> failwith "can't cat no docs"
  | [ doc ] -> doc
  | d :: ds ->
    List.fold_right ds ~init:d ~f:(fun d ds -> Jyp_pretty.Lang.Doc.Cat (info, d, ds))
;;

let text ~info str = Jyp_pretty.Lang.Doc.Text (info, (info, str))

let rec to_pretty = function
  | Lang.Exp.Zero info -> text ~info "Z"
  | Succ (info, exp) -> cat ~info [ text ~info "S"; to_pretty exp ]
  | Ifz (info, e0, (x, e1), e) ->
    cat
      ~info
      [ text ~info "ifz"
      ; to_pretty e
      ; text ~info "{"
      ; text ~info "z -> "
      ; to_pretty e0
      ; text ~info "|"
      ; text ~info "s("
      ; text ~info:x.info x.name
      ; text ~info ") -> "
      ; to_pretty e1
      ; text ~info "}"
      ]
  | Fun (info, t, (x, e)) ->
    cat
      ~info
      [ text ~info "lam"
      ; text ~info "{"
      ; typ_to_pretty t
      ; text ~info "}("
      ; text ~info:x.info x.name
      ; text ~info "."
      ; to_pretty e
      ; text ~info ")"
      ]
  | Ap (info, e1, e2) -> Cat (info, to_pretty e1, to_pretty e2)
  | Fix (info, t, (x, e)) ->
    cat
      ~info
      [ text ~info "fix"
      ; text ~info:x.info x.name
      ; text ~info ":"
      ; typ_to_pretty t
      ; text ~info "is"
      ; to_pretty e
      ]
  | Exp_var (info, name) -> text ~info name
;;

(* TODO:
  - syntax for substitution: [e/x]e1
  - should we have to antiquote e1 and e2 in Ap(e1; e2)?
  *)
let to_core =
  [%lvca.core
    {|
let rec eval = \(tm : exp) -> match tm with {
  | Zero() -> {Zero()}
  | Succ(exp) -> let exp = eval exp in Succ(exp)
  | Ifz(_ty; e0; x. e1; e) ->
    let e = eval e in
    match e with {
      | Zero() -> e
      | Succ(e) -> open e x e1
    }
  | Ap(Fun(x. e1); e2) ->
    let e2 = eval e2 in
    open e x e2
  | Ap(e1; e2) ->
    let e1 = eval e1 in
    eval {Ap(e1; e2)}
  | Fix(_typ; x. e) -> open e x tm
}
in eval
  |}]
;;

let rec is_val ~eager = function
  | Lang.Plain.Zero | Fun _ -> true
  | Succ e -> (not eager) || is_val ~eager e
  | _ -> false
;;

let is_val' ~eager tm = tm |> Lang.Exp.to_plain |> is_val ~eager

let rec subst v name exp =
  match exp with
  | Lang.Exp.Zero _ -> exp
  | Succ (info, exp) -> Succ (info, subst v name exp)
  | Ifz (info, e0, (x, e1), e) ->
    Ifz
      ( info
      , subst v name e0
      , (x, if String.(x.name = name) then e1 else subst v name e1)
      , subst v name e )
  | Fun (info, typ, (x, e)) ->
    Fun (info, typ, (x, if String.(x.name = name) then e else subst v name e))
  | Ap (info, e1, e2) -> Ap (info, subst v name e1, subst v name e2)
  | Fix (info, typ, (x, e)) ->
    Fix (info, typ, (x, if String.(x.name = name) then e else subst v name e))
  | Exp_var (_info, name') -> if String.(name = name') then v else exp
;;

module Provenance = struct
  type 'info t =
    | Root of 'info
    | Derived of 'info t Lang.Exp.t

  let rec equal ~info_eq p1 p2 =
    match p1, p2 with
    | Root i1, Root i2 -> info_eq i1 i2
    | Derived e1, Derived e2 -> Exp.equal ~info_eq:(equal ~info_eq) e1 e2
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
  | Lang.Exp.Zero _ | Fun _ | Exp_var _ -> Error ("stuck", tm)
  | Succ (_, tm) ->
    if eager
    then (
      let%map tm = transition ~eager tm in
      Lang.Exp.Succ (info, tm))
    else Error ("stuck", tm)
  | Ifz (_, e0, (x, e1), e) ->
    if is_val' ~eager e
    then (
      match e with
      | Zero _ -> Ok (set_info e0)
      | Succ (_, e) -> Ok (subst e x.name e1 |> set_info)
      | _ -> Error ("expected either Zero or Succ(e) in the discriminee", tm))
    else (
      let%map e = transition ~eager e in
      Lang.Exp.Ifz (info, e0, (x, e1), e))
  | Ap (_, Fun (_, _, (x, e)), e2) ->
    Ok (subst e2 x.name e |> set_info) (* TODO: check e2 is_val *)
  | Ap (_, e1, e2) ->
    let%map e1 = transition ~eager e1 in
    Lang.Exp.Ap (info, e1, e2)
  | Fix (_, _typ, (x, e)) -> Ok (subst tm x.name e |> set_info)
;;

let eval ?(eager = true) ?(step_limit = 50) tm =
  let steps = Queue.create () in
  let rec go ~step_limit tm =
    if Int.(step_limit = 0)
    then Error ("ran out of steps", tm)
    else if is_val' ~eager tm
    then Ok tm
    else (
      Queue.enqueue steps tm;
      match transition ~eager tm with
      | Ok tm' -> go ~step_limit:(step_limit - 1) tm'
      | Error (msg, tm) -> Error (msg, tm))
  in
  let result = go ~step_limit tm in
  Queue.to_list steps, result
;;

let%test_module _ =
  (module struct
    let go ?(eager = true) str =
      match
        Lvca_parsing.(parse_string (whitespace *> Exp.parse ~comment:c_comment) str)
      with
      | Error msg -> Fmt.pr "%s" msg
      | Ok tm ->
        let tm = Lang.Exp.map_info ~f:(fun info -> Provenance.Root info) tm in
        (match eval ~eager tm with
        | _, Error (msg, tm) -> Fmt.pr "%s: %a" msg Exp.pp tm
        | _, Ok tm -> Exp.pp Fmt.stdout tm)
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

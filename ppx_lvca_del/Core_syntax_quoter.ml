open Base
open Ppxlib
open Syntax_quoter
open Exp
open Lvca_del

module Binding_aware_pattern = struct
  let rec t ~loc = function
    | Lvca_syntax.Binding_aware_pattern.Operator (pos, name, scopes) ->
      let name_exp = string ~loc name in
      let scopes = scopes |> List.map ~f:(scope ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Binding_aware_pattern.Operator
          ([%e provenance ~loc pos], [%e name_exp], [%e scopes])]
    | Var (pos, str) ->
      [%expr
        Lvca_syntax.Binding_aware_pattern.Var
          ([%e provenance ~loc pos], [%e string ~loc str])]
    | Primitive p ->
      [%expr Lvca_syntax.Binding_aware_pattern.Primitive [%e Primitive.all ~loc p]]

  and scope ~loc (Lvca_syntax.Binding_aware_pattern.Scope (vars, body)) =
    let body = t ~loc body in
    let vars =
      vars
      |> List.map ~f:(fun (i, name) ->
             [%expr [%e provenance ~loc i], [%e string ~loc name]])
      |> list ~loc
    in
    [%expr Lvca_syntax.Binding_aware_pattern.Scope ([%e vars], [%e body])]
  ;;
end

module Core = struct
  let rec list ~loc = function
    | List_model.List.Nil i -> [%expr Lvca_del.List_model.List.Nil [%e provenance ~loc i]]
    | Cons (i, x, xs) ->
      [%expr
        Lvca_del.List_model.List.Cons ([%e provenance ~loc i], [%e x], [%e list ~loc xs])]
  ;;

  let option ~loc = function
    | Option_model.Option.None i ->
      [%expr Lvca_del.Option_model.Option.None [%e provenance ~loc i]]
    | Some (i, a) ->
      [%expr Lvca_del.Option_model.Option.Some ([%e provenance ~loc i], [%e a])]
  ;;

  module Binding_aware_pattern_model = struct
    let rec pattern ~loc = function
      | Binding_aware_pattern_model.Pattern.Operator (info, name, scopes) ->
        let name_exp = Primitive.string ~loc name in
        let scopes = scopes |> List_model.map ~f:(scope ~loc) |> list ~loc in
        [%expr
          Lvca_del.Binding_aware_pattern_model.Pattern.Operator
            ([%e provenance ~loc info], [%e name_exp], [%e scopes])]
      | Var (info, str) ->
        [%expr
          Lvca_del.Binding_aware_pattern_model.Pattern.Var
            ([%e provenance ~loc info], [%e Primitive.string ~loc str])]
      | Primitive (info, p) ->
        [%expr
          Lvca_del.Binding_aware_pattern_model.Pattern.Primitive
            ([%e provenance ~loc info], [%e Primitive.all ~loc p])]

    and scope ~loc (Binding_aware_pattern_model.Scope.Scope (info, vars, body)) =
      let body = pattern ~loc body in
      let vars = vars |> List_model.map ~f:(Primitive.string ~loc) |> list ~loc in
      [%expr
        Lvca_del.Binding_aware_pattern_model.Scope.Scope
          ([%e provenance ~loc info], [%e vars], [%e body])]
    ;;
  end

  module Sort_model = struct
    let rec sort ~loc = function
      | Sort_model.Kernel.Sort.Name (i, str) ->
        [%expr
          Lvca_del.Sort_model.Sort.Name
            ([%e provenance ~loc i], [%e Primitive.string ~loc str])]
      | Ap (i, str, lst) ->
        [%expr
          Lvca_del.Sort_model.Sort.Ap
            ([%e provenance ~loc i], [%e Primitive.string ~loc str], [%e ap_list ~loc lst])]

    and ap_list ~loc = function
      | Sort_model.Kernel.Ap_list.Nil i ->
        [%expr Lvca_del.Sort_model.Ap_list.Nil [%e provenance ~loc i]]
      | Cons (i, s, lst) ->
        [%expr
          Lvca_del.Sort_model.Ap_list.Cons
            ([%e provenance ~loc i], [%e sort ~loc s], [%e ap_list ~loc lst])]
    ;;
  end

  module Type = struct
    let rec t ~loc = function
      | Core.Type.Ty.Sort (info, s) ->
        [%expr Core.Type.Ty.Sort ([%e provenance ~loc info], [%e Sort_model.sort ~loc s])]
      | Arrow (info, t1, t2) ->
        [%expr
          Core.Type.Ty.Arrow ([%e provenance ~loc info], [%e t ~loc t1], [%e t ~loc t2])]
      | Forall (info, (binders, body)) ->
        [%expr
          Core.Type.Ty.Forall
            ([%e provenance ~loc info], ([%e pattern ~loc binders], [%e t ~loc body]))]
      | Ty_var (i, name) ->
        [%expr Core.Lang.Term.Ty_var ([%e provenance ~loc i], [%e string ~loc name])]
    ;;
  end

  let rec term ~loc = function
    | Core.Lang.Term.Nominal (i, tm) ->
      [%expr Core.Lang.Term.Nominal ([%e provenance ~loc i], [%e nominal ~loc tm])]
    | Ap (i, tm, tms) ->
      let tms = tms |> List_model.map ~f:(term ~loc) |> list ~loc in
      [%expr Core.Lang.Term.Ap ([%e provenance ~loc i], [%e term ~loc tm], [%e tms])]
    | Case (i, tm, scopes) ->
      let scopes = scopes |> List_model.map ~f:(case_scope ~loc) |> list ~loc in
      [%expr Core.Lang.Term.Case ([%e provenance ~loc i], [%e term ~loc tm], [%e scopes])]
    | Lambda (i, ty, (var, body)) ->
      [%expr
        Core.Lang.Term.Lambda
          ( [%e provenance ~loc i]
          , [%e Type.t ~loc ty]
          , ([%e single_var ~loc var], [%e term ~loc body]) )]
    | Let (i, tm, ty, (var, body)) ->
      let info = provenance ~loc i in
      let tm = term ~loc tm in
      let ty = ty |> Option_model.map ~f:(Type.t ~loc) |> option ~loc in
      let var = single_var ~loc var in
      let body = term ~loc body in
      [%expr Core.Lang.Term.Let ([%e info], [%e tm], [%e ty], ([%e var], [%e body]))]
    | Let_rec (i, rows, (binders, body)) ->
      let info = provenance ~loc i in
      let rows = rows |> List_model.map ~f:(letrec_row ~loc) |> list ~loc in
      let binders = pattern ~loc binders in
      let body = term ~loc body in
      [%expr Core.Lang.Term.Let ([%e info], [%e rows], ([%e binders], [%e body]))]
    | Subst (i, (var, body), arg) ->
      let info = provenance ~loc i in
      let arg = term ~loc arg in
      let var = single_var ~loc var in
      let body = term ~loc body in
      [%expr Core.Lang.Term.Subst ([%e info], ([%e var], [%e body]), [%e arg])]
    | Term_var (i, name) ->
      [%expr Core.Lang.Term.Term_var ([%e provenance ~loc i], [%e string ~loc name])]

  and case_scope ~loc (Core.Lang.Case_scope.Case_scope (info, pat, tm)) =
    [%expr
      Core.Lang.Case_scope.Case_scope
        ( [%e provenance ~loc info]
        , [%e Binding_aware_pattern_model.pattern ~loc pat]
        , [%e term ~loc tm] )]

  and letrec_row ~loc (Core.Lang.Letrec_row.Letrec_row (info, ty, tm)) =
    [%expr
      Core.Lang.Letrec_row.Letrec_row
        ([%e provenance ~loc info], [%e Type.t ~loc ty], [%e term ~loc tm])]
  ;;
end

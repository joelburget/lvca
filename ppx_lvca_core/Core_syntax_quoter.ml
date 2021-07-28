open Base
open Ppxlib
open Syntax_quoter
open Exp

module Binding_aware_pattern = struct
  let rec t ~loc = function
    | Lvca_syntax.Binding_aware_pattern.Operator (pos, name, scopes) ->
      let name_exp = str ~loc name in
      let scopes = scopes |> List.map ~f:(scope ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Binding_aware_pattern.Operator
          ([%e commented ~loc pos], [%e name_exp], [%e scopes])]
    | Var (pos, s) ->
      [%expr
        Lvca_syntax.Binding_aware_pattern.Var ([%e commented ~loc pos], [%e str ~loc s])]
    | Primitive p ->
      [%expr Lvca_syntax.Binding_aware_pattern.Primitive [%e Primitive.all ~loc p]]

  and scope ~loc (Lvca_syntax.Binding_aware_pattern.Scope (vars, body)) =
    let body = t ~loc body in
    let vars =
      vars
      |> List.map ~f:(fun (i, name) -> [%expr [%e commented ~loc i], [%e str ~loc name]])
      |> list ~loc
    in
    [%expr Lvca_syntax.Binding_aware_pattern.Scope ([%e vars], [%e body])]
  ;;
end

module Core = struct
  module Sort_model = struct
    let rec sort ~loc = function
      | Lvca_core.Sort_model.Sort.Name (i, str) ->
        [%expr
          Lvca_core.Sort_model.Sort.Name
            ([%e commented ~loc i], [%e Primitive.string ~loc str])]
      | Ap (i, str, lst) ->
        [%expr
          Lvca_core.Sort_model.Sort.Ap
            ([%e commented ~loc i], [%e Primitive.string ~loc str], [%e ap_list ~loc lst])]

    and ap_list ~loc = function
      | Lvca_core.Sort_model.Ap_list.Nil i ->
        [%expr Lvca_core.Sort_model.Ap_list.Nil [%e commented ~loc i]]
      | Cons (i, s, lst) ->
        [%expr
          Lvca_core.Sort_model.Ap_list.Cons
            ([%e commented ~loc i], [%e sort ~loc s], [%e ap_list ~loc lst])]
    ;;
  end

  module Type = struct
    let rec t ~loc = function
      | Lvca_core.Lang.Ty.Sort (info, s) ->
        [%expr
          Lvca_core.Lang.Ty.Sort ([%e commented ~loc info], [%e Sort_model.sort ~loc s])]
      | Arrow (info, t1, t2) ->
        [%expr
          Lvca_core.Lang.Ty.Arrow
            ([%e commented ~loc info], [%e t ~loc t1], [%e t ~loc t2])]
    ;;
  end

  let is_rec ~loc = function
    | Lvca_core.Lang.Is_rec.Rec i ->
      [%expr Lvca_core.Lang.Is_rec.Rec [%e commented ~loc i]]
    | No_rec i -> [%expr Lvca_core.Lang.Is_rec.No_rec [%e commented ~loc i]]
  ;;

  let rec term ~loc = function
    | Lvca_core.Types.Term tm -> [%expr Lvca_core.Types.Term [%e nominal ~loc tm]]
    | Core_app (i, tm, tms) ->
      let tms = tms |> List.map ~f:(term ~loc) |> list ~loc in
      [%expr
        Lvca_core.Types.Core_app ([%e commented ~loc i], [%e term ~loc tm], [%e tms])]
    | Case (i, tm, scopes) ->
      let scopes = scopes |> List.map ~f:(case_scope ~loc) |> list ~loc in
      [%expr Lvca_core.Types.Case ([%e commented ~loc i], [%e term ~loc tm], [%e scopes])]
    | Lambda (i, ty, scope) ->
      [%expr
        Lvca_core.Types.Lambda
          ([%e commented ~loc i], [%e Type.t ~loc ty], [%e core_scope ~loc scope])]
    | Let x -> [%expr Lvca_core.Types.Let [%e let_ ~loc x]]
    | Var (i, name) ->
      [%expr Lvca_core.Types.Var ([%e commented ~loc i], [%e str ~loc name])]

  and let_ ~loc x =
    [%expr
      Lvca_core.Types.
        { info = [%e commented ~loc x.Lvca_core.Types.info]
        ; is_rec = [%e is_rec ~loc x.is_rec]
        ; tm = [%e term ~loc x.tm]
        ; ty = [%e option ~loc Type.t x.ty]
        ; scope = [%e core_scope ~loc x.scope]
        }]

  and core_scope ~loc (Lvca_core.Types.Scope (name, tm)) =
    [%expr Lvca_core.Types.Scope ([%e str ~loc name], [%e term ~loc tm])]

  and case_scope ~loc (Lvca_core.Types.Case_scope (pat, tm)) =
    [%expr
      Lvca_core.Types.Case_scope ([%e Binding_aware_pattern.t ~loc pat], [%e term ~loc tm])]
  ;;
end

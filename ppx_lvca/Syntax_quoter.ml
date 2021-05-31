open Base
open Lvca_provenance
open Lvca_syntax
open Abstract_syntax
open Ppxlib

module Pat = struct
  let rec list ~loc = function
    | [] -> [%pat? []]
    | expr :: exprs -> [%pat? [%p expr] :: [%p list ~loc exprs]]
  ;;
end

module Exp = struct
  let rec list ~loc = function
    | [] -> [%expr []]
    | expr :: exprs -> [%expr [%e expr] :: [%e list ~loc exprs]]
  ;;

  let str ~loc str = Ast_builder.Default.estring ~loc str
  let int ~loc i = Ast_builder.Default.eint ~loc i
  let int32 ~loc i = Ast_builder.Default.eint32 ~loc i
  let float ~loc f = Ast_builder.Default.efloat ~loc (Float.to_string f)
  let char ~loc c = Ast_builder.Default.echar ~loc c
  let bigint ~loc i = [%expr Z.of_string [%e str ~loc (Z.to_string i)]]

  let opt_range ~loc = function
    | None -> [%expr None]
    | Some Range.{ start; finish } ->
      let start = int ~loc start in
      let finish = int ~loc finish in
      [%expr Some Lvca_provenance.Range.{ start = [%e start]; finish = [%e finish] }]
  ;;

  let prim ~loc (pos, prim) =
    let pos = opt_range ~loc pos in
    match prim with
    | Primitive_impl.Plain.Integer i ->
      [%expr [%e pos], Lvca_syntax.Primitive_impl.Plain.Integer [%e bigint ~loc i]]
    | Primitive_impl.Plain.Int32 i ->
      [%expr [%e pos], Lvca_syntax.Primitive_impl.Plain.Int32 [%e int32 ~loc i]]
    | String s ->
      [%expr [%e pos], Lvca_syntax.Primitive_impl.Plain.String [%e str ~loc s]]
    | Float f ->
      [%expr [%e pos], Lvca_syntax.Primitive_impl.Plain.Float [%e float ~loc f]]
    | Char c -> [%expr [%e pos], Lvca_syntax.Primitive_impl.Plain.Char [%e char ~loc c]]
  ;;

  let rec pattern ~loc = function
    | Pattern.Operator (pos, name, pats) ->
      let name_exp = str ~loc name in
      let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Pattern.Operator ([%e opt_range ~loc pos], [%e name_exp], [%e pats])]
    | Var (pos, s) ->
      [%expr Lvca_syntax.Pattern.Var ([%e opt_range ~loc pos], [%e str ~loc s])]
    | Ignored (pos, s) ->
      [%expr Lvca_syntax.Pattern.Ignored ([%e opt_range ~loc pos], [%e str ~loc s])]
    | Primitive p -> [%expr Lvca_syntax.Pattern.Primitive [%e prim ~loc p]]
  ;;

  let rec nominal ~loc = function
    | Nominal.Term.Operator (pos, name, scopes) ->
      let name_exp = str ~loc name in
      let scopes = scopes |> List.map ~f:(scope ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Nominal.Term.Operator
          ([%e opt_range ~loc pos], [%e name_exp], [%e scopes])]
    | Var (pos, s) ->
      [%expr Lvca_syntax.Nominal.Term.Var ([%e opt_range ~loc pos], [%e str ~loc s])]
    | Primitive p -> [%expr Lvca_syntax.Nominal.Term.Primitive [%e prim ~loc p]]

  and scope ~loc (Nominal.Scope.Scope (pats, tm)) =
    let tm = nominal ~loc tm in
    let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
    [%expr Lvca_syntax.Nominal.Scope.Scope ([%e pats], [%e tm])]
  ;;

  let rec nonbinding ~loc = function
    | Nonbinding.Operator (pos, name, tms) ->
      let name_exp = str ~loc name in
      let tms = tms |> List.map ~f:(nonbinding ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Nonbinding.Operator ([%e opt_range ~loc pos], [%e name_exp], [%e tms])]
    | Primitive p -> [%expr Lvca_syntax.Nonbinding.Primitive [%e prim ~loc p]]
  ;;

  let rec sort ~loc = function
    | Sort.Ap (pos, name, sorts) ->
      let sorts = sorts |> List.map ~f:(sort ~loc) |> list ~loc in
      [%expr
        Lvca_syntax.Sort.Ap ([%e opt_range ~loc pos], [%e str ~loc name], [%e sorts])]
    | Sort.Name (pos, name) ->
      [%expr Lvca_syntax.Sort.Name ([%e opt_range ~loc pos], [%e str ~loc name])]
  ;;

  let sort_slot ~loc = function
    | Sort_slot.Sort_binding s ->
      [%expr Lvca_syntax.Abstract_syntax.Sort_slot.Sort_binding [%e sort ~loc s]]
    | Sort_pattern { pattern_sort; var_sort } ->
      [%expr
        Lvca_syntax.Abstract_syntax.Sort_slot.Sort_pattern
          { pattern_sort = [%e sort ~loc pattern_sort]
          ; var_sort = [%e sort ~loc var_sort]
          }]
  ;;

  let valence ~loc (Valence.Valence (sort_slots, body_sort)) =
    let sort_slots = sort_slots |> List.map ~f:(sort_slot ~loc) |> list ~loc in
    let body_sort = sort ~loc body_sort in
    [%expr Lvca_syntax.Abstract_syntax.Valence.Valence ([%e sort_slots], [%e body_sort])]
  ;;

  let arity ~loc valences = valences |> List.map ~f:(valence ~loc) |> list ~loc

  let operator_def ~loc (Operator_def.Operator_def (name, arity')) =
    [%expr
      Lvca_syntax.Abstract_syntax.Operator_def.Operator_def
        ([%e str ~loc name], [%e arity ~loc arity'])]
  ;;

  let kind ~loc (Kind.Kind (pos, n)) =
    [%expr
      Lvca_syntax.Abstract_syntax.Kind.Kind ([%e opt_range ~loc pos], [%e int ~loc n])]
  ;;

  let option ~loc maker = function None -> [%expr None] | Some x -> maker ~loc x

  let sort_def ~loc (Sort_def.Sort_def (vars, op_defs)) =
    let f (name, kind_opt) = [%expr [%e str ~loc name], [%e option ~loc kind kind_opt]] in
    let vars = vars |> List.map ~f |> list ~loc in
    let op_defs = op_defs |> List.map ~f:(operator_def ~loc) |> list ~loc in
    [%expr Lvca_syntax.Abstract_syntax.Sort_def.Sort_def ([%e vars], [%e op_defs])]
  ;;

  let language ~loc { externals; sort_defs } =
    let externals =
      externals
      |> List.map ~f:(fun (name, kind') ->
             [%expr [%e str ~loc name], [%e kind ~loc kind']])
      |> list ~loc
    in
    let sort_defs =
      sort_defs
      |> List.map ~f:(fun (name, sort_def') ->
             [%expr [%e str ~loc name], [%e sort_def ~loc sort_def']])
      |> list ~loc
    in
    [%expr
      Lvca_syntax.Abstract_syntax.
        { externals = [%e externals]; sort_defs = [%e sort_defs] }]
  ;;

  module Binding_aware_pattern = struct
    let rec t ~loc = function
      | Lvca_syntax.Binding_aware_pattern.Operator (pos, name, scopes) ->
        let name_exp = str ~loc name in
        let scopes = scopes |> List.map ~f:(scope ~loc) |> list ~loc in
        [%expr
          Lvca_syntax.Binding_aware_pattern.Operator
            ([%e opt_range ~loc pos], [%e name_exp], [%e scopes])]
      | Var (pos, s) ->
        [%expr
          Lvca_syntax.Binding_aware_pattern.Var ([%e opt_range ~loc pos], [%e str ~loc s])]
      | Ignored (pos, s) ->
        [%expr
          Lvca_syntax.Binding_aware_pattern.Ignored
            ([%e opt_range ~loc pos], [%e str ~loc s])]
      | Primitive p ->
        [%expr Lvca_syntax.Binding_aware_pattern.Primitive [%e prim ~loc p]]

    and scope ~loc (Lvca_syntax.Binding_aware_pattern.Scope (vars, body)) =
      let body = t ~loc body in
      let vars =
        vars
        |> List.map ~f:(fun (info, name) ->
               [%expr [%e opt_range ~loc info], [%e str ~loc name]])
        |> list ~loc
      in
      [%expr Lvca_syntax.Binding_aware_pattern.Scope ([%e vars], [%e body])]
    ;;
  end

  module Core = struct
    module Type = struct
      let rec t ~loc = function
        | Lvca_core.Type.Sort s -> [%expr Lvca_core.Type.Sort [%e sort ~loc s]]
        | Arrow ts ->
          let ts' = ts |> List.map ~f:(t ~loc) |> list ~loc in
          [%expr Lvca_core.Type.Arrow [%e ts']]
      ;;
    end

    let is_rec ~loc = function
      | Lvca_core.Is_rec.Rec -> [%expr Lvca_core.Is_rec.Rec]
      | No_rec -> [%expr Lvca_core.Is_rec.No_rec]
    ;;

    let rec term ~loc = function
      | Lvca_core.Types.Term tm -> [%expr Lvca_core.Types.Term [%e nominal ~loc tm]]
      | Core_app (info, tm, tms) ->
        let tms = tms |> List.map ~f:(term ~loc) |> list ~loc in
        [%expr
          Lvca_core.Types.Core_app ([%e opt_range ~loc info], [%e term ~loc tm], [%e tms])]
      | Case (info, tm, scopes) ->
        let scopes = scopes |> List.map ~f:(case_scope ~loc) |> list ~loc in
        [%expr
          Lvca_core.Types.Case ([%e opt_range ~loc info], [%e term ~loc tm], [%e scopes])]
      | Lambda (info, ty, scope) ->
        [%expr
          Lvca_core.Types.Lambda
            ([%e opt_range ~loc info], [%e Type.t ~loc ty], [%e core_scope ~loc scope])]
      | Let x -> [%expr Lvca_core.Types.Let [%e let_ ~loc x]]
      | Var (info, name) ->
        [%expr Lvca_core.Types.Var ([%e opt_range ~loc info], [%e str ~loc name])]

    and let_ ~loc x =
      [%expr
        Lvca_core.Types.
          { info = [%e opt_range ~loc x.Lvca_core.Types.info]
          ; is_rec = [%e is_rec ~loc x.is_rec]
          ; tm = [%e term ~loc x.tm]
          ; ty = [%e option ~loc Type.t x.ty]
          ; scope = [%e core_scope ~loc x.scope]
          }]

    and core_scope ~loc (Lvca_core.Types.Scope (name, tm)) =
      [%expr Lvca_core.Types.Scope ([%e str ~loc name], [%e term ~loc tm])]

    and case_scope ~loc (Lvca_core.Types.Case_scope (pat, tm)) =
      [%expr
        Lvca_core.Types.Case_scope
          ([%e Binding_aware_pattern.t ~loc pat], [%e term ~loc tm])]
    ;;
  end
end

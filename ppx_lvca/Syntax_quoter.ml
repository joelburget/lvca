open Base
open Lvca_provenance
open Lvca_syntax
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

  let prim ~loc = function
    | pos, Primitive_impl.Plain.Integer i ->
      [%expr
        [%e opt_range ~loc pos]
        , Lvca_syntax.Primitive_impl.Plain.Integer [%e bigint ~loc i]]
    | pos, Primitive_impl.Plain.Int32 i ->
      [%expr
        [%e opt_range ~loc pos], Lvca_syntax.Primitive_impl.Plain.Int32 [%e int32 ~loc i]]
    | pos, String s ->
      [%expr
        [%e opt_range ~loc pos], Lvca_syntax.Primitive_impl.Plain.String [%e str ~loc s]]
    | pos, Float f ->
      [%expr
        [%e opt_range ~loc pos], Lvca_syntax.Primitive_impl.Plain.Float [%e float ~loc f]]
    | pos, Char c ->
      [%expr
        [%e opt_range ~loc pos], Lvca_syntax.Primitive_impl.Plain.Char [%e char ~loc c]]
  ;;

  let rec pattern ~loc = function
    | Pattern.Operator (pos, name, pats) ->
      let name_exp = str ~loc name in
      let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
      [%expr Pattern.Operator ([%e opt_range ~loc pos], [%e name_exp], [%e pats])]
    | Var (pos, s) -> [%expr Pattern.Var ([%e opt_range ~loc pos], [%e str ~loc s])]
    | Ignored (pos, s) ->
      [%expr Pattern.Ignored ([%e opt_range ~loc pos], [%e str ~loc s])]
    | Primitive p -> [%expr Pattern.Primitive [%e prim ~loc p]]
  ;;

  let rec nominal ~loc = function
    | Nominal.Term.Operator (pos, name, scopes) ->
      let name_exp = str ~loc name in
      let scopes = scopes |> List.map ~f:(scope ~loc) |> list ~loc in
      [%expr Nominal.Term.Operator ([%e opt_range ~loc pos], [%e name_exp], [%e scopes])]
    | Var (pos, s) -> [%expr Nominal.Term.Var ([%e opt_range ~loc pos], [%e str ~loc s])]
    | Primitive p -> [%expr Nominal.Term.Primitive [%e prim ~loc p]]

  and scope ~loc (Nominal.Scope.Scope (pats, tm)) =
    let tm = nominal ~loc tm in
    let pats = pats |> List.map ~f:(pattern ~loc) |> list ~loc in
    [%expr Nominal.Scope.Scope ([%e pats], [%e tm])]
  ;;

  let rec nonbinding ~loc = function
    | Nonbinding.Operator (pos, name, tms) ->
      let name_exp = str ~loc name in
      let tms = tms |> List.map ~f:(nonbinding ~loc) |> list ~loc in
      [%expr Nonbinding.Operator ([%e opt_range ~loc pos], [%e name_exp], [%e tms])]
    | Primitive p -> [%expr Nonbinding.Primitive [%e prim ~loc p]]
  ;;

  let rec sort ~loc = function
    | Sort.Ap (pos, name, sorts) ->
      let sorts = sorts |> List.map ~f:(sort ~loc) |> list ~loc in
      [%expr Sort.Ap ([%e opt_range ~loc pos], [%e str ~loc name], [%e sorts])]
    | Sort.Name (pos, name) ->
      [%expr Sort.Name ([%e opt_range ~loc pos], [%e str ~loc name])]
  ;;

  let sort_slot ~loc = function
    | Abstract_syntax.Sort_slot.Sort_binding s ->
      [%expr Abstract_syntax.Sort_slot.Sort_binding [%e sort ~loc s]]
    | Sort_pattern { pattern_sort; var_sort } ->
      [%expr
        Abstract_syntax.Sort_slot.Sort_pattern
          { pattern_sort = [%e sort ~loc pattern_sort]
          ; var_sort = [%e sort ~loc var_sort]
          }]
  ;;

  let valence ~loc (Abstract_syntax.Valence.Valence (sort_slots, body_sort)) =
    let sort_slots = sort_slots |> List.map ~f:(sort_slot ~loc) |> list ~loc in
    let body_sort = sort ~loc body_sort in
    [%expr Abstract_syntax.Valence.Valence ([%e sort_slots], [%e body_sort])]
  ;;

  let arity ~loc valences = valences |> List.map ~f:(valence ~loc) |> list ~loc

  let operator_def ~loc (Abstract_syntax.Operator_def.Operator_def (name, arity')) =
    [%expr
      Abstract_syntax.Operator_def.Operator_def
        ([%e str ~loc name], [%e arity ~loc arity'])]
  ;;

  let kind ~loc (Abstract_syntax.Kind.Kind (pos, n)) =
    [%expr Abstract_syntax.Kind.Kind ([%e opt_range ~loc pos], [%e int ~loc n])]
  ;;

  let option ~loc maker = function None -> [%expr None] | Some x -> maker ~loc x

  let sort_def ~loc (Abstract_syntax.Sort_def.Sort_def (vars, op_defs)) =
    let f (name, kind_opt) = [%expr [%e str ~loc name], [%e option ~loc kind kind_opt]] in
    let vars = vars |> List.map ~f |> list ~loc in
    let op_defs = op_defs |> List.map ~f:(operator_def ~loc) |> list ~loc in
    [%expr Abstract_syntax.Sort_def.Sort_def ([%e vars], [%e op_defs])]
  ;;

  let language ~loc Abstract_syntax.{ externals; sort_defs } =
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
    [%expr Abstract_syntax.{ externals = [%e externals]; sort_defs = [%e sort_defs] }]
  ;;
end

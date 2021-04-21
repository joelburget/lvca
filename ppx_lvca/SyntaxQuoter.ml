open Base
open Lvca_syntax
open Ppxlib
module ParsePrimitive = Lvca_syntax.Primitive.Parse (ParseUtil.CComment)
module ParsePattern = Lvca_syntax.Pattern.Parse (ParseUtil.CComment)
module ParseTerm = Nominal.Term.Parse (ParseUtil.CComment)
module ParseNonbinding = NonBinding.Parse (ParseUtil.CComment)
module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

let rec mk_list ~loc = function
  | [] -> [%expr []]
  | expr :: exprs -> [%expr [%e expr] :: [%e mk_list ~loc exprs]]
;;

let mk_str ~loc str = Ast_builder.Default.estring ~loc str
let mk_int ~loc i = Ast_builder.Default.eint ~loc i
let mk_float ~loc f = Ast_builder.Default.efloat ~loc (Float.to_string f)
let mk_char ~loc c = Ast_builder.Default.echar ~loc c
let mk_bigint ~loc i = [%expr Z.of_string [%e mk_str ~loc (Z.to_string i)]]

let mk_pos ~loc = function
  | None -> [%expr None]
  | Some Range.{ start; finish } ->
    let start = mk_int ~loc start in
    let finish = mk_int ~loc finish in
    [%expr Some Range.{ start = [%e start]; finish = [%e finish] }]
;;

let mk_prim ~loc = function
  | pos, Lvca_syntax.Primitive.Plain.Integer i ->
    [%expr
      [%e mk_pos ~loc pos], Lvca_syntax.Primitive.Plain.Integer [%e mk_bigint ~loc i]]
  | pos, String s ->
    [%expr [%e mk_pos ~loc pos], Lvca_syntax.Primitive.Plain.String [%e mk_str ~loc s]]
  | pos, Float f ->
    [%expr [%e mk_pos ~loc pos], Lvca_syntax.Primitive.Plain.Float [%e mk_float ~loc f]]
  | pos, Char c ->
    [%expr [%e mk_pos ~loc pos], Lvca_syntax.Primitive.Plain.Char [%e mk_char ~loc c]]
;;

let rec mk_pattern ~loc = function
  | Pattern.Operator (pos, name, pats) ->
    let name_exp = mk_str ~loc name in
    let pats = pats |> List.map ~f:(mk_pattern ~loc) |> mk_list ~loc in
    [%expr Pattern.Operator ([%e mk_pos ~loc pos], [%e name_exp], [%e pats])]
  | Var (pos, str) -> [%expr Pattern.Var ([%e mk_pos ~loc pos], [%e mk_str ~loc str])]
  | Ignored (pos, str) ->
    [%expr Pattern.Ignored ([%e mk_pos ~loc pos], [%e mk_str ~loc str])]
  | Primitive prim -> [%expr Pattern.Primitive [%e mk_prim ~loc prim]]
;;

let rec mk_nominal ~loc = function
  | Nominal.Term.Operator (pos, name, scopes) ->
    let name_exp = mk_str ~loc name in
    let scopes = scopes |> List.map ~f:(mk_scope ~loc) |> mk_list ~loc in
    [%expr Nominal.Term.Operator ([%e mk_pos ~loc pos], [%e name_exp], [%e scopes])]
  | Var (pos, str) ->
    [%expr Nominal.Term.Var ([%e mk_pos ~loc pos], [%e mk_str ~loc str])]
  | Primitive prim -> [%expr Nominal.Term.Primitive [%e mk_prim ~loc prim]]

and mk_scope ~loc (Nominal.Scope.Scope (pats, tm)) =
  let tm = mk_nominal ~loc tm in
  let pats = pats |> List.map ~f:(mk_pattern ~loc) |> mk_list ~loc in
  [%expr Nominal.Scope.Scope ([%e pats], [%e tm])]
;;

let rec mk_nonbinding ~loc = function
  | NonBinding.Operator (pos, name, tms) ->
    let name_exp = mk_str ~loc name in
    let tms = tms |> List.map ~f:(mk_nonbinding ~loc) |> mk_list ~loc in
    [%expr NonBinding.Operator ([%e mk_pos ~loc pos], [%e name_exp], [%e tms])]
  | Primitive prim -> [%expr NonBinding.Primitive [%e mk_prim ~loc prim]]
;;

let rec mk_sort ~loc = function
  | Sort.Ap (pos, name, sorts) ->
    let sorts = sorts |> List.map ~f:(mk_sort ~loc) |> mk_list ~loc in
    [%expr Sort.Ap ([%e mk_pos ~loc pos], [%e mk_str ~loc name], [%e sorts])]
  | Sort.Name (pos, name) ->
    [%expr Sort.Name ([%e mk_pos ~loc pos], [%e mk_str ~loc name])]
;;

let mk_sort_slot ~loc = function
  | AbstractSyntax.SortSlot.SortBinding s ->
    [%expr AbstractSyntax.SortSlot.SortBinding [%e mk_sort ~loc s]]
  | SortPattern { pattern_sort; var_sort } ->
    [%expr
      AbstractSyntax.SortSlot.SortPattern
        { pattern_sort = [%e mk_sort ~loc pattern_sort]
        ; var_sort = [%e mk_sort ~loc var_sort]
        }]
;;

let mk_valence ~loc (AbstractSyntax.Valence.Valence (sort_slots, body_sort)) =
  let sort_slots = sort_slots |> List.map ~f:(mk_sort_slot ~loc) |> mk_list ~loc in
  let body_sort = mk_sort ~loc body_sort in
  [%expr AbstractSyntax.Valence.Valence ([%e sort_slots], [%e body_sort])]
;;

let mk_arity ~loc valences = valences |> List.map ~f:(mk_valence ~loc) |> mk_list ~loc

let mk_operator_def ~loc (AbstractSyntax.OperatorDef.OperatorDef (name, arity)) =
  [%expr
    AbstractSyntax.OperatorDef.OperatorDef
      ([%e mk_str ~loc name], [%e mk_arity ~loc arity])]
;;

let mk_kind ~loc (AbstractSyntax.Kind.Kind (pos, n)) =
  [%expr AbstractSyntax.Kind.Kind ([%e mk_pos ~loc pos], [%e mk_int ~loc n])]
;;

let mk_option ~loc maker = function None -> [%expr None] | Some x -> maker ~loc x

let mk_sort_def ~loc (AbstractSyntax.SortDef.SortDef (vars, op_defs)) =
  let f (name, kind_opt) =
    [%expr [%e mk_str ~loc name], [%e mk_option ~loc mk_kind kind_opt]]
  in
  let vars = vars |> List.map ~f |> mk_list ~loc in
  let op_defs = op_defs |> List.map ~f:(mk_operator_def ~loc) |> mk_list ~loc in
  [%expr AbstractSyntax.SortDef.SortDef ([%e vars], [%e op_defs])]
;;

let mk_language ~loc AbstractSyntax.{ externals; sort_defs } =
  let externals =
    externals
    |> List.map ~f:(fun (name, kind) ->
           [%expr [%e mk_str ~loc name], [%e mk_kind ~loc kind]])
    |> mk_list ~loc
  in
  let sort_defs =
    sort_defs
    |> List.map ~f:(fun (name, sort_def) ->
           [%expr [%e mk_str ~loc name], [%e mk_sort_def ~loc sort_def]])
    |> mk_list ~loc
  in
  [%expr AbstractSyntax.{ externals = [%e externals]; sort_defs = [%e sort_defs] }]
;;

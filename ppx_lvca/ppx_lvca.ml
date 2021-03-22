open Base
open Lvca_syntax
open Ppxlib
module ParsePrimitive = Lvca_syntax.Primitive.Parse (ParseUtil.CComment)
module ParsePattern = Lvca_syntax.Pattern.Parse (ParseUtil.CComment)
module ParseTerm = Nominal.Parse (ParseUtil.CComment)
module ParseNonbinding = NonBinding.Parse (ParseUtil.CComment)
module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

(* TODO: parser, core, nonbinding / OCaml data mapping *)

let extract_string loc expr =
  (* payload and location of the string contents, inside "" or {||} *)
  let adjust shift loc =
    let adjust shift p = { p with Lexing.pos_cnum = p.pos_cnum + shift } in
    { loc with
      Location.loc_start = adjust shift loc.loc_start
    ; Location.loc_end = adjust (-shift) loc.loc_end
    }
  in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (str, _loc, None)) -> str, adjust 1 expr.pexp_loc
  | Pexp_constant (Pconst_string (str, _loc, Some x)) ->
    str, adjust (String.length x + 2) expr.pexp_loc
  | _ -> Location.raise_errorf ~loc "Expecting string payload"
;;

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
  | Lvca_syntax.Primitive.PrimInteger i ->
    [%expr Lvca_syntax.Primitive.PrimInteger [%e mk_bigint ~loc i]]
  | PrimString s -> [%expr Lvca_syntax.Primitive.PrimString [%e mk_str ~loc s]]
  | PrimFloat f -> [%expr Lvca_syntax.Primitive.PrimFloat [%e mk_float ~loc f]]
  | PrimChar c -> [%expr Lvca_syntax.Primitive.PrimChar [%e mk_char ~loc c]]
;;

let rec mk_pattern ~loc = function
  | Pattern.Operator (pos, name, pats) ->
    let name_exp = mk_str ~loc name in
    let pats = pats |> List.map ~f:(mk_pattern ~loc) |> mk_list ~loc in
    [%expr Pattern.Operator ([%e mk_pos ~loc pos], [%e name_exp], [%e pats])]
  | Var (pos, str) -> [%expr Pattern.Var ([%e mk_pos ~loc pos], [%e mk_str ~loc str])]
  | Ignored (pos, str) ->
    [%expr Pattern.Ignored ([%e mk_pos ~loc pos], [%e mk_str ~loc str])]
  | Primitive (pos, prim) ->
    [%expr Pattern.Primitive ([%e mk_pos ~loc pos], [%e mk_prim ~loc prim])]
;;

let rec mk_nominal ~loc = function
  | Nominal.Operator (pos, name, scopes) ->
    let name_exp = mk_str ~loc name in
    let scopes = scopes |> List.map ~f:(mk_scope ~loc) |> mk_list ~loc in
    [%expr Nominal.Operator ([%e mk_pos ~loc pos], [%e name_exp], [%e scopes])]
  | Var (pos, str) -> [%expr Nominal.Var ([%e mk_pos ~loc pos], [%e mk_str ~loc str])]
  | Primitive (pos, prim) ->
    [%expr Nominal.Primitive ([%e mk_pos ~loc pos], [%e mk_prim ~loc prim])]

and mk_scope ~loc (Nominal.Scope (pats, tm)) =
  let tm = mk_nominal ~loc tm in
  let pats = pats |> List.map ~f:(mk_pattern ~loc) |> mk_list ~loc in
  [%expr Nominal.Scope ([%e pats], [%e tm])]
;;

let rec mk_nonbinding ~loc = function
  | NonBinding.Operator (pos, name, tms) ->
    let name_exp = mk_str ~loc name in
    let tms = tms |> List.map ~f:(mk_nonbinding ~loc) |> mk_list ~loc in
    [%expr NonBinding.Operator ([%e mk_pos ~loc pos], [%e name_exp], [%e tms])]
  | Primitive (pos, prim) ->
    [%expr NonBinding.Primitive ([%e mk_pos ~loc pos], [%e mk_prim ~loc prim])]
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

let rec ptyp_of_sort ~loc var_set = function
  | Sort.Name (_, name) ->
    if Set.mem var_set name
    then Ptyp_var name
    else Ptyp_constr ({ txt = Lident name; loc }, [])
  | Ap (_, name, args) ->
    Ptyp_constr
      ( { txt = Lident name; loc }
      , args
        |> List.map ~f:(fun sort ->
               { ptyp_desc = ptyp_of_sort ~loc var_set sort
               ; ptyp_loc = loc
               ; ptyp_loc_stack = []
               ; ptyp_attributes = []
               }) )
;;

let mk_type ~loc ptyp_desc =
  { ptyp_desc; ptyp_loc = loc; ptyp_loc_stack = []; ptyp_attributes = [] }
;;

let mk_var_type ~loc name = mk_type ~loc (Ptyp_var name)

let core_type_of_valence
    ~loc
    var_set
    (AbstractSyntax.Valence.Valence (binding_sort_slots, body_sort))
  =
  let body_type = ptyp_of_sort ~loc var_set body_sort in
  let ptyp_desc =
    match binding_sort_slots with
    | [] -> body_type
    | slots ->
      let var_types = [ mk_var_type ~loc "info" ] in
      let binding_types =
        slots
        |> List.map ~f:(fun slot ->
               let ty =
                 match slot with
                 | AbstractSyntax.SortSlot.SortBinding _sort ->
                   Ptyp_constr ({ txt = Lident "string"; loc }, [])
                 | SortPattern _sort ->
                   Ptyp_constr ({ txt = Ldot (Lident "Pattern", "t"); loc }, var_types)
               in
               mk_type ~loc ty)
      in
      let body_type = mk_type ~loc body_type in
      Ptyp_tuple (binding_types @ [ body_type ])
  in
  mk_type ~loc ptyp_desc
;;

(* TODO: more sophisticated rule? *)
let ctor_name = String.capitalize

let mk_language_module ~loc AbstractSyntax.{ externals = _; sort_defs } =
  let structure_items =
    sort_defs
    |> List.map ~f:(fun (name, AbstractSyntax.SortDef.SortDef (vars, op_defs)) ->
           let vars = vars |> List.map ~f:Lvca_util.Tuple2.get1 in
           let var_set = vars |> Lvca_util.String.Set.of_list in
           let ctor_decls =
             op_defs
             |> List.map
                  ~f:(fun (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity)) ->
                    let args = arity |> List.map ~f:(core_type_of_valence ~loc var_set) in
                    { pcd_name = { txt = ctor_name op_name; loc }
                    ; pcd_args = Pcstr_tuple args
                    ; pcd_res = None (* Is this only for GADTs? *)
                    ; pcd_loc = loc
                    ; pcd_attributes = []
                    })
           in
           let ptype_params =
             "info" :: vars
             |> List.map ~f:(fun var_name ->
                    let core_type = mk_var_type ~loc var_name in
                    core_type, Invariant
                    (* Covariant? *))
           in
           let type_decl =
             { ptype_name = { txt = name; loc }
             ; ptype_params
             ; ptype_cstrs = [] (* we never use constraints *)
             ; ptype_kind = Ptype_variant ctor_decls
             ; ptype_private = Public
             ; ptype_manifest = None
             ; ptype_attributes = [] (* we never have attributes *)
             ; ptype_loc = loc
             }
           in
           { pstr_desc = Pstr_type (Recursive, [ type_decl ]); pstr_loc = loc })
  in
  { pmod_desc = Pmod_structure structure_items; pmod_loc = loc; pmod_attributes = [] }
;;

let expand_nominal ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match ParseUtil.parse_string (ParseTerm.whitespace_t ParsePrimitive.t) str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> mk_nominal ~loc tm
;;

let expand_nonbinding ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match ParseUtil.parse_string (ParseNonbinding.whitespace_term ParsePrimitive.t) str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> mk_nonbinding ~loc tm
;;

let expand_pattern ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match ParseUtil.parse_string (ParsePattern.whitespace_t ParsePrimitive.t) str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> mk_pattern ~loc tm
;;

let expand_abstract_syntax ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match ParseUtil.parse_string ParseAbstract.whitespace_t str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> mk_language ~loc syntax
;;

let expand_abstract_syntax_module ~(loc : Location.t) ~path:_ (expr : expression)
    : module_expr
  =
  let str, loc = extract_string loc expr in
  match ParseUtil.parse_string ParseAbstract.whitespace_t str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> mk_language_module ~loc syntax
;;

let term_extension =
  Extension.declare
    "lvca_nominal"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_nominal
;;

let nonbinding_extension =
  Extension.declare
    "lvca_nonbinding"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_nonbinding
;;

let pattern_extension =
  Extension.declare
    "lvca_pattern"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_pattern
;;

let abstract_syntax_extension =
  Extension.declare
    "lvca_abstract_syntax"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_abstract_syntax
;;

let abstract_syntax_module_extension =
  Extension.declare
    "abstract_syntax_module" (* TODO: better naming *)
    Extension.Context.Module_expr
    Ast_pattern.(single_expr_payload __)
    expand_abstract_syntax_module
;;

let () =
  Ppxlib.Driver.register_transformation
    "lvca"
    ~rules:
      [ Context_free.Rule.extension term_extension
      ; Context_free.Rule.extension nonbinding_extension
      ; Context_free.Rule.extension pattern_extension
      ; Context_free.Rule.extension abstract_syntax_extension
      ; Context_free.Rule.extension abstract_syntax_module_extension
      ]
;;

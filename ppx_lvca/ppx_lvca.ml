open Base
open Lvca_syntax
open Ppxlib
module ParsePrimitive = Lvca_syntax.Primitive.Parse (ParseUtil.CComment)
module ParsePattern = Lvca_syntax.Pattern.Parse (ParseUtil.CComment)
module ParseTerm = Nominal.Term.Parse (ParseUtil.CComment)
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
  | Nominal.Term.Operator (pos, name, scopes) ->
    let name_exp = mk_str ~loc name in
    let scopes = scopes |> List.map ~f:(mk_scope ~loc) |> mk_list ~loc in
    [%expr Nominal.Term.Operator ([%e mk_pos ~loc pos], [%e name_exp], [%e scopes])]
  | Var (pos, str) ->
    [%expr Nominal.Term.Var ([%e mk_pos ~loc pos], [%e mk_str ~loc str])]
  | Primitive (pos, prim) ->
    [%expr Nominal.Term.Primitive ([%e mk_pos ~loc pos], [%e mk_prim ~loc prim])]

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

module ModuleExpander = struct
  module Util = Lvca_util

  let mk_type ~loc ptyp_desc =
    { ptyp_desc; ptyp_loc = loc; ptyp_loc_stack = []; ptyp_attributes = [] }
  ;;

  let mk_pat ~loc ppat_desc =
    { ppat_desc; ppat_loc = loc; ppat_loc_stack = []; ppat_attributes = [] }
  ;;

  let mk_exp ~loc pexp_desc =
    { pexp_desc; pexp_loc = loc; pexp_loc_stack = []; pexp_attributes = [] }
  ;;

  let mk_value_binding ~loc pvb_pat pvb_expr =
    { pvb_pat; pvb_expr; pvb_attributes = []; pvb_loc = loc }
  ;;

  (* TODO: more sophisticated rule? *)
  let ctor_name = String.capitalize
  let module_name = String.capitalize
  let mk_var_type ~loc name = mk_type ~loc (Ptyp_var name)
  let pattern_t_ident = Ldot (Lident "Pattern", "t")
  let pattern_t ~loc = { txt = pattern_t_ident; loc }
  let void_t_loc = Ldot (Ldot (Lident "Lvca_util", "Void"), "t")
  let void_t ~loc = { txt = void_t_loc; loc }

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

  let args_of_valence
      ~loc
      ~info
      var_set
      (AbstractSyntax.Valence.Valence (binding_sort_slots, body_sort))
    =
    let body_type = ptyp_of_sort ~loc var_set body_sort in
    let ptyp_desc =
      match binding_sort_slots with
      | [] -> body_type
      | slots ->
        let pat_var_types =
          if info
          then [ mk_var_type ~loc "info"; mk_type ~loc (Ptyp_constr (void_t ~loc, [])) ]
          else
            [ mk_type ~loc (Ptyp_constr ({ txt = Lident "unit"; loc }, []))
            ; mk_type ~loc (Ptyp_constr (void_t ~loc, []))
            ]
        in
        let binding_types =
          slots
          |> List.map ~f:(fun slot ->
                 let ty =
                   match slot with
                   | AbstractSyntax.SortSlot.SortBinding _sort ->
                     Ptyp_constr ({ txt = Lident "string"; loc }, [])
                   | SortPattern _sort -> Ptyp_constr (pattern_t ~loc, pat_var_types)
                 in
                 mk_type ~loc ty)
        in
        let body_type = mk_type ~loc body_type in
        Ptyp_tuple (binding_types @ [ body_type ])
    in
    mk_type ~loc ptyp_desc
  ;;

  (* XXX possibility of generating two constructors with same name *)
  let mk_ctor ~loc ~info var_set (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
    =
    let args = arity |> List.map ~f:(args_of_valence ~loc ~info var_set) in
    let args = if info then mk_type ~loc (Ptyp_var "info") :: args else args in
    { pcd_name = { txt = ctor_name op_name; loc }
    ; pcd_args = Pcstr_tuple args
    ; pcd_res = None (* Is this only for GADTs? *)
    ; pcd_loc = loc
    ; pcd_attributes = []
    }
  ;;

  let mk_params ~loc vars =
    vars
    |> List.map ~f:(fun var_name ->
           let core_type = mk_var_type ~loc var_name in
           core_type, Invariant
           (* Covariant? *))
  ;;

  let mk_type_decl ~loc ~info (AbstractSyntax.SortDef.SortDef (vars, op_defs)) =
    let vars = vars |> List.map ~f:Util.Tuple2.get1 in
    let var_set = vars |> Util.String.Set.of_list in
    let params = if info then "info" :: vars else vars in
    let type_decl =
      { ptype_name = { txt = "t"; loc }
      ; ptype_params = mk_params ~loc params
      ; ptype_cstrs = [] (* we never use constraints *)
      ; ptype_kind = Ptype_variant (op_defs |> List.map ~f:(mk_ctor ~loc ~info var_set))
      ; ptype_private = Public
      ; ptype_manifest = None
      ; ptype_attributes = [] (* we never have attributes *)
      ; ptype_loc = loc
      }
    in
    { pstr_desc = Pstr_type (Recursive, [ type_decl ]); pstr_loc = loc }
  ;;

  type ctor_type =
    | Plain
    | WithInfo

  type conversion_direction =
    | ToPlain
    | OfPlain

  let mk_operator_pat
      ~loc
      ~ctor_type
      ?(match_info = false)
      ?(match_non_info = true)
      ?(name_base = "x")
      (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
    =
    let var_ix = ref 0 in
    let binders =
      arity
      |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, _)) ->
             let slots =
               ( (* Always one binder for the body *) ) :: List.map slots ~f:(Fn.const ())
             in
             slots
             |> List.map ~f:(fun () ->
                    if match_non_info
                    then (
                      Int.incr var_ix;
                      Printf.sprintf "%s%d" name_base !var_ix)
                    else "_"))
      |> List.join
    in
    let binders =
      match ctor_type, match_info with
      | WithInfo, false -> "_" :: binders
      | WithInfo, true -> Printf.sprintf "%s0" name_base :: binders
      | Plain, _ -> binders
    in
    let binders =
      binders |> List.map ~f:(fun txt -> mk_pat ~loc (Ppat_var { txt; loc }))
    in
    let constr_body =
      match binders with [ x ] -> x | _ -> mk_pat ~loc (Ppat_tuple binders)
    in
    mk_pat
      ~loc
      (let txt =
         match ctor_type with
         | WithInfo -> Lident op_name
         | Plain -> Ldot (Lident "Plain", op_name)
       in
       Ppat_construct ({ txt; loc }, Some constr_body))
  ;;

  let mk_operator_exp
      ~loc
      ~ctor_type
      ?(name_base = "x")
      (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
    =
    let ctor_contents =
      match arity with
      | [] -> None
      | _ ->
        let var_ix = ref 0 in
        (* TODO: recurse with map_info / of_plain *)
        let contents =
          arity
          |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, _)) ->
                 let slots =
                   ( (* Always one var for the body *) )
                   :: List.map slots ~f:(Fn.const ())
                 in
                 slots
                 |> List.map ~f:(fun () ->
                        Int.incr var_ix;
                        Printf.sprintf "%s%d" name_base !var_ix))
          |> List.join
        in
        let contents =
          match ctor_type with
          | WithInfo -> Printf.sprintf "%s0" name_base :: contents
          | Plain -> contents
        in
        let contents =
          contents
          |> List.map ~f:(fun name -> mk_exp ~loc (Pexp_ident { txt = Lident name; loc }))
        in
        (* TODO: apply f *)
        (match contents with
        | [ x1 ] -> Some x1
        | _ -> Some (mk_exp ~loc (Pexp_tuple contents)))
    in
    let txt =
      match ctor_type with
      | WithInfo -> Lident op_name
      | Plain -> Ldot (Lident "Plain", op_name)
    in
    mk_exp ~loc (Pexp_construct ({ txt; loc }, ctor_contents))
  ;;

  let mk_plain_converter ~loc conversion_direction op_defs =
    let value_binding =
      let pat =
        let txt =
          match conversion_direction with ToPlain -> "to_plain" | OfPlain -> "of_plain"
        in
        mk_pat ~loc (Ppat_var { txt; loc })
      in
      let exp =
        let f op_def =
          let pat_ctor_type, exp_ctor_type =
            match conversion_direction with
            | ToPlain -> WithInfo, Plain
            | OfPlain -> Plain, WithInfo
          in
          { pc_lhs = mk_operator_pat ~loc ~ctor_type:pat_ctor_type op_def
          ; pc_guard = None
          ; pc_rhs = mk_operator_exp ~loc ~ctor_type:exp_ctor_type op_def
          }
        in
        let branches = List.map op_defs ~f in
        mk_exp ~loc (Pexp_function branches)
      in
      mk_value_binding ~loc pat exp
    in
    { pstr_desc = Pstr_value (Recursive, [ value_binding ]); pstr_loc = loc }
  ;;

  let mk_sort_module
      ~loc
      name
      (AbstractSyntax.SortDef.SortDef (_vars, op_defs) as sort_def)
    =
    let plain_module =
      let module_binding =
        { pmb_name = { txt = Some "Plain"; loc }
        ; pmb_expr =
            { pmod_desc = Pmod_structure [ mk_type_decl ~loc ~info:false sort_def ]
            ; pmod_loc = loc
            ; pmod_attributes = []
            }
        ; pmb_attributes = []
        ; pmb_loc = loc
        }
      in
      { pstr_desc = Pstr_module module_binding; pstr_loc = loc }
    in
    let to_plain = mk_plain_converter ~loc ToPlain op_defs in
    let of_plain = mk_plain_converter ~loc OfPlain op_defs in
    let equal =
      let body =
        let branches =
          op_defs
          |> List.map ~f:(fun op_def ->
                 let pc_lhs =
                   let p1 =
                     mk_operator_pat
                       ~loc
                       ~ctor_type:WithInfo
                       ~match_info:true
                       ~name_base:"x"
                       op_def
                   in
                   let p2 =
                     mk_operator_pat
                       ~loc
                       ~ctor_type:WithInfo
                       ~match_info:true
                       ~name_base:"y"
                       op_def
                   in
                   mk_pat ~loc (Ppat_tuple [ p1; p2 ])
                 in
                 let pc_rhs =
                   let info_eq =
                     mk_exp ~loc (Pexp_ident { txt = Lident "info_eq"; loc })
                   in
                   let args =
                     [ "x0"; "y0" ]
                     |> List.map ~f:(fun name ->
                            Nolabel, mk_exp ~loc (Pexp_ident { txt = Lident name; loc }))
                   in
                   mk_exp ~loc (Pexp_apply (info_eq, args))
                 in
                 { pc_lhs; pc_guard = None; pc_rhs })
        in
        let last_branch =
          { pc_lhs =
              mk_pat ~loc (Ppat_tuple [ mk_pat ~loc Ppat_any; mk_pat ~loc Ppat_any ])
          ; pc_guard = None
          ; pc_rhs = mk_exp ~loc (Pexp_ident { txt = Lident "false"; loc })
          }
        in
        let branches = Lvca_util.List.snoc branches last_branch in
        Pexp_match
          ( mk_exp
              ~loc
              (Pexp_tuple
                 [ mk_exp ~loc (Pexp_ident { txt = Lident "t1"; loc })
                 ; mk_exp ~loc (Pexp_ident { txt = Lident "t2"; loc })
                 ])
          , branches )
      in
      let pat = mk_pat ~loc (Ppat_var { txt = "equal"; loc }) in
      let exp =
        let pat = mk_pat ~loc (Ppat_var { txt = "info_eq"; loc }) in
        let exp =
          let pat = mk_pat ~loc (Ppat_var { txt = "t1"; loc }) in
          let exp =
            let pat = mk_pat ~loc (Ppat_var { txt = "t2"; loc }) in
            let exp = mk_exp ~loc body in
            mk_exp ~loc (Pexp_fun (Nolabel, None, pat, exp))
          in
          mk_exp ~loc (Pexp_fun (Nolabel, None, pat, exp))
        in
        mk_exp ~loc (Pexp_fun (Labelled "info_eq", None, pat, exp))
      in
      let value_binding = mk_value_binding ~loc pat exp in
      { pstr_desc = Pstr_value (Recursive, [ value_binding ]); pstr_loc = loc }
    in
    let info =
      let pat = mk_pat ~loc (Ppat_var { txt = "info"; loc }) in
      let f op_def =
        let pc_lhs =
          mk_operator_pat
            ~loc
            ~ctor_type:WithInfo
            ~match_info:true
            ~match_non_info:false
            op_def
        in
        { pc_lhs
        ; pc_guard = None
        ; pc_rhs = mk_exp ~loc (Pexp_ident { txt = Lident "x0"; loc })
        }
      in
      let branches = op_defs |> List.map ~f in
      let exp = mk_exp ~loc (Pexp_function branches) in
      let value_binding = mk_value_binding ~loc pat exp in
      { pstr_desc = Pstr_value (Nonrecursive, [ value_binding ]); pstr_loc = loc }
    in
    let map_info =
      let pat = mk_pat ~loc (Ppat_var { txt = "map_info"; loc }) in
      let exp =
        let f op_def =
          { pc_lhs = mk_operator_pat ~loc ~ctor_type:WithInfo op_def
          ; pc_guard = None
          ; pc_rhs = mk_operator_exp ~loc ~ctor_type:WithInfo op_def
          }
        in
        let branches = List.map op_defs ~f in
        mk_exp ~loc (Pexp_function branches)
      in
      let value_binding = mk_value_binding ~loc pat exp in
      { pstr_desc = Pstr_value (Recursive, [ value_binding ]); pstr_loc = loc }
    in
    (*
    let pp_generic = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let of_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let to_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    select?
    *)
    let module_binding =
      { pmb_name = { txt = Some (module_name name); loc }
      ; pmb_expr =
          { pmod_desc =
              Pmod_structure
                [ mk_type_decl ~loc ~info:true sort_def
                ; plain_module
                ; to_plain
                ; of_plain
                ; equal
                ; info
                ; map_info
                  (*
                ; pp_generic
                ; of_nominal
                ; to_nominal
                *)
                ]
          ; pmod_loc = loc
          ; pmod_attributes = []
          }
      ; pmb_attributes = []
      ; pmb_loc = loc
      }
    in
    { pstr_desc = Pstr_module module_binding; pstr_loc = loc }
  ;;

  let mk_container_module ~loc AbstractSyntax.{ externals = _; sort_defs } =
    let structure_items =
      sort_defs |> List.map ~f:(Util.Tuple2.uncurry (mk_sort_module ~loc))
    in
    { pmod_desc = Pmod_structure structure_items; pmod_loc = loc; pmod_attributes = [] }
  ;;

  let expand ~(loc : Location.t) ~path:_ (expr : expression) : module_expr =
    let str, loc = extract_string loc expr in
    match ParseUtil.parse_string ParseAbstract.whitespace_t str with
    | Error msg -> Location.raise_errorf ~loc "%s" msg
    | Ok syntax -> mk_container_module ~loc syntax
  ;;
end

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
    ModuleExpander.expand
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

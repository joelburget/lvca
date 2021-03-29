open Base
open Lvca_syntax
open Ppxlib
module Util = Lvca_util
module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

type ctor_type =
  | Plain
  | WithInfo

type conversion_direction =
  | ToPlain
  | OfPlain

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

let mk_args ~loc args body =
  List.fold_right args ~init:(mk_exp ~loc body) ~f:(fun (label, txt) body ->
      let pat = mk_pat ~loc (Ppat_var { txt; loc }) in
      mk_exp ~loc (Pexp_fun (label, None, pat, body)))
;;

let mk_fun ~loc fun_name args body =
  mk_value_binding
    ~loc
    (mk_pat ~loc (Ppat_var { txt = fun_name; loc }))
    (mk_args ~loc args body)
;;

(* Concatenate a list of names into a Longident. *)
let build_names names =
  match names with
  | [] -> Lvca_util.invariant_violation "build_names: names must be nonempty"
  | nm0 :: nms -> List.fold nms ~init:(Lident nm0) ~f:(fun accum m -> Ldot (accum, m))
;;

(* Access a module member, by default [t]. Eg [Sort.Plain.t]. *)
let modules_t ?(op_name = "t") modules =
  Lvca_util.List.snoc modules op_name |> build_names
;;

(* TODO: more sophisticated naming rules? *)
let ctor_name = String.capitalize
let module_name = String.capitalize
let mk_var_type ~loc name = mk_type ~loc (Ptyp_var name)
let pattern_t_ident = modules_t [ "Pattern" ]
let pattern_t ~loc = { txt = pattern_t_ident; loc }
let void_t_loc = modules_t [ "Lvca_util"; "Void" ]
let void_t ~loc = { txt = void_t_loc; loc }
let mk_typ_tuple ~loc = function [ ty ] -> ty | tys -> mk_type ~loc (Ptyp_tuple tys)

let mk_pat_tuple ~loc = function
  | [ elem ] -> elem
  | elems -> mk_pat ~loc (Ppat_tuple elems)
;;

let mk_exp_tuple ~loc = function
  | [ elem ] -> elem
  | elems -> mk_exp ~loc (Pexp_tuple elems)
;;

let sort_head = function Sort.Name (_, name) | Sort.Ap (_, name, _) -> name

let rec ptyp_of_sort ~loc ~sort_name ~info var_set = function
  | Sort.Name (_, name) ->
    let info_args = if info then [ mk_type ~loc (Ptyp_var "info") ] else [] in
    if Set.mem var_set name
    then (* This is a variable if it's in the set of vars ... *)
      Ptyp_var name
    else if String.(name = sort_name)
    then
      (* ... otherwise it's [t] if it matches the sort name ... *)
      Ptyp_constr ({ txt = Lident "t"; loc }, info_args)
    else if info
    then (
      (* ... otherwise it's ['info Module_name.t] if we include info ... *)
      let txt = modules_t [ module_name name ] in
      Ptyp_constr ({ txt; loc }, info_args))
    else (
      (* ... otherwise it's [Module_name.Plain.t]. *)
      let txt = modules_t [ module_name name; "Plain" ] in
      Ptyp_constr ({ txt; loc }, []))
  | Ap (_, name, args) ->
    let f sort = sort |> ptyp_of_sort ~loc ~sort_name ~info var_set |> mk_type ~loc in
    Ptyp_constr ({ txt = Lident name; loc }, List.map args ~f)
;;

let conjuntion ~loc exps =
  let exps, last = Lvca_util.List.unsnoc exps in
  let op = mk_exp ~loc (Pexp_ident { txt = Lident "&&"; loc }) in
  List.fold_right exps ~init:last ~f:(fun e1 e2 ->
      mk_exp ~loc (Pexp_apply (op, [ Nolabel, e1; Nolabel, e2 ])))
;;

let args_of_valence
    ~loc
    ~info
    ~sort_name
    var_set
    (AbstractSyntax.Valence.Valence (binding_sort_slots, body_sort))
  =
  let body_type = ptyp_of_sort ~loc ~sort_name ~info var_set body_sort in
  match binding_sort_slots with
  | [] -> [ mk_type ~loc body_type ]
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
    binding_types @ [ body_type ]
;;

(* XXX possibility of generating two constructors with same name *)
let mk_ctor_decl
    ~loc
    ~info
    ~sort_name
    var_set
    (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
  =
  let args = arity |> List.map ~f:(args_of_valence ~loc ~info ~sort_name var_set) in
  let args = if info then [ mk_type ~loc (Ptyp_var "info") ] :: args else args in
  let args = List.map args ~f:(mk_typ_tuple ~loc) in
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

let mk_type_decl ~loc ~info ~sort_name (AbstractSyntax.SortDef.SortDef (vars, op_defs)) =
  let vars = List.map vars ~f:Util.Tuple2.get1 in
  let var_set = Util.String.Set.of_list vars in
  let params = if info then "info" :: vars else vars in
  let type_decl =
    { ptype_name = { txt = "t"; loc }
    ; ptype_params = mk_params ~loc params
    ; ptype_cstrs = [] (* we never use constraints *)
    ; ptype_kind =
        Ptype_variant (op_defs |> List.map ~f:(mk_ctor_decl ~loc ~info ~sort_name var_set))
    ; ptype_private = Public
    ; ptype_manifest = None
    ; ptype_attributes = [] (* we never have attributes *)
    ; ptype_loc = loc
    }
  in
  { pstr_desc = Pstr_type (Recursive, [ type_decl ]); pstr_loc = loc }
;;

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
  in
  let binders =
    match ctor_type, match_info with
    | WithInfo, false -> [ "_" ] :: binders
    | WithInfo, true -> [ Printf.sprintf "%s0" name_base ] :: binders
    | Plain, _ -> binders
  in
  let constr_body =
    binders
    |> List.map ~f:(fun names ->
           names
           |> List.map ~f:(fun txt -> mk_pat ~loc (Ppat_var { txt; loc }))
           |> mk_pat_tuple ~loc)
    |> mk_pat_tuple ~loc
  in
  mk_pat
    ~loc
    (let txt =
       match ctor_type with
       | WithInfo -> Lident op_name
       | Plain -> build_names [ "Plain"; op_name ]
     in
     Ppat_construct ({ txt; loc }, Some constr_body))
;;

(* TODO: remove redundancy with pc_rhs *)
let plain_converter_operator_exp
    ~loc
    ~ctor_type
    ?(name_base = "x")
    fun_name
    sort_name
    (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
  =
  let pattern_converter =
    mk_exp ~loc (Pexp_ident { txt = build_names [ "Pattern"; fun_name ]; loc })
  in
  let ctor_contents =
    match arity with
    | [] ->
      (match ctor_type with
      | WithInfo -> Some (mk_exp ~loc (Pexp_tuple []))
      | Plain -> None)
    | _ ->
      let var_ix = ref 0 in
      let body_arg sort =
        Int.incr var_ix;
        let arg = Lident (Printf.sprintf "%s%d" name_base !var_ix) in
        let scoped_converter =
          if String.(sort_head sort = sort_name)
          then Lident fun_name
          else
            (* is it always valid to just use module_name? *)
            build_names [ module_name (sort_head sort); fun_name ]
        in
        Pexp_apply
          ( mk_exp ~loc (Pexp_ident { txt = scoped_converter; loc })
          , [ Nolabel, mk_exp ~loc (Pexp_ident { txt = arg; loc }) ] )
      in
      let contents =
        arity
        |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
               slots
               |> List.map ~f:(fun slot ->
                      Int.incr var_ix;
                      let arg =
                        let name = Printf.sprintf "%s%d" name_base !var_ix in
                        Pexp_ident { txt = Lident name; loc }
                      in
                      match slot with
                      | AbstractSyntax.SortSlot.SortBinding _sort -> arg
                      | SortPattern _ ->
                        Pexp_apply (pattern_converter, [ Nolabel, mk_exp ~loc arg ]))
               |> Fn.flip Lvca_util.List.snoc (body_arg body_sort))
      in
      let contents =
        match ctor_type with
        | WithInfo -> [ Pexp_tuple [] ] :: contents
        | Plain -> contents
      in
      let contents =
        contents
        |> List.map ~f:(fun names ->
               names |> List.map ~f:(mk_exp ~loc) |> mk_exp_tuple ~loc)
        |> mk_exp_tuple ~loc
      in
      Some contents
  in
  let names =
    match ctor_type with WithInfo -> [ op_name ] | Plain -> [ "Plain"; op_name ]
  in
  mk_exp ~loc (Pexp_construct ({ txt = build_names names; loc }, ctor_contents))
;;

let mk_plain_converter ~loc conversion_direction sort_name op_defs =
  let value_binding =
    let fun_name =
      match conversion_direction with ToPlain -> "to_plain" | OfPlain -> "of_plain"
    in
    let pat = mk_pat ~loc (Ppat_var { txt = fun_name; loc }) in
    let exp =
      let f op_def =
        let pat_ctor_type, exp_ctor_type =
          match conversion_direction with
          | ToPlain -> WithInfo, Plain
          | OfPlain -> Plain, WithInfo
        in
        { pc_lhs = mk_operator_pat ~loc ~ctor_type:pat_ctor_type op_def
        ; pc_guard = None
        ; pc_rhs =
            plain_converter_operator_exp
              ~loc
              ~ctor_type:exp_ctor_type
              fun_name
              sort_name
              op_def
        }
      in
      let branches = List.map op_defs ~f in
      mk_exp ~loc (Pexp_function branches)
    in
    mk_value_binding ~loc pat exp
  in
  { pstr_desc = Pstr_value (Recursive, [ value_binding ]); pstr_loc = loc }
;;

let mk_equal ~loc sort_name op_defs =
  let same_sort sort = String.(sort_head sort = sort_name) in
  let body =
    let branches =
      op_defs
      |> List.map
           ~f:(fun (AbstractSyntax.OperatorDef.OperatorDef (_op_name, arity) as op_def) ->
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
               let info_eq = mk_exp ~loc (Pexp_ident { txt = Lident "info_eq"; loc }) in
               let var_ix = ref 0 in
               let mk_xy () =
                 [ "x"; "y" ]
                 |> List.map ~f:(fun base ->
                        let txt = Lident (Printf.sprintf "%s%d" base !var_ix) in
                        Nolabel, mk_exp ~loc (Pexp_ident { txt; loc }))
               in
               let info_exp = mk_exp ~loc (Pexp_apply (info_eq, mk_xy ())) in
               let other_exps =
                 arity
                 |> List.map
                      ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
                        let slots_checks =
                          slots
                          |> List.map ~f:(fun slot ->
                                 Int.incr var_ix;
                                 let names, args =
                                   match slot with
                                   | AbstractSyntax.SortSlot.SortBinding _sort ->
                                     [ "String"; "=" ], mk_xy ()
                                   | SortPattern _ ->
                                     ( [ "Pattern"; "equal" ]
                                     , (Labelled "info_eq", info_eq) :: mk_xy () )
                                 in
                                 let f =
                                   mk_exp
                                     ~loc
                                     (Pexp_ident { txt = build_names names; loc })
                                 in
                                 mk_exp ~loc (Pexp_apply (f, args)))
                        in
                        let body_check =
                          Int.incr var_ix;
                          let txt =
                            if same_sort body_sort
                            then Lident "equal"
                            else
                              build_names [ module_name (sort_head body_sort); "equal" ]
                          in
                          let args = (Labelled "info_eq", info_eq) :: mk_xy () in
                          mk_exp
                            ~loc
                            (Pexp_apply (mk_exp ~loc (Pexp_ident { txt; loc }), args))
                        in
                        Lvca_util.List.snoc slots_checks body_check)
                 |> List.join
               in
               conjuntion ~loc (info_exp :: other_exps)
             in
             { pc_lhs; pc_guard = None; pc_rhs })
    in
    let last_branch =
      { pc_lhs = mk_pat ~loc (Ppat_tuple [ mk_pat ~loc Ppat_any; mk_pat ~loc Ppat_any ])
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
  let value_binding =
    mk_fun
      ~loc
      "equal"
      [ Labelled "info_eq", "info_eq"; Nolabel, "t1"; Nolabel, "t2" ]
      body
  in
  { pstr_desc = Pstr_value (Recursive, [ value_binding ]); pstr_loc = loc }
;;

let mk_info ~loc op_defs =
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
;;

(* TODO: remove redundancy with plain_converter_operator_exp *)
let map_info_rhs ~loc sort_name (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity)) =
  let pattern_map_info =
    mk_exp ~loc (Pexp_ident { txt = build_names [ "Pattern"; "map_info" ]; loc })
  in
  let f = mk_exp ~loc (Pexp_ident { txt = Lident "f"; loc }) in
  let ctor_contents =
    match arity with
    | [] ->
      (* TODO: add test case for this *)
      mk_exp
        ~loc
        (Pexp_apply (f, [ Nolabel, mk_exp ~loc (Pexp_ident { txt = Lident "x0"; loc }) ]))
    | _ ->
      let var_ix = ref 0 in
      let body_arg sort =
        Int.incr var_ix;
        let arg = Lident (Printf.sprintf "x%d" !var_ix) in
        let scoped_map_info =
          if String.(sort_head sort = sort_name)
          then Lident "map_info"
          else
            (* is it always valid to just use module_name? *)
            build_names [ module_name (sort_head sort); "map_info" ]
        in
        Pexp_apply
          ( mk_exp ~loc (Pexp_ident { txt = scoped_map_info; loc })
          , [ Labelled "f", f; Nolabel, mk_exp ~loc (Pexp_ident { txt = arg; loc }) ] )
      in
      arity
      |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
             slots
             |> List.map ~f:(fun slot ->
                    Int.incr var_ix;
                    let arg =
                      let name = Printf.sprintf "x%d" !var_ix in
                      Pexp_ident { txt = Lident name; loc }
                    in
                    match slot with
                    | AbstractSyntax.SortSlot.SortBinding _sort -> arg
                    | SortPattern _ ->
                      Pexp_apply
                        (pattern_map_info, [ Labelled "f", f; Nolabel, mk_exp ~loc arg ]))
             |> Fn.flip Lvca_util.List.snoc (body_arg body_sort))
      |> List.cons
           [ Pexp_apply
               (f, [ Nolabel, mk_exp ~loc (Pexp_ident { txt = Lident "x0"; loc }) ])
           ]
      |> List.map ~f:(fun names ->
             names |> List.map ~f:(mk_exp ~loc) |> mk_exp_tuple ~loc)
      |> mk_exp_tuple ~loc
  in
  mk_exp
    ~loc
    (Pexp_construct ({ txt = build_names [ op_name ]; loc }, Some ctor_contents))
;;

let mk_map_info ~loc sort_name op_defs =
  let body =
    let f op_def =
      let pc_lhs = mk_operator_pat ~loc ~ctor_type:WithInfo ~match_info:true op_def in
      { pc_lhs; pc_guard = None; pc_rhs = map_info_rhs ~loc sort_name op_def }
    in
    let branches = List.map op_defs ~f in
    Pexp_function branches
  in
  let value_binding = mk_fun ~loc "map_info" [ Labelled "f", "f" ] body in
  { pstr_desc = Pstr_value (Recursive, [ value_binding ]); pstr_loc = loc }
;;

let mk_sort_module
    ~loc
    sort_name
    (AbstractSyntax.SortDef.SortDef (_vars, op_defs) as sort_def)
  =
  let plain_module =
    let module_binding =
      { pmb_name = { txt = Some "Plain"; loc }
      ; pmb_expr =
          { pmod_desc =
              Pmod_structure [ mk_type_decl ~loc ~info:false ~sort_name sort_def ]
          ; pmod_loc = loc
          ; pmod_attributes = []
          }
      ; pmb_attributes = []
      ; pmb_loc = loc
      }
    in
    { pstr_desc = Pstr_module module_binding; pstr_loc = loc }
  in
  let to_plain = mk_plain_converter ~loc ToPlain sort_name op_defs in
  let of_plain = mk_plain_converter ~loc OfPlain sort_name op_defs in
  let equal = mk_equal ~loc sort_name op_defs in
  let info = mk_info ~loc op_defs in
  let map_info = mk_map_info ~loc sort_name op_defs in
  (*
    let pp_generic = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let of_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let to_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    select?
    *)
  let module_binding =
    { pmb_name = { txt = Some (module_name sort_name); loc }
    ; pmb_expr =
        { pmod_desc =
            Pmod_structure
              [ mk_type_decl ~loc ~info:true ~sort_name sort_def
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

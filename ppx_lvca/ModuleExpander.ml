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

(* Concatenate a list of names into a Longident. *)
let build_names names =
  match names with
  | [] -> Lvca_util.invariant_violation "build_names: names must be nonempty"
  | nm0 :: nms -> List.fold nms ~init:(Lident nm0) ~f:(fun accum m -> Ldot (accum, m))
;;

(* TODO: more sophisticated naming rules? *)
let ctor_name = String.capitalize
let module_name = String.capitalize
let guard = None

let mk_typ_tuple ~loc = function
  | [] -> [%type: unit]
  | [ ty ] -> ty
  | tys -> mk_type ~loc (Ptyp_tuple tys)
;;

let mk_pat_tuple ~loc = function
  | [] -> [%pat? ()]
  | [ elem ] -> elem
  | elems ->
    { ppat_desc = Ppat_tuple elems
    ; ppat_loc = loc
    ; ppat_loc_stack = []
    ; ppat_attributes = []
    }
;;

let mk_exp_tuple ~loc = function
  | [] -> [%expr ()]
  | [ elem ] -> elem
  | elems ->
    { pexp_desc = Pexp_tuple elems
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
;;

let sort_head = function Sort.Name (_, name) | Sort.Ap (_, name, _) -> name

let rec ptyp_of_sort ~loc ~sort_name ~info var_set = function
  | Sort.Name (_, name) ->
    let info_args = if info then [ [%type: 'info] ] else [] in
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
      let txt = build_names [ module_name name; "t" ] in
      Ptyp_constr ({ txt; loc }, info_args))
    else (
      (* ... otherwise it's [Module_name.Plain.t]. *)
      let txt = build_names [ module_name name; "Plain"; "t" ] in
      Ptyp_constr ({ txt; loc }, []))
  | Ap (_, name, args) ->
    let f sort = sort |> ptyp_of_sort ~loc ~sort_name ~info var_set |> mk_type ~loc in
    Ptyp_constr ({ txt = Lident name; loc }, List.map args ~f)
;;

let conjuntion ~loc exps =
  let exps, last = Lvca_util.List.unsnoc exps in
  List.fold_right exps ~init:last ~f:(fun e1 e2 -> [%expr [%e e1] && [%e e2]])
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
  | _ ->
    binding_sort_slots
    |> List.map ~f:(function
           | AbstractSyntax.SortSlot.SortBinding _sort -> [%type: string]
           | SortPattern _sort ->
             if info
             then [%type: ('info, Lvca_util.Void.t) Pattern.t]
             else [%type: (unit, Lvca_util.Void.t) Pattern.t])
    |> Fn.flip Lvca_util.List.snoc (mk_type ~loc body_type)
;;

(* XXX possibility of generating two constructors with same name *)
let mk_ctor_decl
    ~loc
    ~info
    ~sort_name
    var_set
    (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
  =
  let (module Ast) = Ast_builder.make loc in
  let args = List.map arity ~f:(args_of_valence ~loc ~info ~sort_name var_set) in
  let args = if info then [ [%type: 'info] ] :: args else args in
  let args = List.map args ~f:(mk_typ_tuple ~loc) in
  Ast.constructor_declaration
    ~name:{ txt = ctor_name op_name; loc }
    ~args:(Pcstr_tuple args)
    ~res:None
;;

let mk_type_decl ~loc ~info ~sort_name (AbstractSyntax.SortDef.SortDef (vars, op_defs)) =
  let (module Ast) = Ast_builder.make loc in
  let vars = List.map vars ~f:Util.Tuple2.get1 in
  let var_set = Util.String.Set.of_list vars in
  let params =
    (if info then "info" :: vars else vars)
    |> List.map ~f:Ast.ptyp_var
    |> List.map ~f:(fun ty -> ty, Invariant (* Covariant? *))
  in
  let kind =
    Ptype_variant (List.map op_defs ~f:(mk_ctor_decl ~loc ~info ~sort_name var_set))
  in
  let type_decl =
    Ast.type_declaration
      ~name:{ txt = "t"; loc }
      ~params
      ~cstrs:[]
      ~kind
      ~private_:Public
      ~manifest:None
  in
  Ast.pstr_type Recursive [ type_decl ]
;;

let mk_operator_pat
    ~loc
    ~ctor_type
    ?(match_info = false)
    ?(match_non_info = true)
    ?(name_base = "x")
    (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
  =
  let (module Ast) = Ast_builder.make loc in
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
           |> List.map ~f:(fun txt -> Ast.ppat_var { txt; loc })
           |> mk_pat_tuple ~loc)
    |> mk_pat_tuple ~loc
  in
  let txt =
    match ctor_type with
    | WithInfo -> Lident op_name
    | Plain -> build_names [ "Plain"; op_name ]
  in
  Ast.ppat_construct { txt; loc } (Some constr_body)
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
  let (module Ast) = Ast_builder.make loc in
  let pattern_converter =
    Ast.pexp_ident { txt = build_names [ "Pattern"; fun_name ]; loc }
  in
  let ctor_contents =
    match arity with
    | [] -> (match ctor_type with WithInfo -> Some [%expr ()] | Plain -> None)
    | _ ->
      let var_ix = ref 0 in
      let body_arg sort =
        Int.incr var_ix;
        let f =
          if String.(sort_head sort = sort_name)
          then Ast.evar fun_name
          else
            (* is it always valid to just use module_name? *)
            Ast.pexp_ident
              { txt = build_names [ module_name (sort_head sort); fun_name ]; loc }
        in
        let arg = Ast.evar (Printf.sprintf "%s%d" name_base !var_ix) in
        [%expr [%e f] [%e arg]]
      in
      let contents =
        arity
        |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
               slots
               |> List.map ~f:(fun slot ->
                      Int.incr var_ix;
                      let arg = Ast.evar (Printf.sprintf "%s%d" name_base !var_ix) in
                      match slot with
                      | AbstractSyntax.SortSlot.SortBinding _sort -> arg
                      | SortPattern _ -> [%expr [%e pattern_converter] [%e arg]])
               |> Fn.flip Lvca_util.List.snoc (body_arg body_sort))
        |> List.map ~f:(mk_exp_tuple ~loc)
      in
      let contents =
        match ctor_type with WithInfo -> [%expr ()] :: contents | Plain -> contents
      in
      Some (mk_exp_tuple contents ~loc)
  in
  let names =
    match ctor_type with WithInfo -> [ op_name ] | Plain -> [ "Plain"; op_name ]
  in
  Ast.pexp_construct { txt = build_names names; loc } ctor_contents
;;

let mk_plain_converter ~loc conversion_direction sort_name op_defs =
  let (module Ast) = Ast_builder.make loc in
  let fun_name =
    match conversion_direction with ToPlain -> "to_plain" | OfPlain -> "of_plain"
  in
  let f op_def =
    let pat_ctor_type, exp_ctor_type =
      match conversion_direction with
      | ToPlain -> WithInfo, Plain
      | OfPlain -> Plain, WithInfo
    in
    let rhs =
      plain_converter_operator_exp ~loc ~ctor_type:exp_ctor_type fun_name sort_name op_def
    in
    Ast.case ~lhs:(mk_operator_pat ~loc ~ctor_type:pat_ctor_type op_def) ~guard ~rhs
  in
  op_defs |> List.map ~f |> Ast.pexp_function
;;

let mk_equal ~loc sort_name op_defs =
  let (module Ast) = Ast_builder.make loc in
  let same_sort sort = String.(sort_head sort = sort_name) in
  let branches =
    op_defs
    |> List.map
         ~f:(fun (AbstractSyntax.OperatorDef.OperatorDef (_op_name, arity) as op_def) ->
           let lhs =
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
             [%pat? [%p p1], [%p p2]]
           in
           let rhs =
             let var_ix = ref 0 in
             let mk_xy () =
               ("x", "y")
               |> Lvca_util.Tuple2.map ~f:(fun base ->
                      Ast.evar (Printf.sprintf "%s%d" base !var_ix))
             in
             let info_exp =
               let x, y = mk_xy () in
               [%expr info_eq [%e x] [%e y]]
             in
             let other_exps =
               arity
               |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
                      let slots_checks =
                        slots
                        |> List.map ~f:(fun slot ->
                               Int.incr var_ix;
                               let x, y = mk_xy () in
                               match slot with
                               | AbstractSyntax.SortSlot.SortBinding _sort ->
                                 [%expr String.([%e x] = [%e y])]
                               | SortPattern _ ->
                                 [%expr Pattern.equal ~info_eq [%e x] [%e y]])
                      in
                      let body_check =
                        Int.incr var_ix;
                        let txt =
                          if same_sort body_sort
                          then Lident "equal"
                          else build_names [ module_name (sort_head body_sort); "equal" ]
                        in
                        let ident = Ast.pexp_ident { txt; loc } in
                        let x, y = mk_xy () in
                        [%expr [%e ident] ~info_eq [%e x] [%e y]]
                      in
                      Lvca_util.List.snoc slots_checks body_check)
               |> List.join
             in
             conjuntion ~loc (info_exp :: other_exps)
           in
           Ast.case ~lhs ~guard ~rhs)
  in
  let last_branch = Ast.case ~lhs:[%pat? _, _] ~guard ~rhs:[%expr false] in
  Ast.pexp_match [%expr t1, t2] (Lvca_util.List.snoc branches last_branch)
;;

let mk_info ~loc op_defs =
  let (module Ast) = Ast_builder.make loc in
  let f op_def =
    let lhs =
      mk_operator_pat
        ~loc
        ~ctor_type:WithInfo
        ~match_info:true
        ~match_non_info:false
        op_def
    in
    Ast.case ~lhs ~guard ~rhs:[%expr x0]
  in
  op_defs |> List.map ~f |> Ast.pexp_function
;;

(* TODO: remove redundancy with plain_converter_operator_exp *)
let map_info_rhs ~loc sort_name (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity)) =
  let (module Ast) = Ast_builder.make loc in
  let ctor_contents =
    match arity with
    | [] -> (* TODO: add test case for this *) [%expr f x0]
    | _ ->
      let var_ix = ref 0 in
      let body_arg sort =
        Int.incr var_ix;
        let f =
          if String.(sort_head sort = sort_name)
          then Ast.evar "map_info"
          else
            (* is it always valid to just use module_name? *)
            Ast.pexp_ident
              { txt = build_names [ module_name (sort_head sort); "map_info" ]; loc }
        in
        let arg = Ast.evar (Printf.sprintf "x%d" !var_ix) in
        [%expr [%e f] ~f [%e arg]]
      in
      arity
      |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
             slots
             |> List.map ~f:(fun slot ->
                    Int.incr var_ix;
                    let arg = Ast.evar (Printf.sprintf "x%d" !var_ix) in
                    match slot with
                    | AbstractSyntax.SortSlot.SortBinding _sort -> arg
                    | SortPattern _ -> [%expr Pattern.map_info ~f [%e arg]])
             |> Fn.flip Lvca_util.List.snoc (body_arg body_sort))
      |> List.cons [ [%expr f x0] ]
      |> List.map ~f:(mk_exp_tuple ~loc)
      |> mk_exp_tuple ~loc
  in
  Ast.pexp_construct { txt = build_names [ op_name ]; loc } (Some ctor_contents)
;;

let mk_map_info ~loc sort_name op_defs =
  let (module Ast) = Ast_builder.make loc in
  let f op_def =
    let lhs = mk_operator_pat ~loc ~ctor_type:WithInfo ~match_info:true op_def in
    Ast.case ~lhs ~guard ~rhs:(map_info_rhs ~loc sort_name op_def)
  in
  op_defs |> List.map ~f |> Ast.pexp_function
;;

let mk_sort_module
    ~loc
    sort_name
    (AbstractSyntax.SortDef.SortDef (_vars, op_defs) as sort_def)
  =
  let (module Ast) = Ast_builder.make loc in
  let plain_module =
    let expr = Ast.pmod_structure [ mk_type_decl ~loc ~info:false ~sort_name sort_def ] in
    Ast.module_binding ~name:{ txt = Some "Plain"; loc } ~expr |> Ast.pstr_module
  in
  (*
    let pp_generic = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let of_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let to_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    select?
    *)
  let expr =
    Ast.pmod_structure
      ([ mk_type_decl ~loc ~info:true ~sort_name sort_def; plain_module ]
      @ [%str
          let rec to_plain = [%e mk_plain_converter ~loc ToPlain sort_name op_defs]
          let rec of_plain = [%e mk_plain_converter ~loc OfPlain sort_name op_defs]
          let rec equal ~info_eq t1 t2 = [%e mk_equal ~loc sort_name op_defs]
          let info = [%e mk_info ~loc op_defs]
          let rec map_info ~f = [%e mk_map_info ~loc sort_name op_defs]])
  in
  Ast.module_binding ~name:{ txt = Some (module_name sort_name); loc } ~expr
  |> Ast.pstr_module
;;

let mk_container_module ~loc AbstractSyntax.{ externals = _; sort_defs } =
  let (module Ast) = Ast_builder.make loc in
  sort_defs
  |> List.map ~f:(Util.Tuple2.uncurry (mk_sort_module ~loc))
  |> Ast.pmod_structure
;;

let expand ~(loc : Location.t) ~path:_ (expr : expression) : module_expr =
  let str, loc = extract_string loc expr in
  match ParseUtil.parse_string ParseAbstract.whitespace_t str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> mk_container_module ~loc syntax
;;

open Base
open Lvca_syntax
open Ppxlib
module Util = Lvca_util
module Syn = AbstractSyntax
module ParseAbstract = Syn.Parse (ParseUtil.CComment)
module SSet = Lvca_util.String.Set
module SMap = Lvca_util.String.Map

type conversion_direction =
  | ToPlain
  | OfPlain

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

let mk_pat_tuple ~loc = function
  | [] -> [%pat? ()]
  | [ elem ] -> elem
  | elems -> Ast_builder.Default.ppat_tuple ~loc elems
;;

let mk_exp_tuple ~loc = function
  | [] -> [%expr ()]
  | [ elem ] -> elem
  | elems -> Ast_builder.Default.pexp_tuple ~loc elems
;;

let sort_head = function Sort.Name (_, name) | Sort.Ap (_, name, _) -> name

let ptyp_of_sort (module Ast : Ast_builder.S) ~sort_name ~info =
  let loc = Ast.loc in
  function
  | Sort.Name (_, name) | Ap (_, name, _) ->
    let args = if info then [ [%type: 'info] ] else [] in
    let names =
      (* [t] if it matches the sort name, otherwise it's ['info Module_name.t]
         if we include info, otherwise it's [Module_name.Plain.t]. *)
      match String.(name = sort_name), info with
      | true, _ -> [ "t" ]
      | false, true -> [ module_name name; "t" ]
      | false, false -> [ module_name name; "Plain"; "t" ]
    in
    Ast.ptyp_constr { txt = build_names names; loc } args
;;

let conjuntion ~loc exps =
  let exps, last = Lvca_util.List.unsnoc exps in
  List.fold_right exps ~init:last ~f:(fun e1 e2 -> [%expr [%e e1] && [%e e2]])
;;

let args_of_valence
    (module Ast : Ast_builder.S)
    ~info
    ~sort_name
    (Syn.Valence.Valence (binding_sort_slots, body_sort))
  =
  let loc = Ast.loc in
  let body_type = ptyp_of_sort (module Ast) ~sort_name ~info body_sort in
  match binding_sort_slots with
  | [] -> [ body_type ]
  | _ ->
    binding_sort_slots
    |> List.map ~f:(function
           | Syn.SortSlot.SortBinding _sort -> [%type: string]
           | SortPattern _sort ->
             if info then [%type: 'info Pattern.t] else [%type: Pattern.Plain.t])
    |> Fn.flip Lvca_util.List.snoc body_type
;;

(* XXX possibility of generating two constructors with same name *)
let mk_ctor_decl
    (module Ast : Ast_builder.S)
    ~info
    ~sort_name
    (Syn.OperatorDef.OperatorDef (op_name, arity))
  =
  let loc = Ast.loc in
  let args =
    arity
    |> List.map ~f:(args_of_valence (module Ast) ~info ~sort_name)
    |> (if info then List.cons [ [%type: 'info] ] else Fn.id)
    |> List.map ~f:(function
           | [] -> [%type: unit]
           | [ ty ] -> ty
           | tys -> Ast_builder.Default.ptyp_tuple ~loc tys)
  in
  Ast.constructor_declaration
    ~name:{ txt = ctor_name op_name; loc }
    ~args:(Pcstr_tuple args)
    ~res:None
;;

type self_referential =
  | IsSelfReferential
  | IsntSelfReferential

(* Whether each sort is or is not self-referential. *)
let get_self_ref_map sort_defs =
  let sort_map = SMap.of_alist_exn sort_defs in
  sort_defs
  |> List.map ~f:(fun (sort_name, _) ->
         let seen_sorts = ref SSet.empty in
         let rec go current_sort_name =
           if Set.mem !seen_sorts current_sort_name
           then false
           else (
             seen_sorts := Set.add !seen_sorts current_sort_name;
             match Map.find sort_map current_sort_name with
             | None -> (* external *) false
             | Some (Syn.SortDef.SortDef (_, op_defs)) ->
               op_defs
               |> List.exists ~f:(fun (Syn.OperatorDef.OperatorDef (_name, arity)) ->
                      arity
                      |> List.exists
                           ~f:(fun (Syn.Valence.Valence (_sort_slots, body_sort)) ->
                             let name = sort_head body_sort in
                             String.(name = sort_name) || go name)))
         in
         let self_referential =
           if go sort_name then IsSelfReferential else IsntSelfReferential
         in
         sort_name, self_referential)
  |> SMap.of_alist_exn
;;

let mk_type_decl (module Ast : Ast_builder.S) ~info ~sort_def_map =
  let params =
    if info then [ Ast.ptyp_var "info", (NoVariance, NoInjectivity) ] else []
  in
  let decls =
    sort_def_map
    |> Map.to_alist
    |> List.map ~f:(fun (sort_name, Syn.SortDef.SortDef (_vars, op_defs)) ->
           let kind =
             Ptype_variant
               (List.map op_defs ~f:(mk_ctor_decl (module Ast) ~info ~sort_name))
           in
           Ast.type_declaration
             ~name:{ txt = sort_name; loc = Ast.loc }
             ~params
             ~cstrs:[]
             ~kind
             ~private_:Public
             ~manifest:None)
  in
  Ast.pstr_type Recursive decls
;;

module OperatorPat = struct
  type ctor_type =
    | Plain
    | WithInfo

  let is_valid_ocaml_constr_name str =
    (not (String.is_empty str)) && Char.is_uppercase str.[0]
  ;;

  let mk
      (module Ast : Ast_builder.S)
      ~ctor_type
      ?(match_info = false)
      ?(match_non_info = true)
      ?(name_base = "x")
      (Syn.OperatorDef.OperatorDef (op_name, arity))
    =
    let loc = Ast.loc in
    if not (is_valid_ocaml_constr_name op_name)
    then Location.raise_errorf ~loc "Invalid OCaml operator name: %s" op_name;
    let var_ix = ref 0 in
    let v ix = Ast.ppat_var { txt = Printf.sprintf "%s%d" name_base ix; loc } in
    let contents =
      arity
      |> List.map ~f:(fun (Syn.Valence.Valence (slots, _)) ->
             let slots =
               ( (* Always one binder for the body *) ) :: List.map slots ~f:(Fn.const ())
             in
             slots
             |> List.map ~f:(fun () ->
                    if match_non_info
                    then (
                      Int.incr var_ix;
                      v !var_ix)
                    else Ast.ppat_any))
    in
    let contents =
      match ctor_type with
      | WithInfo -> [ (if match_info then v 0 else Ast.ppat_any) ] :: contents
      | Plain -> contents
    in
    let body =
      match contents with
      | [] -> None
      | _ -> Some (contents |> List.map ~f:(mk_pat_tuple ~loc) |> mk_pat_tuple ~loc)
    in
    let txt =
      let container_name =
        match ctor_type with WithInfo -> "Types" | Plain -> "Plain"
      in
      build_names [ container_name; op_name ]
    in
    Ast.ppat_construct { txt; loc } body
  ;;
end

module OperatorExp = struct
  type mapping_rhs_ty =
    | Plain
    | WithInfo of expression

  let mk
      (module Ast : Ast_builder.S)
      ~ctor_type (* Building a plain or with-info data type *)
      ?(mk_app = fun f arg -> Ast.([%expr [%e f] [%e arg]]))
      ?(name_base = "x")
      sort_defs (* Sorts being defined together *)
      fun_name (* The name of the function being defined *)
      (Syn.OperatorDef.OperatorDef (op_name, arity))
    =
    let loc = Ast.loc in
    let var_ix = ref 0 in
    let v () = Ast.evar (Printf.sprintf "%s%d" name_base !var_ix) in
    let pattern_converter =
      Ast.pexp_ident { txt = build_names [ "Pattern"; fun_name ]; loc }
    in
    let body_arg sort =
      Int.incr var_ix;
      let f =
        let sort_name = sort_head sort in
        if Map.mem sort_defs sort_name
        then Ast.evar sort_name
        else
          (* is it always valid to just use module_name? *)
          Ast.pexp_ident
            { txt = build_names [ module_name (sort_head sort); fun_name ]; loc }
      in
      mk_app f (v ())
    in
    let contents =
      arity
      |> List.map ~f:(fun (Syn.Valence.Valence (slots, body_sort)) ->
             slots
             |> List.map ~f:(fun slot ->
                    Int.incr var_ix;
                    match slot with
                    | Syn.SortSlot.SortBinding _sort -> v ()
                    | SortPattern _ -> mk_app pattern_converter (v ()))
             |> Fn.flip Lvca_util.List.snoc (body_arg body_sort))
      |> List.map ~f:(mk_exp_tuple ~loc)
    in
    let contents =
      match ctor_type with WithInfo expr -> expr :: contents | Plain -> contents
    in
    let body =
      match contents with [] -> None | _ -> Some (contents |> mk_exp_tuple ~loc)
    in
    let txt =
      let container_name =
        match ctor_type with WithInfo _ -> "Types" | Plain -> "Plain"
      in
      build_names [ container_name; op_name ]
    in
    Ast.pexp_construct { txt; loc } body
  ;;
end

let mk_to_plain (module Ast : Ast_builder.S) sort_defs sort_name op_defs =
  let f op_def =
    let lhs = OperatorPat.mk (module Ast) ~ctor_type:WithInfo op_def in
    let rhs = OperatorExp.mk (module Ast) ~ctor_type:Plain sort_defs "to_plain" op_def in
    Ast.case ~lhs ~guard ~rhs
  in
  let body = op_defs |> List.map ~f |> Ast.pexp_function in
  Ast.value_binding ~pat:Ast.(ppat_var { txt = sort_name; loc }) ~expr:body
;;

let mk_of_plain (module Ast : Ast_builder.S) sort_defs sort_name op_defs =
  let f op_def =
    let lhs = OperatorPat.mk (module Ast) ~ctor_type:Plain op_def in
    let rhs =
      OperatorExp.mk
        (module Ast)
        ~ctor_type:Ast.(WithInfo [%expr ()])
        sort_defs
        "of_plain"
        op_def
    in
    Ast.case ~lhs ~guard ~rhs
  in
  let body = op_defs |> List.map ~f |> Ast.pexp_function in
  Ast.(value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr:body)
;;

let mk_map_info (module Ast : Ast_builder.S) sort_defs sort_name op_defs =
  let f op_def =
    let lhs = OperatorPat.mk (module Ast) ~ctor_type:WithInfo ~match_info:true op_def in
    let rhs =
      OperatorExp.mk
        (module Ast)
        ~ctor_type:Ast.(WithInfo [%expr f x0])
        ~mk_app:Ast.(fun f arg -> [%expr [%e f] ~f [%e arg]])
        sort_defs
        "map_info"
        op_def
    in
    Ast.case ~lhs ~guard ~rhs
  in
  let body =
    let open Ast in
    op_defs
    |> List.map ~f
    |> pexp_function
    |> pexp_fun (Labelled "f") None (ppat_var { txt = "f"; loc })
  in
  Ast.(value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr:body)
;;

let mk_equal (module Ast : Ast_builder.S) _sort_defs sort_name op_defs =
  let loc = Ast.loc in
  let same_sort sort = String.(sort_head sort = sort_name) in
  let f (Syn.OperatorDef.OperatorDef (_op_name, arity) as op_def) =
    let lhs =
      let p1, p2 =
        ("x", "y")
        |> Lvca_util.Tuple2.map ~f:(fun name_base ->
               OperatorPat.mk
                 (module Ast)
                 ~ctor_type:WithInfo
                 ~match_info:true
                 ~name_base
                 op_def)
      in
      [%pat? [%p p1], [%p p2]]
    in
    let rhs =
      let arity_exps =
        let var_ix = ref 0 in
        let mk_xy () =
          ("x", "y")
          |> Lvca_util.Tuple2.map ~f:(fun base ->
                 Ast.evar (Printf.sprintf "%s%d" base !var_ix))
        in
        arity
        |> List.map ~f:(fun (Syn.Valence.Valence (slots, body_sort)) ->
               let slots_checks =
                 slots
                 |> List.map ~f:(fun slot ->
                        Int.incr var_ix;
                        let x, y = mk_xy () in
                        match slot with
                        | Syn.SortSlot.SortBinding _sort ->
                          [%expr Base.String.( = ) [%e x] [%e y]]
                        | SortPattern _ -> [%expr Pattern.equal ~info_eq [%e x] [%e y]])
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
      conjuntion ~loc ([%expr info_eq x0 y0] :: arity_exps)
    in
    Ast.case ~lhs ~guard ~rhs
  in
  let branches = List.map op_defs ~f in
  let branches =
    match op_defs with
    | [] | [ _ ] -> branches
    | _ ->
      (* If there's more than one operator we need to add an extra case *)
      let last_branch = Ast.case ~lhs:[%pat? _, _] ~guard ~rhs:[%expr false] in
      Lvca_util.List.snoc branches last_branch
  in
  let expr =
    let open Ast in
    branches
    |> pexp_match [%expr t1, t2]
    |> pexp_fun Nolabel None (ppat_var { txt = "t2"; loc })
    |> pexp_fun Nolabel None (ppat_var { txt = "t1"; loc })
    |> pexp_fun (Labelled "info_eq") None (ppat_var { txt = "info_eq"; loc })
  in
  Ast.(value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr)
;;

let mk_info (module Ast : Ast_builder.S) _sort_defs sort_name op_defs =
  let f op_def =
    let lhs =
      OperatorPat.mk
        (module Ast)
        ~ctor_type:WithInfo
        ~match_info:true
        ~match_non_info:false
        op_def
    in
    Ast.(case ~lhs ~guard ~rhs:[%expr x0])
  in
  let body = op_defs |> List.map ~f |> Ast.pexp_function in
  Ast.(value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr:body)
;;

let mk_pstr_value (module Ast : Ast_builder.S) value_bindings =
  let is_rec =
    (* TODO: this is wrong: do the correct check *)
    match value_bindings with [ _ ] -> Nonrecursive | _ -> Recursive
  in
  Ast.pstr_value is_rec value_bindings
;;

let mk_wrapper_module (module Ast : Ast_builder.S) sort_defs =
  let loc = Ast.loc in
  let sort_def_map = SMap.of_alist_exn sort_defs in
  let adapt
      (maker :
        (module Ast_builder.S)
        -> _ Syn.SortDef.t SMap.t
        -> string
        -> _ Syn.OperatorDef.t list
        -> Ppxlib.value_binding)
    =
    sort_defs
    |> List.map ~f:(fun (sort_name, Syn.SortDef.SortDef (_vars, op_defs)) ->
           maker (module Ast) sort_def_map sort_name op_defs)
    (* TODO: must find SCCs and topologically sort. *)
    |> mk_pstr_value (module Ast)
  in
  let defs =
    (* Each of these functions is potentially recursive (across multiple types), pre-declare. *)
    [%str
      module Types = struct
        [%%i mk_type_decl (module Ast) ~info:true ~sort_def_map]
      end

      module Plain = struct
        [%%i mk_type_decl (module Ast) ~info:false ~sort_def_map]
      end

      module Info = struct
        [%%i adapt mk_info]
      end

      module ToPlain = struct
        [%%i adapt mk_to_plain]
      end

      module OfPlain = struct
        [%%i adapt mk_of_plain]
      end

      module Equal = struct
        [%%i adapt mk_equal]
      end

      module MapInfo = struct
        [%%i adapt mk_map_info]
      end

      (*
      TODO:
      module PpGeneric = struct end
      module OfNominal = struct end
      module ToNominal = struct end
      module Jsonify = struct end
      module Unjsonify = struct end
      module Select = struct end
      module SubstAll = struct end

      TODO: individual type modules
      *)]
  in
  let init = Ast.pmod_structure defs in
  Ast.module_binding
    ~name:{ txt = Some "Wrapper" (* (module_name sort_name) *); loc }
    ~expr:init
  |> Ast.pstr_module
;;

let mk_type_module
    (module Ast : Ast_builder.S)
    sort_name
    (Syn.SortDef.SortDef (vars, op_defs))
  =
  let loc = Ast.loc in
  (* Turn into a functor over type args *)
  let f (name, kind_opt) accum =
    match kind_opt with
    | None (* XXX should do kind inference instead of assuming it's * *)
    | Some (Syn.Kind.Kind (_, 1)) ->
      let mod_param =
        Named
          ( { txt = Some (module_name name); loc }
          , Ast.pmty_ident { txt = build_names [ "LanguageObject"; "AllTermS" ]; loc } )
      in
      Ast.pmod_functor mod_param accum
    | Some kind ->
      Location.raise_errorf
        ~loc
        "Code generation currently only supports external modules of kind * (%s is %s)"
        name
        (Fmt.to_to_string Syn.Kind.pp kind)
  in
  let plain_type_decl =
    let kind =
      Ptype_variant
        (List.map op_defs ~f:(mk_ctor_decl (module Ast) ~info:false ~sort_name))
    in
    Ast.pstr_type
      Recursive
      [ Ast.type_declaration
          ~name:{ txt = "t"; loc = Ast.loc }
          ~params:[]
          ~cstrs:[]
          ~kind
          ~private_:Public
          ~manifest:
            (Some (Ast.ptyp_constr { txt = build_names [ "Plain"; sort_name ]; loc } []))
      ]
  in
  let info_type_decl =
    let kind =
      Ptype_variant
        (List.map op_defs ~f:(mk_ctor_decl (module Ast) ~info:true ~sort_name))
    in
    Ast.pstr_type
      Recursive
      [ Ast.type_declaration
          ~name:{ txt = "t"; loc = Ast.loc }
          ~params:[ Ast.ptyp_var "info", (NoVariance, NoInjectivity) ]
          ~cstrs:[]
          ~kind
          ~private_:Public
          ~manifest:
            (Some
               (Ast.ptyp_constr
                  { txt = build_names [ "Types"; sort_name ]; loc }
                  [ Ast.ptyp_var "info" ]))
      ]
  in
  let fun_defs =
    [ "info", "Info"
    ; "to_plain", "ToPlain"
    ; "of_plain", "OfPlain"
    ; "equal", "Equal"
    ; "map_info", "MapInfo"
    ]
    |> List.map ~f:(fun (fun_name, mod_name) ->
           let open Ast in
           pstr_value
             Nonrecursive
             [ value_binding
                 ~pat:(ppat_var { txt = fun_name; loc })
                 ~expr:(pexp_ident { txt = build_names [ mod_name; sort_name ]; loc })
             ])
  in
  let init =
    Ast.pmod_structure
      ([ Ast.module_binding
           ~name:{ txt = Some "Plain"; loc }
           ~expr:(Ast.pmod_structure [ plain_type_decl ])
         |> Ast.pstr_module
       ]
      @ info_type_decl :: fun_defs)
  in
  let expr = List.fold_right vars ~init ~f in
  Ast.module_binding ~name:{ txt = Some (module_name sort_name); loc } ~expr
  |> Ast.pstr_module
;;

let mk_container_module ~loc Syn.{ externals; sort_defs } =
  let (module Ast) = Ast_builder.make loc in
  (* pre-declare types *)
  let wrapper_module = mk_wrapper_module (module Ast) sort_defs in
  let type_modules =
    List.map sort_defs ~f:(Util.Tuple2.uncurry (mk_type_module (module Ast)))
  in
  (*
  let sort_defs =
    [%str let language = [%e SyntaxQuoter.mk_language ~loc lang]] @ sort_defs
  in
  *)
  (* Turn into a functor over externals *)
  let f (name, kind) accum =
    match kind with
    | Syn.Kind.Kind (_, 1) ->
      let mod_param =
        Named
          ( { txt = Some (module_name name); loc }
          , Ast.pmty_ident { txt = build_names [ "LanguageObject"; "AllTermS" ]; loc } )
      in
      Ast.pmod_functor mod_param accum
    | _ ->
      Location.raise_errorf
        ~loc
        "Code generation currently only supports external modules of kind * (%s is %s)"
        name
        (Fmt.to_to_string Syn.Kind.pp kind)
  in
  List.fold_right externals ~init:(Ast.pmod_structure (wrapper_module :: type_modules)) ~f
;;

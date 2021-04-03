open Base
open Lvca_syntax
open Ppxlib
module Util = Lvca_util
module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

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
    (AbstractSyntax.Valence.Valence (binding_sort_slots, body_sort))
  =
  let loc = Ast.loc in
  let body_type = ptyp_of_sort (module Ast) ~sort_name ~info body_sort in
  match binding_sort_slots with
  | [] -> [ body_type ]
  | _ ->
    binding_sort_slots
    |> List.map ~f:(function
           | AbstractSyntax.SortSlot.SortBinding _sort -> [%type: string]
           | SortPattern _sort ->
             if info
             then [%type: ('info, Lvca_util.Void.t) Pattern.t]
             else [%type: Lvca_util.Void.t Pattern.Plain.t])
    |> Fn.flip Lvca_util.List.snoc body_type
;;

(* XXX possibility of generating two constructors with same name *)
let mk_ctor_decl
    (module Ast : Ast_builder.S)
    ~info
    ~sort_name
    (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
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

[%%if ocaml_version < (4, 12, 0)]

let novariance = Invariant

[%%else]

let novariance = NoVariance

[%%endif]

let mk_type_decl (module Ast : Ast_builder.S) ~info ~sort_name op_defs =
  let params = if info then [ Ast.ptyp_var "info", novariance ] else [] in
  let kind =
    Ptype_variant (List.map op_defs ~f:(mk_ctor_decl (module Ast) ~info ~sort_name))
  in
  let type_decl =
    Ast.type_declaration
      ~name:{ txt = "t"; loc = Ast.loc }
      ~params
      ~cstrs:[]
      ~kind
      ~private_:Public
      ~manifest:None
  in
  Ast.pstr_type Recursive [ type_decl ]
;;

module OperatorPat = struct
  type ctor_type =
    | Plain
    | WithInfo

  let mk
      (module Ast : Ast_builder.S)
      ~ctor_type
      ?(match_info = false)
      ?(match_non_info = true)
      ?(name_base = "x")
      (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
    =
    let loc = Ast.loc in
    let var_ix = ref 0 in
    let v ix = Ast.ppat_var { txt = Printf.sprintf "%s%d" name_base ix; loc } in
    let contents =
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
      match ctor_type with
      | WithInfo -> Lident op_name
      | Plain -> build_names [ "Plain"; op_name ]
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
      sort_name (* The name of the sort being defined *)
      fun_name (* The name of the function being defined *)
      (AbstractSyntax.OperatorDef.OperatorDef (op_name, arity))
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
        if String.(sort_head sort = sort_name)
        then Ast.evar fun_name
        else
          (* is it always valid to just use module_name? *)
          Ast.pexp_ident
            { txt = build_names [ module_name (sort_head sort); fun_name ]; loc }
      in
      mk_app f (v ())
    in
    let contents =
      arity
      |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
             slots
             |> List.map ~f:(fun slot ->
                    Int.incr var_ix;
                    match slot with
                    | AbstractSyntax.SortSlot.SortBinding _sort -> v ()
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
      match ctor_type with
      | WithInfo _ -> Lident op_name
      | Plain -> build_names [ "Plain"; op_name ]
    in
    Ast.pexp_construct { txt; loc } body
  ;;
end

let mk_to_plain (module Ast : Ast_builder.S) sort_name op_defs =
  let f op_def =
    let lhs = OperatorPat.mk (module Ast) ~ctor_type:WithInfo op_def in
    let rhs = OperatorExp.mk (module Ast) ~ctor_type:Plain sort_name "to_plain" op_def in
    Ast.case ~lhs ~guard ~rhs
  in
  op_defs |> List.map ~f |> Ast.pexp_function
;;

let mk_of_plain (module Ast : Ast_builder.S) sort_name op_defs =
  let f op_def =
    let lhs = OperatorPat.mk (module Ast) ~ctor_type:Plain op_def in
    let rhs =
      OperatorExp.mk
        (module Ast)
        ~ctor_type:Ast.(WithInfo [%expr ()])
        sort_name
        "of_plain"
        op_def
    in
    Ast.case ~lhs ~guard ~rhs
  in
  op_defs |> List.map ~f |> Ast.pexp_function
;;

let mk_map_info (module Ast : Ast_builder.S) sort_name op_defs =
  let f op_def =
    let lhs = OperatorPat.mk (module Ast) ~ctor_type:WithInfo ~match_info:true op_def in
    let rhs =
      OperatorExp.mk
        (module Ast)
        ~ctor_type:Ast.(WithInfo [%expr f x0])
        ~mk_app:Ast.(fun f arg -> [%expr [%e f] ~f [%e arg]])
        sort_name
        "map_info"
        op_def
    in
    Ast.case ~lhs ~guard ~rhs
  in
  op_defs |> List.map ~f |> Ast.pexp_function
;;

let mk_equal (module Ast : Ast_builder.S) sort_name op_defs =
  let loc = Ast.loc in
  let same_sort sort = String.(sort_head sort = sort_name) in
  let f (AbstractSyntax.OperatorDef.OperatorDef (_op_name, arity) as op_def) =
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
        |> List.map ~f:(fun (AbstractSyntax.Valence.Valence (slots, body_sort)) ->
               let slots_checks =
                 slots
                 |> List.map ~f:(fun slot ->
                        Int.incr var_ix;
                        let x, y = mk_xy () in
                        match slot with
                        | AbstractSyntax.SortSlot.SortBinding _sort ->
                          [%expr Base.String.( = ) [%e x] [%e y]]
                        | SortPattern _ ->
                          [%expr
                            Pattern.equal
                              ~info_eq
                              ~prim_eq:Lvca_util.Void.( = )
                              [%e x]
                              [%e y]])
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
  let last_branch = Ast.case ~lhs:[%pat? _, _] ~guard ~rhs:[%expr false] in
  let branches = op_defs |> List.map ~f |> Fn.flip Lvca_util.List.snoc last_branch in
  Ast.pexp_match [%expr t1, t2] branches
;;

let mk_info (module Ast : Ast_builder.S) op_defs =
  let f op_def =
    let lhs =
      OperatorPat.mk
        (module Ast)
        ~ctor_type:WithInfo
        ~match_info:true
        ~match_non_info:false
        op_def
    in
    Ast.case ~lhs ~guard ~rhs:Ast.([%expr x0])
  in
  op_defs |> List.map ~f |> Ast.pexp_function
;;

let mk_sort_module
    (module Ast : Ast_builder.S)
    sort_name
    (AbstractSyntax.SortDef.SortDef (vars, op_defs))
  =
  let loc = Ast.loc in
  let expr =
    Ast.pmod_structure [ mk_type_decl (module Ast) ~info:false ~sort_name op_defs ]
  in
  (*
    let pp_generic = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let of_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    let to_nominal = { pstr_desc = Pstr_value (Recursive, []); pstr_loc = loc } in
    select?
    *)
  let init =
    Ast.pmod_structure
      ([ mk_type_decl (module Ast) ~info:true ~sort_name op_defs
       ; Ast.module_binding ~name:{ txt = Some "Plain"; loc } ~expr |> Ast.pstr_module
       ]
      @ [%str
          let rec to_plain = [%e mk_to_plain (module Ast) sort_name op_defs]
          let rec of_plain = [%e mk_of_plain (module Ast) sort_name op_defs]
          let rec equal ~info_eq t1 t2 = [%e mk_equal (module Ast) sort_name op_defs]
          let info = [%e mk_info (module Ast) op_defs]
          let rec map_info ~f = [%e mk_map_info (module Ast) sort_name op_defs]])
  in
  (* Turn into a functor over type args *)
  let f (name, kind_opt) accum =
    match kind_opt with
    | None (* XXX should do kind inference instead of assuming it's * *)
    | Some (AbstractSyntax.Kind.Kind (_, 1)) ->
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
        (Fmt.to_to_string AbstractSyntax.Kind.pp kind)
  in
  let expr = List.fold_right vars ~init ~f in
  Ast.module_binding ~name:{ txt = Some (module_name sort_name); loc } ~expr
  |> Ast.pstr_module
;;

let mk_container_module ~loc (AbstractSyntax.{ externals; sort_defs } as lang) =
  let (module Ast) = Ast_builder.make loc in
  let sort_defs =
    List.map sort_defs ~f:(Util.Tuple2.uncurry (mk_sort_module (module Ast)))
  in
  let sort_defs =
    [%str let language = [%e SyntaxQuoter.mk_language ~loc lang]] @ sort_defs
  in
  (* Turn into a functor over externals *)
  let f (name, kind) accum =
    match kind with
    | AbstractSyntax.Kind.Kind (_, 1) ->
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
        (Fmt.to_to_string AbstractSyntax.Kind.pp kind)
  in
  List.fold_right externals ~init:(Ast.pmod_structure sort_defs) ~f
;;

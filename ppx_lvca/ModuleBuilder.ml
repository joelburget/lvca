open Base
open Lvca_syntax
open Ppxlib
module Util = Lvca_util
module SSet = Util.String.Set
module SMap = Util.String.Map
module Syn = AbstractSyntax
module ParseAbstract = Syn.Parse (ParseUtil.CComment)

let ( >> ), ( << ) = Util.(( >> ), ( << ))

(*
  ## Sort variables

  Example definition: [pair a b := Pair(a; b)]. We have two choices for how to
  translate this to OCaml:

  1. type variables

  We end up with something like [type ('a, 'b) pair = Pair of 'a * 'b] (or
  [type ('info, 'a, 'b) pair = Pair of 'info * 'a * 'b]). This is
  great but we can't easily define helpers:

    [let of_plain = function Plain.Pair (x1, x2) -> Types.Pair ((), ???, ???)]

  2. functor

  We define

      module Pair (A : LanguageObject_intf.S) (B : LanguageObject_intf.S) = struct
        type 'info t = Pair of 'info * 'info A.t * 'info B.t
        ...
      end

  This might work actually? Except it's kind of a massive pain given that we
  define all types together and all modules separately.

  3. hybrid

      integer : *
      string : *
      pair a b := Pair(a; b)
      uses_pair := UsesPair(pair integer string)
      diag a := Diag(pair a a)

  ==>

      module Types = struct
        type ('info, 'a, 'b) pair = Pair of 'info * 'a * 'b
        type 'info uses_pair = UsesPair of 'info * ('info, Integer.t, String.t) pair
        type ('info, 'a) diag = Diag of ('info, 'a, 'a) pair
      end

      module OfPlain = struct
        let pair f_a f_b = function
          | Plain.Pair (x1, x2) -> Types.Pair ((), f_a x1, f_b x2)
        let uses_pair = function
          | Plain.UsesPair x1 ->
              Types.UsesPair ((), pair Integer.of_plain String.of_plain x1)
        let diag f_a = function
          | Plain.Diag x1 -> Types.Diag ((), pair f_a f_a x1)
      end

      module Pair (A : LanguageObject_intf.S) (B : LanguageObject_intf.S) = struct
        type 'info t = ('info, 'info A.t, 'info B.t) Types.pair =
          | Pair of 'info * 'info A.t * 'info B.t
        let of_plain = OfPlain.pair A.of_plain B.of_plain
      end

      module UsesPair = struct
        type 'info uses_pair = ('info, 'info Integer.t, 'info String.t) Types.uses_pair =
          | UsesPair of 'info * ('info, Integer.t, 'info String.t) pair
        let of_plain = OfPlain.uses_pair
      end

      module Diag (A : LanguageObject_intf.S) = struct
        type 'info diag = ('info, 'info A.t) Types.diag =
          | Diag of 'info * ('info, 'info A.t, 'info A.t) pair
        let of_plain = OfPlain.diag A.of_plain
      end

  ## Handling of sorts

  There are a few factors determining the definition and use of a given sort:
    * The use of 'info or not
    * Whether the sort:
      - an external
      - a variable
      - or defined in this language, in which case, whether
        + the sort is part of the SCC we're currently defining
        + or, was previously defined
    * Whether we're using the sort in the [Types] module or in an individual
      sort definition module
*)

type conversion_direction =
  | ToPlain
  | OfPlain

(* Concatenate a list of names into a Longident. *)
(* TODO: is this just Longident.unflatten? *)
let unflatten names =
  match names with
  | [] -> Lvca_util.invariant_violation "unflatten: names must be nonempty"
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

let labelled_fun (module Ast : Ast_builder.S) name =
  Ast.(pexp_fun (Labelled name) None (ppat_var { txt = name; loc }))
;;

let labelled_arg (module Ast : Ast_builder.S) name =
  Ast.(Labelled name, pexp_ident { txt = Lident name; loc })
;;

let conjuntion ~loc exps =
  let exps, last = Lvca_util.List.unsnoc exps in
  List.fold_right exps ~init:last ~f:(fun e1 e2 -> [%expr [%e e1] && [%e e2]])
;;

type typedef_mode =
  | TypesModule
  | IndividualTypeModule

module CtorDecl (Ast : Ast_builder.S) = struct
  let mk
      ~info
      ~typedef_mode
      ~var_names
      ~mutual_sorts
      ~prim_names
      (Syn.OperatorDef.OperatorDef (op_name, arity))
    =
    let open Ast in
    let pattern_type =
      if info then [%type: 'info Pattern.t] else [%type: Pattern.Plain.t]
    in
    let ptyp_of_sort sort =
      let name = sort_head sort in
      let args = if info then [ [%type: 'info] ] else [] in
      match Set.mem var_names name with
      | true ->
        (match typedef_mode with
        | TypesModule -> ptyp_var name
        | IndividualTypeModule ->
          let names =
            if info then [ module_name name; "t" ] else [ module_name name; "Plain"; "t" ]
          in
          ptyp_constr { txt = unflatten names; loc } args)
      | false ->
        (match Map.find mutual_sorts name with
        | Some (Syn.SortDef.SortDef (vars, _op_defs)) ->
          let qualified_name, extra_args =
            match typedef_mode with
            | TypesModule ->
              let extra_args =
                List.map vars ~f:(fun (var_name, _) -> ptyp_var var_name)
              in
              [ name ], extra_args
            | IndividualTypeModule ->
              (match info with
              | true ->
                let extra_args =
                  List.map vars ~f:(fun (var_name, _) ->
                      let txt = unflatten [ module_name var_name; "t" ] in
                      ptyp_constr { txt; loc } args)
                in
                [ "Wrapper"; "Types"; name ], extra_args
              | false ->
                let extra_args =
                  List.map vars ~f:(fun (var_name, _) ->
                      let txt = unflatten [ module_name var_name; "Plain"; "t" ] in
                      ptyp_constr { txt; loc } args)
                in
                [ "Wrapper"; "Plain"; name ], extra_args)
          in
          ptyp_constr { txt = unflatten qualified_name; loc } (args @ extra_args)
        | None ->
          let qualified_name =
            match Set.mem prim_names name, info with
            | true, true -> [ module_name name; "t" ]
            | true, false -> [ module_name name; "Plain"; "t" ]
            | false, true -> [ "Wrapper"; "Types"; name ]
            | false, false -> [ "Wrapper"; "Plain"; name ]
          in
          ptyp_constr { txt = unflatten qualified_name; loc } args)
    in
    let args_of_valence (Syn.Valence.Valence (binding_sort_slots, body_sort)) =
      let args =
        match binding_sort_slots with
        | [] -> []
        | _ ->
          binding_sort_slots
          |> List.map ~f:(function
                 | Syn.SortSlot.SortBinding _sort -> [%type: string]
                 | SortPattern _sort -> pattern_type)
      in
      args @ [ ptyp_of_sort body_sort ]
    in
    let args =
      arity
      |> List.map ~f:args_of_valence
      |> (if info then List.cons [ [%type: 'info] ] else Fn.id)
      |> List.map ~f:(function
             | [] -> [%type: unit]
             | [ ty ] -> ty
             | tys -> Ast_builder.Default.ptyp_tuple ~loc tys)
    in
    constructor_declaration
      ~name:{ txt = ctor_name op_name; loc }
      ~args:(Pcstr_tuple args)
      ~res:None
  ;;
end

let get_sort_ref_map sort_defs =
  let sort_set = sort_defs |> List.map ~f:fst |> SSet.of_list in
  sort_defs
  |> List.map ~f:(fun (sort_name, Syn.SortDef.SortDef (vars, op_defs)) ->
         let vars = List.map vars ~f:fst in
         let connections =
           op_defs
           |> List.map ~f:(fun (Syn.OperatorDef.OperatorDef (_name, arity)) ->
                  arity
                  |> List.filter_map
                       ~f:(fun (Syn.Valence.Valence (_sort_slots, body_sort)) ->
                         let sort_name = sort_head body_sort in
                         (* Only add if it's a known sort name (not an external) and not shadowed by a var. *)
                         if Set.mem sort_set sort_name
                            && not (List.mem vars sort_name ~equal:String.( = ))
                         then Some sort_name
                         else None)
                  |> SSet.of_list)
           |> SSet.union_list
           |> Set.to_list
         in
         sort_name, connections)
  |> SMap.of_alist_exn
;;

type is_rec =
  | IsRec
  | IsntRec

module TypeDecls (Ast : Ast_builder.S) = struct
  module CtorDecl = CtorDecl (Ast)

  let mk ~info ~sort_def_map ~prim_names =
    let params0 =
      if info then [ Ast.ptyp_var "info", (NoVariance, NoInjectivity) ] else []
    in
    sort_def_map
    |> Map.to_alist
    |> List.map ~f:(fun (sort_name, Syn.SortDef.SortDef (vars, op_defs)) ->
           let var_names = vars |> List.map ~f:fst |> SSet.of_list in
           let mk_var name = Ast.ptyp_var name, (NoVariance, NoInjectivity) in
           let params = vars |> List.map ~f:fst |> List.map ~f:mk_var in
           let params = params0 @ params in
           let kind =
             Ptype_variant
               (List.map
                  op_defs
                  ~f:
                    (CtorDecl.mk
                       ~info
                       ~typedef_mode:TypesModule
                       ~var_names
                       ~mutual_sorts:sort_def_map
                       ~prim_names))
           in
           Ast.type_declaration
             ~name:{ txt = sort_name; loc = Ast.loc }
             ~params
             ~cstrs:[]
             ~kind
             ~private_:Public
             ~manifest:None)
  ;;
end

module OperatorPat (Ast : Ast_builder.S) = struct
  type ctor_type =
    | Plain
    | WithInfo

  let is_valid_ocaml_constr_name str =
    (not (String.is_empty str)) && Char.is_uppercase str.[0]
  ;;

  let mk
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
      unflatten [ container_name; op_name ]
    in
    Ast.ppat_construct { txt; loc } body
  ;;
end

module OperatorExp (Ast : Ast_builder.S) = struct
  type mapping_rhs_ty =
    | Plain
    | WithInfo of expression

  let mk
      ~ctor_type (* Building a plain or with-info data type *)
      ~var_names
      ?(mk_app = fun f args -> Ast.(pexp_apply f args))
      ?(name_base = "x")
      sort_defs (* Sorts being defined together *)
      fun_name (* The name of the function being defined *)
      (Syn.OperatorDef.OperatorDef (op_name, arity))
    =
    let loc = Ast.loc in
    let var_ix = ref 0 in
    let v () = Ast.evar (Printf.sprintf "%s%d" name_base !var_ix) in
    let pattern_converter =
      Ast.pexp_ident { txt = unflatten [ "Pattern"; fun_name ]; loc }
    in
    let body_arg sort =
      Int.incr var_ix;
      let f, var_args =
        let sort_name = sort_head sort in
        match Map.find sort_defs sort_name with
        | Some (Syn.SortDef.SortDef (vars, _)) ->
          let var_args =
            vars
            |> List.map ~f:fst
            |> List.map ~f:(fun sort_name -> Nolabel, Ast.evar ("f_" ^ sort_name))
          in
          Ast.evar sort_name, var_args
        | None when Set.mem var_names sort_name ->
          Ast.pexp_ident { txt = Lident ("f_" ^ sort_name); loc }, []
        | None ->
          (* is it always valid to just use module_name? *)
          ( Ast.pexp_ident
              { txt = unflatten [ module_name (sort_head sort); fun_name ]; loc }
          , [] )
      in
      mk_app f (var_args @ [ Nolabel, v () ])
    in
    let contents =
      arity
      |> List.map ~f:(fun (Syn.Valence.Valence (slots, body_sort)) ->
             slots
             |> List.map ~f:(fun slot ->
                    Int.incr var_ix;
                    match slot with
                    | Syn.SortSlot.SortBinding _sort -> v ()
                    | SortPattern _ -> mk_app pattern_converter [ Nolabel, v () ])
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
      unflatten [ container_name; op_name ]
    in
    Ast.pexp_construct { txt; loc } body
  ;;
end

module ToPlain (Ast : Ast_builder.S) = struct
  module OperatorPat = OperatorPat (Ast)
  module OperatorExp = OperatorExp (Ast)

  let mk sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = OperatorPat.mk ~ctor_type:WithInfo op_def in
      let rhs = OperatorExp.mk ~var_names ~ctor_type:Plain sort_defs "to_plain" op_def in
      Ast.case ~lhs ~guard ~rhs
    in
    let init = op_defs |> List.map ~f |> Ast.pexp_function in
    let f (var_name, _kind_opt) body =
      Ast.(pexp_fun Nolabel None (ppat_var { txt = "f_" ^ var_name; loc }) body)
    in
    let expr = List.fold_right vars ~init ~f in
    Ast.value_binding ~pat:Ast.(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module OfPlain (Ast : Ast_builder.S) = struct
  module OperatorPat = OperatorPat (Ast)
  module OperatorExp = OperatorExp (Ast)

  let mk sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = OperatorPat.mk ~ctor_type:Plain op_def in
      let rhs =
        OperatorExp.mk
          ~var_names
          ~ctor_type:Ast.(WithInfo [%expr ()])
          sort_defs
          "of_plain"
          op_def
      in
      Ast.case ~lhs ~guard ~rhs
    in
    let init = op_defs |> List.map ~f |> Ast.pexp_function in
    let f (var_name, _kind_opt) body =
      Ast.(pexp_fun Nolabel None (ppat_var { txt = "f_" ^ var_name; loc }) body)
    in
    let expr = List.fold_right vars ~init ~f in
    Ast.(value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr)
  ;;
end

module MapInfo (Ast : Ast_builder.S) = struct
  module OperatorPat = OperatorPat (Ast)
  module OperatorExp = OperatorExp (Ast)

  let mk sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = OperatorPat.mk ~ctor_type:WithInfo ~match_info:true op_def in
      let rhs =
        OperatorExp.mk
          ~var_names
          ~ctor_type:Ast.(WithInfo [%expr f x0])
          ~mk_app:Ast.(fun f args -> pexp_apply f (labelled_arg (module Ast) "f" :: args))
          sort_defs
          "map_info"
          op_def
      in
      Ast.case ~lhs ~guard ~rhs
    in
    let init =
      op_defs |> List.map ~f |> Ast.pexp_function |> labelled_fun (module Ast) "f"
    in
    let f (var_name, _kind_opt) body =
      Ast.(pexp_fun Nolabel None (ppat_var { txt = "f_" ^ var_name; loc }) body)
    in
    let expr = List.fold_right vars ~init ~f in
    Ast.(value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr)
  ;;
end

module Equal (Ast : Ast_builder.S) = struct
  module OperatorPat = OperatorPat (Ast)

  let mk sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let loc = Ast.loc in
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f (Syn.OperatorDef.OperatorDef (_op_name, arity) as op_def) =
      let lhs =
        let p1, p2 =
          ("x", "y")
          |> Lvca_util.Tuple2.map ~f:(fun name_base ->
                 OperatorPat.mk ~ctor_type:WithInfo ~match_info:true ~name_base op_def)
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
                   let txt, var_args =
                     (* Defined sort vs external *)
                     let sort_name = sort_head body_sort in
                     match Map.find sort_defs sort_name with
                     | Some (Syn.SortDef.SortDef (vars, _)) ->
                       let var_args =
                         vars
                         |> List.map ~f:fst
                         |> List.map ~f:(fun sort_name ->
                                Nolabel, Ast.evar ("f_" ^ sort_name))
                       in
                       Lident sort_name, var_args
                     | _ when Set.mem var_names sort_name -> Lident ("f_" ^ sort_name), []
                     | _ -> unflatten [ module_name sort_name; "equal" ], []
                   in
                   let ident = Ast.pexp_ident { txt; loc } in
                   let x, y = mk_xy () in
                   Ast.pexp_apply
                     ident
                     (var_args
                     @ [ labelled_arg (module Ast) "info_eq"; Nolabel, x; Nolabel, y ])
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
    let init =
      let open Ast in
      branches
      |> pexp_match [%expr t1, t2]
      |> pexp_fun Nolabel None (ppat_var { txt = "t2"; loc })
      |> pexp_fun Nolabel None (ppat_var { txt = "t1"; loc })
      |> labelled_fun (module Ast) "info_eq"
    in
    let f (var_name, _kind_opt) body =
      Ast.(pexp_fun Nolabel None (ppat_var { txt = "f_" ^ var_name; loc }) body)
    in
    let expr = List.fold_right vars ~init ~f in
    Ast.(value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr)
  ;;
end

module Info (Ast : Ast_builder.S) = struct
  module OperatorPat = OperatorPat (Ast)
  open Ast

  let mk _sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let mk_case op_def =
      let lhs =
        OperatorPat.mk ~ctor_type:WithInfo ~match_info:true ~match_non_info:false op_def
      in
      case ~lhs ~guard ~rhs:[%expr x0]
    in
    let expr =
      let init = op_defs |> List.map ~f:mk_case |> pexp_function in
      let mk_var_arg (var_name, _kind_opt) body =
        pexp_fun Nolabel None (ppat_var { txt = "_f_" ^ var_name; loc }) body
      in
      List.fold_right vars ~init ~f:mk_var_arg
    in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

(* The wrapper module holds TODO *)
module WrapperModule (Ast : Ast_builder.S) = struct
  module Graph = DirectedGraph.F (Base.String)
  module TypeDecls = TypeDecls (Ast)
  module Info = Info (Ast)
  module ToPlain = ToPlain (Ast)
  module OfPlain = OfPlain (Ast)
  module Equal = Equal (Ast)
  module MapInfo = MapInfo (Ast)

  let mk ~prim_names sort_defs =
    let loc = Ast.loc in
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let sort_ref_map = get_sort_ref_map sort_defs in
    let Graph.ConnectedComponents.{ scc_graph; sccs } =
      Graph.connected_components sort_ref_map
    in
    let ordered_sccs =
      scc_graph
      |> DirectedGraph.Int.topsort_exn
      |> List.map ~f:(Map.find_exn sccs)
      |> List.map
           ~f:
             (Set.to_list
             >> List.map ~f:(fun name -> name, Map.find_exn sort_def_map name))
      |> List.rev
    in
    let adapt
        ?definite_rec
        (maker :
          _ Syn.SortDef.t SMap.t
          -> string
          -> _ Syn.SortDef.t (* -> _ Syn.OperatorDef.t list *)
          -> Ppxlib.value_binding)
      =
      ordered_sccs
      |> List.map ~f:(fun scc ->
             let value_bindings =
               List.map scc ~f:(fun (sort_name, sort_def) ->
                   maker sort_def_map sort_name sort_def)
             in
             let is_rec =
               match definite_rec with
               | Some is_rec -> is_rec
               | None ->
                 (* Recursive if scc is more than one component or if self-referential. *)
                 (match scc with
                 | [ (sort_name, _) ] ->
                   (match Map.find sort_ref_map sort_name with
                   | Some [ sort_name' ] when String.(sort_name' = sort_name) -> Recursive
                   | _ -> Nonrecursive)
                 | _ -> Recursive)
             in
             Ast.pstr_value is_rec value_bindings)
    in
    let info_decls = TypeDecls.mk ~info:true ~sort_def_map ~prim_names in
    let plain_decls = TypeDecls.mk ~info:false ~sort_def_map ~prim_names in
    let defs =
      (* Each of these functions is potentially recursive (across multiple types), pre-declare. *)
      [%str
        module Types = struct
          [%%i Ast.pstr_type Recursive info_decls]
        end

        module Plain = struct
          [%%i Ast.pstr_type Recursive plain_decls]
        end

        module Info = [%m Ast.pmod_structure (adapt ~definite_rec:Nonrecursive Info.mk)]
        module ToPlain = [%m Ast.pmod_structure (adapt ToPlain.mk)]
        module OfPlain = [%m Ast.pmod_structure (adapt OfPlain.mk)]
        module Equal = [%m Ast.pmod_structure (adapt Equal.mk)]
        module MapInfo = [%m Ast.pmod_structure (adapt MapInfo.mk)]

        (*
      TODO:
      module PpGeneric = struct end
      module OfNominal = struct end
      module ToNominal = struct end
      module Jsonify = struct end
      module Unjsonify = struct end
      module Select = struct end
      module SubstAll = struct end
      *)]
    in
    let wrapper_module =
      Ast.module_binding
        ~name:{ txt = Some "Wrapper" (* (module_name sort_name) *); loc }
        ~expr:(Ast.pmod_structure defs)
      |> Ast.pstr_module
    in
    ( wrapper_module
    , Ast.psig_type Recursive info_decls
    , Ast.psig_type Recursive plain_decls )
  ;;
end

let all_term_s (module Ast : Ast_builder.S) =
  Ast.(pmty_ident { txt = unflatten [ "LanguageObject"; "AllTermS" ]; loc })
;;

let mk_individual_type_module
    (module Ast : Ast_builder.S)
    ~prim_names:_
    ~sort_def_map:_
    sort_name
    (Syn.SortDef.SortDef (vars, _op_defs))
  =
  let loc = Ast.loc in
  let _var_names = vars |> List.map ~f:fst |> SSet.of_list in
  let plain_type_decl =
    let manifest_arg_list =
      vars
      |> List.map ~f:fst
      |> List.map ~f:(fun name ->
             Ast.ptyp_constr
               { txt = unflatten [ module_name name; "Plain"; "t" ]; loc }
               [])
    in
    let kind =
      Ptype_abstract
      (*
      Ptype_variant
        (List.map
           op_defs
           ~f:
             (CtorDecl.mk
                ~info:false
                ~typedef_mode:IndividualTypeModule
                ~var_names
                ~mutual_sorts:sort_def_map
                ~prim_names))
                *)
    in
    let manifest =
      Some
        (Ast.ptyp_constr
           { txt = unflatten [ "Wrapper"; "Plain"; sort_name ]; loc }
           manifest_arg_list)
    in
    Ast.pstr_type
      Recursive
      [ Ast.type_declaration
          ~name:{ txt = "t"; loc = Ast.loc }
          ~params:[]
          ~cstrs:[]
          ~kind
          ~private_:Public
          ~manifest
      ]
  in
  let info_type_decl =
    let manifest_arg_list =
      vars
      |> List.map ~f:fst
      |> List.map ~f:(fun name ->
             Ast.ptyp_constr
               { txt = unflatten [ module_name name; "t" ]; loc }
               [ Ast.ptyp_var "info" ])
    in
    let kind =
      Ptype_abstract
      (*
      Ptype_variant
        (List.map
           op_defs
           ~f:
             (CtorDecl.mk
                ~info:true
                ~typedef_mode:IndividualTypeModule
                ~var_names
                ~mutual_sorts:sort_def_map
                ~prim_names))
                *)
    in
    let manifest =
      Some
        (Ast.ptyp_constr
           { txt = unflatten [ "Wrapper"; "Types"; sort_name ]; loc }
           (Ast.ptyp_var "info" :: manifest_arg_list))
    in
    Ast.pstr_type
      Recursive
      [ Ast.type_declaration
          ~name:{ txt = "t"; loc = Ast.loc }
          ~params:[ Ast.ptyp_var "info", (NoVariance, NoInjectivity) ]
          ~cstrs:[]
          ~kind
          ~private_:Public
          ~manifest
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
           let wrapper_fun =
             pexp_ident { txt = unflatten [ "Wrapper"; mod_name; sort_name ]; loc }
           in
           let expr =
             let args =
               match vars with
               | [] -> []
               | _ ->
                 vars
                 |> List.map ~f:(fun (name, _kind_opt) ->
                        let txt = unflatten [ module_name name; fun_name ] in
                        Nolabel, pexp_ident { txt; loc })
             in
             let labelled_args =
               match fun_name with
               | "equal" -> [ labelled_arg (module Ast) "info_eq" ]
               | "map_info" -> [ labelled_arg (module Ast) "f" ]
               | _ -> []
             in
             let tm = pexp_ident { txt = Lident "tm"; loc } in
             let args = args @ Util.List.snoc labelled_args (Nolabel, tm) in
             pexp_apply wrapper_fun args
           in
           let expr = pexp_fun Nolabel None (ppat_var { txt = "tm"; loc }) expr in
           let expr =
             match fun_name with
             | "equal" -> labelled_fun (module Ast) "info_eq" expr
             | "map_info" -> labelled_fun (module Ast) "f" expr
             | _ -> expr
           in
           let pat = ppat_var { txt = fun_name; loc } in
           pstr_value Nonrecursive [ value_binding ~pat ~expr ])
  in
  (* TODO: implement *)
  let fun_defs =
    List.append
      fun_defs
      [ [%stri
          let pp_generic ~open_loc:_ ~close_loc:_ ppf _tm = Fmt.pf ppf "TODO: pp_generic"]
      ; [%stri
          module Parse (Comment : ParseUtil.Comment_int) = struct
            let t = failwith "TODO"
          end]
      ]
  in
  let init =
    let open Ast in
    pmod_structure
      (info_type_decl
       :: [%stri module Plain = [%m pmod_structure [ plain_type_decl ]]] :: fun_defs)
  in
  let f (name, kind_opt) accum =
    match kind_opt with
    | None (* XXX should do kind inference instead of assuming it's * *)
    | Some (Syn.Kind.Kind (_, 1)) ->
      let mod_param =
        Named ({ txt = Some (module_name name); loc }, all_term_s (module Ast))
      in
      Ast.pmod_functor mod_param accum
    | Some kind ->
      Location.raise_errorf
        ~loc
        "Code generation currently only supports external modules of kind * (%s is %s)"
        name
        (Fmt.to_to_string Syn.Kind.pp kind)
  in
  let expr = List.fold_right vars ~init ~f in
  Ast.module_binding ~name:{ txt = Some (module_name sort_name); loc } ~expr
  |> Ast.pstr_module
;;

(* The top-level container / result (which is really a functor if there are externals). *)
module ContainerModule (Ast : Ast_builder.S) = struct
  module WrapperModule = WrapperModule (Ast)

  let loc = Ast.loc

  let mk Syn.{ externals; sort_defs } =
    let all_term_s = all_term_s (module Ast) in
    let prim_names = externals |> List.map ~f:fst |> SSet.of_list in
    (* pre-declare types *)
    let wrapper_module, info_types_sig, plain_types_sig =
      WrapperModule.mk ~prim_names sort_defs
    in
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let type_modules =
      List.map
        sort_defs
        ~f:
          (Util.Tuple2.uncurry
             (mk_individual_type_module (module Ast) ~prim_names ~sort_def_map))
    in
    (* TODO: include language?
  let sort_defs =
    [%str let language = [%e SyntaxQuoter.mk_language ~loc lang]] @ sort_defs
  in
  *)
    (* Turn into a functor over externals *)
    let mod_param (name, kind) =
      match kind with
      | Some (Syn.Kind.Kind (_, 1)) | None ->
        Named ({ txt = Some (module_name name); loc }, all_term_s)
      | Some kind ->
        Location.raise_errorf
          ~loc
          "Code generation currently only supports external modules of kind * (%s is %s)"
          name
          (Fmt.to_to_string Syn.Kind.pp kind)
    in
    let mod_param' (name, kind) = mod_param (name, Some kind) in
    let expr =
      let init =
        Ast.pmod_structure
          ([ wrapper_module
           ; [%stri module Types = Wrapper.Types]
           ; [%stri module Plain = Wrapper.Plain]
           ]
          @ type_modules)
      in
      List.fold_right externals ~init ~f:(mod_param' >> Ast.pmod_functor)
    in
    let open Ast in
    let info = ptyp_var "info" in
    let mod_ty =
      let plain_sig =
        psig_module
          (module_declaration
             ~name:{ txt = Some "Plain"; loc }
             ~type_:(pmty_signature [ plain_types_sig ]))
      in
      let types_sig =
        psig_module
          (module_declaration
             ~name:{ txt = Some "Types"; loc }
             ~type_:(pmty_signature [ info_types_sig ]))
      in
      let tdecl ~params ~manifest =
        type_declaration
          ~name:{ txt = "t"; loc }
          ~params
          ~cstrs:[]
          ~kind:Ptype_abstract
          ~private_:Public
          ~manifest
      in
      let mk_args vars =
        let make names args = ptyp_constr { txt = unflatten names; loc } args in
        let info_args, plain_args =
          vars
          |> List.map ~f:(fun (name, _) ->
                 ( make [ module_name name; "t" ] [ info ]
                 , make [ module_name name; "Plain"; "t" ] [] ))
          |> List.unzip
        in
        info :: info_args, plain_args
      in
      let sort_module_sigs =
        sort_defs
        |> List.map ~f:(fun (sort_name, Syn.SortDef.SortDef (vars, _op_defs)) ->
               let info_args, plain_args = mk_args vars in
               let type_decl =
                 let info_ty = { txt = unflatten [ "Types"; sort_name ]; loc } in
                 let params = [ info, (NoVariance, NoInjectivity) ] in
                 tdecl ~params ~manifest:(Some (ptyp_constr info_ty info_args))
               in
               let plain_type_decl =
                 let plain_ty = { txt = unflatten [ "Plain"; sort_name ]; loc } in
                 tdecl ~params:[] ~manifest:(Some (ptyp_constr plain_ty plain_args))
               in
               let init =
                 pmty_with
                   all_term_s
                   [ Pwith_type ({ txt = Lident "t"; loc }, type_decl)
                   ; Pwith_type
                       ({ txt = unflatten [ "Plain"; "t" ]; loc }, plain_type_decl)
                   ]
               in
               let name = { txt = Some (module_name sort_name); loc } in
               let type_ =
                 List.fold_right vars ~init ~f:(mod_param >> Ast.pmty_functor)
               in
               psig_module (module_declaration ~name ~type_))
      in
      let init = Ast.pmty_signature (types_sig :: plain_sig :: sort_module_sigs) in
      List.fold_right externals ~init ~f:(mod_param' >> Ast.pmty_functor)
    in
    Ast.pmod_constraint expr mod_ty
  ;;
end

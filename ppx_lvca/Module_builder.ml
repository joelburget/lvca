open Base
open Lvca_syntax
open Lvca_provenance
open Lvca_util
open Ppxlib
module SSet = String.Set
module SMap = String.Map
module Syn = Abstract_syntax
module Graph = Directed_graph.Make (Base.String)

module Supported_function = struct
  type t =
    | Fun_of_nominal
    | Fun_to_nominal
    | Fun_equivalent
    | Fun_info

  let fun_name = function
    | Fun_of_nominal -> "of_nominal"
    | Fun_to_nominal -> "to_nominal"
    | Fun_equivalent -> "equivalent"
    | Fun_info -> "info"
  ;;

  let mod_name = String.capitalize << fun_name
end

type type_decl_context =
  | Predefinition
  | Individual_type_module

module type Builder_context = sig
  (** The contents of the source file. *)
  val buf : string

  module Ast : Ast_builder.S
end

let get_range = function
  | `Located (Provenance.Located.Parse_located { range; _ }) -> range
  | _ -> None
;;

module Update_loc (Context : Builder_context) = struct
  open Context

  let loc = Ast.loc

  (* pos_bol: offset of the beginning of the line
     pos_cnum: offset of the position (number of characters between the
       beginning of the lexbuf and the position)

     > The difference between pos_cnum and pos_bol is the character offset
     within the line (i.e. the column number, assuming each character is one
     column wide).

     Update the contextual location to focus on just the given [Opt_range]. Example:

     {[
  012345678901234567890123456
0 module Lang =
1 [%lvca.abstract_syntax_module
2 {\|
3 integer : *
4
5 foo :=
6   | Foo(integer)
7   | Bar(foo[foo]. foo. foo)
  012345678901234567890123456
     ]}

     Here the module-level loc will be the location of the string, from 2,2 to
     2,100 (or something like that). So, we need to add locations in [optrange]
     to the contextual [loc].
   *)
  let go : Opt_range.t -> Location.t =
   fun optrange ->
    match optrange with
    | None -> loc
    | Some { start; finish } ->
      (* Buffers before and internal to the referenced span. *)
      let before_buf = String.subo ~len:start buf in
      let internal_buf = String.sub ~pos:start ~len:(finish - start) buf in
      (* Number of newlines before and internal to the referenced span. *)
      let buf_start_lnum = String.count ~f:Char.(fun c -> c = '\n') before_buf in
      let internal_lines = String.count ~f:Char.(fun c -> c = '\n') internal_buf in
      let start_bol =
        match String.rsplit2 before_buf ~on:'\n' with
        | Some (lstr, _rstr) -> String.(length lstr)
        | None -> 0
      in
      let finish_bol =
        match String.rsplit2 internal_buf ~on:'\n' with
        | Some (lstr, _rstr) -> String.(length before_buf + length lstr)
        | None -> start_bol
      in
      { loc with
        loc_start =
          { loc.loc_start with
            pos_lnum = loc.loc_start.pos_lnum + buf_start_lnum
          ; pos_bol = start_bol
          ; pos_cnum = start - 1
          }
      ; loc_end =
          { loc.loc_end with
            pos_lnum = loc.loc_start.pos_lnum + buf_start_lnum + internal_lines
          ; pos_bol = finish_bol
          ; pos_cnum = finish - 1
          }
      }
 ;;
end

let%test_module "Update_loc" =
  (module struct
    let mk_pos pos_lnum pos_bol pos_cnum =
      Lexing.{ pos_fname = "test"; pos_lnum; pos_bol; pos_cnum }
    ;;

    module Context1 = struct
      let buf = "quick\nbrown\nfox"
      (*         ^      ^        ^
                 012345 678901 234
                 1      2        3 *)

      module Ast = Ast_builder.Make (struct
        let loc =
          Location.
            { loc_start = mk_pos 1 0 0
            ; loc_end = mk_pos 2 12 (String.length buf - 1)
            ; loc_ghost = false
            }
        ;;
      end)
    end

    let print loc =
      Location.print Fmt.stdout loc;
      Fmt.pr "\n"
    ;;

    let%expect_test _ =
      let module Update_loc = Update_loc (Context1) in
      let go = Update_loc.go >> print in
      go None;
      go (Some { start = 0; finish = 14 });
      go (Some { start = 0; finish = 13 });
      go (Some { start = 6; finish = 14 });
      go (Some { start = 12; finish = 14 });
      [%expect
        {|
      File "test", line 1, characters 0-14:
      File "test", line 1, characters -1-13:
      File "test", line 1, characters -1-12:
      File "test", line 2, characters 0-8:
      File "test", line 3, characters 0-2: |}]
    ;;

    module Context2 = struct
      let buf = "quick brown fox"
      (*         ^     ^       ^
                 012345678901234
                 1     2       3 *)

      module Ast = Ast_builder.Make (struct
        let loc =
          Location.
            { loc_start = mk_pos 0 0 0
            ; loc_end = mk_pos 2 12 (String.length buf - 1)
            ; loc_ghost = false
            }
        ;;
      end)
    end

    let%expect_test _ =
      let module Update_loc = Update_loc (Context2) in
      let go = Update_loc.go >> print in
      go None;
      go (Some { start = 0; finish = 14 });
      go (Some { start = 0; finish = 13 });
      go (Some { start = 6; finish = 14 });
      go (Some { start = 12; finish = 14 });
      [%expect
        {|
      File "test", line 0, characters 0-14:
      File "test", line 0, characters -1-13:
      File "test", line 0, characters -1-12:
      File "test", line 0, characters 5-13:
      File "test", line 0, characters 11-13: |}]
    ;;
  end)
;;

(* Concatenate a list of names into a Longident. *)
(* TODO: is this just Longident.unflatten? *)
let unflatten names =
  match names with
  | [] -> Lvca_util.invariant_violation ~here:[%here] "unflatten: names must be nonempty"
  | nm0 :: nms -> List.fold nms ~init:(Lident nm0) ~f:(fun accum m -> Ldot (accum, m))
;;

(* TODO: more sophisticated naming rules? *)
let ctor_name = String.capitalize
let module_name = String.capitalize
let var_ctor_name sort_name = String.capitalize sort_name ^ "_var"
let guard = None

let conjuntion ~loc exps =
  let exps, last = Lvca_util.List.unsnoc exps in
  List.fold_right exps ~init:last ~f:(fun e1 e2 -> [%expr [%e e1] && [%e e2]])
;;

let sort_head = function Sort.Name (_, name) | Sort.Ap (_, name, _) -> name

let rec all_sort_names = function
  | Sort.Name (_, name) -> SSet.singleton name
  | Sort.Ap (_, name, args) ->
    Set.union
      (SSet.singleton name)
      (args |> Sort.Ap_list.to_list |> List.map ~f:all_sort_names |> SSet.union_list)
;;

(* Returns a mapping from the name of each sort defined in this language to
   the name of each sort it uses without passing through an opaque external.
   This is used to determine dependencies between sorts.

   Note: opaque here meaning mapped to Lvca_syntax.Nominal.Term. The
   alternative is if this type is mapped to another module via pragma.

   We don't count bound variable types.

   Example: {[
     list : * -> *

     maybe a := None() | Some(a)

     x := ...
     y := ...

     foo :=
      | Ctr1(list x)
      | Ctr2(maybe y)
  ]}

   Here foo directly uses maybe, and y, but not list (external) or x (passed
   through an external).

   Why (for (a)) do we want only directly-used sorts? Because [maybe x] is
   represented as [x maybe] but [list x] is represented as [Nominal.Term.t]. We
   only want the former for dependency-graph-building purposes.
   *)
let get_sort_ref_info ~prims ~sort_defs =
  let known_sorts_1 = sort_defs |> List.map ~f:fst |> SSet.of_list in
  let known_sorts_2 = prims |> Map.keys |> SSet.of_list in
  let known_sorts = Set.union known_sorts_1 known_sorts_2 in
  let sort_deps =
    List.map sort_defs ~f:(fun (sort_name, Syn.Sort_def.Sort_def (vars, op_defs)) ->
        let vars = vars |> List.map ~f:fst |> SSet.of_list in
        let sort_deps =
          op_defs
          |> List.map
               ~f:(fun (Syn.Operator_def.Operator_def (_info, _name, Arity (_, arity))) ->
                 arity
                 |> List.map ~f:(fun (Syn.Valence.Valence (_sort_slots, body_sort)) ->
                        let sort_name = sort_head body_sort in
                        if Set.mem known_sorts sort_name
                        then all_sort_names body_sort
                        else SSet.singleton sort_name)
                 |> SSet.union_list)
          |> SSet.union_list
        in
        (* Only add if it's a known sort name (not an external) and not
            shadowed by a var. *)
        let sort_deps = Set.diff (Set.inter sort_deps known_sorts_1) vars in
        sort_name, Set.to_list sort_deps)
  in
  SMap.of_alist_exn sort_deps
;;

(** Is this sort ever bound in the language? *)
type bound_unbound =
  | Bound
  | Unbound

(** Determine whether each sort is ever bound in the language. *)
let make_binding_status sort_defs =
  let result =
    sort_defs
    |> List.map ~f:(fun (name, _def) -> name, Unbound)
    |> Hashtbl.of_alist_exn (module String)
  in
  List.iter sort_defs ~f:(fun (_sort_name, Syn.Sort_def.Sort_def (_vars, op_defs)) ->
      List.iter
        op_defs
        ~f:(fun (Syn.Operator_def.Operator_def (_info, _name, Arity (_, arity))) ->
          List.iter arity ~f:(fun (Syn.Valence.Valence (sort_slots, _body_sort)) ->
              List.iter sort_slots ~f:(fun slot ->
                  let sort =
                    match slot with
                    | Sort_binding sort -> sort
                    | Sort_pattern { var_sort; _ } -> var_sort
                  in
                  Hashtbl.set result ~key:(sort_head sort) ~data:Bound))));
  result
;;

module Helpers (Context : Builder_context) = struct
  open Context
  open Ast
  module Update_loc = Update_loc (Context)

  let update_loc rng = Update_loc.go rng

  (** Context for classifying a sort ([classify_sort]) as [defn_status]. *)
  type context =
    { var_names : SSet.t
    ; mutual_sorts : Syn.Sort_def.t SMap.t
    ; prims : string list SMap.t
    }

  (** When using a sort, we treat it differently depending on if it's a variable, defined
      mutually with the current sort (as defined by [mutual_sorts]), or predefined (either
      as an imported primitive or a sort defined earlier in this language). *)
  type defn_status =
    | Variable
    | Mutual_sort
    | External_sort of string list

  let classify_sort context sort =
    let { var_names; mutual_sorts; prims } = context in
    let sort_name, sort_args = Sort.split sort in
    if Set.mem var_names sort_name
    then Variable
    else (
      match Map.find mutual_sorts sort_name with
      | Some (Syn.Sort_def.Sort_def (vars, _op_defs)) ->
        (if List.(Int.(length vars <> length sort_args))
        then
          Location.Error.(
            raise
              (make
                 ~loc
                 ~sub:[]
                 Fmt.(
                   str
                     "mismatched vars ([%a]) and args ([%a]) to sort %s"
                     (list ~sep:comma string)
                     (List.map vars ~f:fst)
                     (list ~sep:comma Sort.pp)
                     sort_args
                     sort_name))));
        Mutual_sort
      | None ->
        (match Map.find prims sort_name with
        | Some mods -> External_sort mods
        | None ->
          Location.raise_errorf
            ~loc
            "Unknown sort: %s (known: {%s})"
            sort_name
            Fmt.(str "%a" (list string) (Map.keys prims))))
  ;;

  let nominal_term = [ "Lvca_syntax"; "Nominal"; "Term" ]
  let nominal_operator = [ "Lvca_syntax"; "Nominal"; "Term"; "Operator" ]
  let nominal_operator' = unflatten nominal_operator

  (** Make the type corresponding to a sort in a constructor declaration. Eg:

      [ nonempty := Nonempty (string; list string) ]

      ==>

      If [list] is defined in this module:

      {[
        nonempty = Nonempty of Provenance.t * Nominal.t * list
                                             ^^^^^^^^^^   ^^^^
      ]}

      If [list] is an external:

      {[
        nonempty =
           | Nonempty of Provenance.t * Nominal.t * Nominal.t
                                        ^^^^^^^^^   ^^^^^^^^^
      ]} *)
  let ptyp_of_sort ~type_decl_context ~context sort =
    let rec go sort =
      let loc = sort |> Sort.info |> get_range |> update_loc in
      let name, sort_args = Sort.split sort in
      match classify_sort context sort with
      | Variable -> ptyp_var name
      | Mutual_sort ->
        let sort_args = List.map sort_args ~f:go in
        let names =
          match type_decl_context with
          | Predefinition -> [ name ]
          | Individual_type_module -> [ "Wrapper"; "Types"; name ]
        in
        ptyp_constr { txt = unflatten names; loc } sort_args
      | External_sort mods ->
        let module_path, args = mods, List.map sort_args ~f:go in
        ptyp_constr { txt = unflatten (module_path @ [ "t" ]); loc } args
    in
    go sort
  ;;

  (** Make a function application for a sort expression, eg in [of_nominal]:

      {[
        Types.Cons (x0, x1, x2) -> Types.Cons ((f x0), (a ~f x1), (list a ~f x2))
                                                       ^^^^^^^^^  ^^^^^^^^^^^^^^
      ]}

      This is used in the RHS of [of_nominal] / [to_nominal] definitions. *)
  let mk_sort_app ~fun_defn ~classify_sort ~args sort =
    let rec go sort =
      let loc = sort |> Sort.info |> get_range |> update_loc in
      let apply sort_fun =
        match sort with
        | Sort.Ap (_, _, args) ->
          pexp_apply
            sort_fun
            (args |> Sort.Ap_list.to_list |> List.map ~f:(fun sort -> Nolabel, go sort))
        | Name _ -> sort_fun
      in
      let names =
        match classify_sort sort with
        | External_sort mods -> mods @ [ Supported_function.fun_name fun_defn ]
        | Variable | Mutual_sort -> [ sort_head sort ]
      in
      apply (pexp_ident { txt = unflatten names; loc })
    in
    pexp_apply (go sort) args
  ;;

  (* Make a function taking an f_* argument *)
  let f_fun ?(used = true) var_name =
    pexp_fun Nolabel None (pvar ((if used then "" else "_") ^ var_name))
  ;;

  let nominal_convertible_s =
    pmty_ident
      { txt = unflatten [ "Lvca_syntax"; "Nominal"; "Convertible"; "Extended_s" ]; loc }
  ;;

  let mk_exp_tuple = function
    | [] -> [%expr ()]
    | [ elem ] -> elem
    | elems -> pexp_tuple elems
  ;;

  let mk_exp_tuple' contents =
    match contents with [] -> None | _ -> Some (mk_exp_tuple contents)
  ;;

  let mk_pat_tuple = function
    | [] -> [%pat? ()]
    | [ elem ] -> elem
    | elems -> ppat_tuple elems
  ;;

  let mk_pat_tuple' contents =
    match contents with [] -> None | _ -> Some (mk_pat_tuple contents)
  ;;

  let var_allocator name_base =
    let var_ix = ref 0 in
    fun () ->
      Int.incr var_ix;
      let name = Printf.sprintf "%s%d" name_base !var_ix in
      pvar name, evar name
  ;;

  let evar_allocator name_base =
    let alloc = var_allocator name_base in
    alloc >> snd
  ;;

  let pvar_allocator name_base =
    let alloc = var_allocator name_base in
    alloc >> fst
  ;;

  let plain_typ_var name = ptyp_var name, (NoVariance, NoInjectivity)
end

(** Helper for declaring a constructor. *)
module Ctor_decl (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  let args_of_valence
      ~type_decl_context
      context
      (Syn.Valence.Valence (binding_sort_slots, body_sort))
    =
    let pattern_type = [%type: Pattern.t] in
    let args =
      List.map binding_sort_slots ~f:(function
          | Syn.Sort_slot.Sort_binding sort ->
            let loc = sort |> Sort.info |> get_range |> update_loc in
            [%type: Lvca_syntax.Single_var.t]
          | Sort_pattern _sort -> pattern_type)
    in
    args @ [ ptyp_of_sort ~type_decl_context ~context body_sort ]
  ;;

  let args_of_arity
      ~type_decl_context
      ~var_names
      ~mutual_sorts
      ~prims
      (Syn.Arity.Arity (_, arity))
    =
    let args_of_valence =
      args_of_valence ~type_decl_context { var_names; mutual_sorts; prims }
    in
    arity
    |> List.map ~f:args_of_valence
    |> List.cons [ [%type: Lvca_syntax.Provenance.t] ]
    |> List.map ~f:(function [] -> [%type: unit] | [ ty ] -> ty | tys -> ptyp_tuple tys)
  ;;

  let mk
      ~type_decl_context
      ~var_names
      ~mutual_sorts
      ~prims
      (Syn.Operator_def.Operator_def (_info, op_name, arity))
    =
    let args = args_of_arity ~type_decl_context ~var_names ~mutual_sorts ~prims arity in
    constructor_declaration
      ~name:{ txt = ctor_name op_name; loc }
      ~args:(Pcstr_tuple args)
      ~res:None
  ;;
end

module Type_decls (Context : Builder_context) = struct
  open Context
  open Ast
  module Ctor_decl = Ctor_decl (Context)
  open Helpers (Context)

  let mk_op_ctors
      ~type_decl_context
      ~sort_def_map:mutual_sorts
      ~prims
      ~sort_name
      ~sort_binding_status
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let op_ctors =
      List.map
        op_defs
        ~f:(Ctor_decl.mk ~type_decl_context ~var_names ~mutual_sorts ~prims)
    in
    let var_ctor =
      let args = [ [%type: Lvca_syntax.Provenance.t]; [%type: string] ] in
      constructor_declaration
        ~name:{ txt = var_ctor_name sort_name; loc }
        ~args:(Pcstr_tuple args)
        ~res:None
    in
    (* If this type is ever bound, add a variable constructor. *)
    match Hashtbl.find sort_binding_status sort_name with
    | Some Bound -> op_ctors @ [ var_ctor ]
    | _ -> op_ctors
  ;;

  let mk ~sort_def_map ~sorted_scc_graph ~prims ~sort_binding_status =
    List.map sorted_scc_graph ~f:(fun (_scc_num, sort_name_set) ->
        sort_name_set
        |> Set.to_list
        |> List.map ~f:(fun sort_name ->
               let sort_def = Map.find_exn sort_def_map sort_name in
               let (Syn.Sort_def.Sort_def (vars, _op_defs)) = sort_def in
               let params = vars |> List.map ~f:fst |> List.map ~f:plain_typ_var in
               let op_ctors =
                 mk_op_ctors
                   ~sort_def_map
                   ~type_decl_context:Predefinition
                   ~prims
                   ~sort_name
                   ~sort_binding_status
                   sort_def
               in
               type_declaration
                 ~name:{ txt = sort_name; loc }
                 ~params
                 ~cstrs:[]
                 ~kind:(Ptype_variant op_ctors)
                 ~private_:Public
                 ~manifest:None))
    |> List.join
  ;;
end

module Operator_pat (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  let is_valid_ocaml_constr_name str =
    (not (String.is_empty str)) && Char.is_uppercase str.[0]
  ;;

  let mk
      ?(match_info = false)
      ?(match_non_info = true)
      ?(name_base = "x")
      (Syn.Operator_def.Operator_def (_info, op_name, Arity (_, arity)))
    =
    if not (is_valid_ocaml_constr_name op_name)
    then Location.raise_errorf ~loc "Invalid OCaml operator name: %s" op_name;
    let var_ix = ref 0 in
    let v ix = pvar (Printf.sprintf "%s%d" name_base ix) in
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
                    else ppat_any))
    in
    let contents = [ (if match_info then v 0 else ppat_any) ] :: contents in
    let body =
      match contents with
      | [] -> None
      | _ -> Some (contents |> List.map ~f:mk_pat_tuple |> mk_pat_tuple)
    in
    let txt = unflatten [ "Types"; op_name ] in
    ppat_construct { txt; loc } body
  ;;
end

module To_nominal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  (* Eg:

    {[
      (* Lam(tm. tm) *)
      | Lam (info, (x0, x1)) ->
        Nominal.Term.Operator (info, "Lam", [
          Nominal.Scope.Scope ([Pattern.Var (info, x0)], tm x1)
        ])
      (* Match_line(tm[tm]. tm) *)
      | Match_line (info, (x0, x1)) ->
        Nominal.Term.Operator (info, "Match_line", [
          Nominal.Scope.Scope ([x0], tm x1)
        ])
      (* Pair(tm; tm) *)
      | Pair (info, x0, x1) ->
        Nominal.Term.Operator (info, "Pair", [
          Nominal.Scope.Scope ([], tm x0);
          Nominal.Scope.Scope ([], tm x1)
        ])
    ]}

    Note: we currently never map to [Primitive].
  *)
  let mk_nominal_exp
      ~var_names
      ~prims
      sort_defs
      (Syn.Operator_def.Operator_def (_info, op_name, Arity (_, arity)))
    =
    (* TODO: change to name "info" *)
    let info = evar "x0" in
    let v = evar_allocator "x" in
    let classify_sort = classify_sort { var_names; mutual_sorts = sort_defs; prims } in
    let body_arg sort =
      mk_sort_app ~fun_defn:Fun_to_nominal ~classify_sort ~args:[ Nolabel, v () ] sort
    in
    let mk_scope (Syn.Valence.Valence (slots, body_sort)) =
      let args =
        List.map slots ~f:(function
            | Syn.Sort_slot.Sort_binding _ ->
              let v = v () in
              [%expr Lvca_syntax.Pattern.Var ([%e v].info, [%e v].name)]
            | Sort_pattern _ -> v ())
        |> Syntax_quoter.Exp.list ~loc
      in
      [%expr Lvca_syntax.Nominal.Scope.Scope ([%e args], [%e body_arg body_sort])]
    in
    let children = arity |> List.map ~f:mk_scope |> Syntax_quoter.Exp.list ~loc in
    let body = mk_exp_tuple' [ info; estring op_name; children ] in
    pexp_construct { txt = nominal_operator'; loc } body
  ;;

  let mk
      ~prims
      ~sort_binding_status
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~match_info:true op_def in
      let rhs = mk_nominal_exp ~var_names ~prims sort_defs op_def in
      case ~lhs ~guard ~rhs
    in
    let var_case =
      case
        ~lhs:
          (ppat_construct
             { txt = Lident (var_ctor_name sort_name); loc }
             (Some [%pat? info, name]))
        ~guard
        ~rhs:[%expr Lvca_syntax.Nominal.Term.Var (info, name)]
    in
    let op_defs = List.map op_defs ~f in
    let op_defs =
      match Hashtbl.find sort_binding_status sort_name with
      | Some Bound -> op_defs @ [ var_case ]
      | _ -> op_defs
    in
    let expr =
      List.fold_right (List.map ~f:fst vars) ~init:(pexp_function op_defs) ~f:f_fun
    in
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

module Of_nominal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  (* Eg:

    {[
      (* Lam(tm. tm) *)
        Nominal.Term.Operator (info, "Lam", [
          Nominal.Scope.Scope ([Pattern.Var (info, x0)], x1)
        ])
        ->
          let%bind x1 = tm x1 in
          Ok (Lam (info, (x0, x1)))
      (* Match_line(tm[tm]. tm) *)
      | Nominal.Term.Operator (info, "Match_line", [
          Nominal.Scope.Scope ([x0], x1)
        ])
        ->
          let%bind x1 = tm x1 in
          Ok (Match_line (info, (x0, x1)))
      (* Pair(tm; tm) *)
      | Nominal.Term.Operator (info, "Pair", [
          Nominal.Scope.Scope ([], x0);
          Nominal.Scope.Scope ([], x1)
        ])
        ->
          let%bind x0 = tm x0 in
          let%bind x1 = tm x1 in
          Ok (Pair (info, x0, x1))
      | tm -> Error tm
    ]}

    Note: we currently never map to [Primitive].
  *)
  let mk_nominal_pat (Syn.Operator_def.Operator_def (_info, op_name, Arity (_, arity))) =
    (* TODO: change to name "info" *)
    let info = pvar "x0" in
    let v = pvar_allocator "x" in
    let mk_scope (Syn.Valence.Valence (slots, _body_sort)) =
      let args =
        List.map slots ~f:(function
            | Syn.Sort_slot.Sort_binding _ ->
              let info_v = v () in
              let name_v = v () in
              [%pat? Lvca_syntax.Pattern.Var ([%p info_v], [%p name_v])]
            | Sort_pattern _ -> v ())
        |> Syntax_quoter.Pat.list ~loc
      in
      [%pat? Lvca_syntax.Nominal.Scope.Scope ([%p args], [%p v ()])]
    in
    let children = List.map arity ~f:mk_scope |> Syntax_quoter.Pat.list ~loc in
    let body = mk_pat_tuple' [ info; pstring op_name; children ] in
    ppat_construct { txt = nominal_operator'; loc } body
  ;;

  let mk_exp
      ~var_names
      ~prims
      sort_defs
      (Syn.Operator_def.Operator_def (_info, op_name, Arity (_, arity)))
    =
    let v = var_allocator "x" in
    let ev = v >> snd in
    let info = evar "x0" in
    (* Queue of (variable, conversion to perform) *)
    let conversions_needed = Queue.create () in
    let classify_sort = classify_sort { var_names; mutual_sorts = sort_defs; prims } in
    let body_arg ev sort =
      mk_sort_app ~fun_defn:Fun_of_nominal ~classify_sort ~args:[ Nolabel, ev ] sort
    in
    let contents =
      arity
      |> List.map ~f:(fun (Syn.Valence.Valence (slots, body_sort)) ->
             let slots_args =
               slots
               |> List.map ~f:(fun slot ->
                      match slot with
                      | Syn.Sort_slot.Sort_binding _sort ->
                        let info_v = ev () in
                        let name_v = ev () in
                        [%expr
                          Lvca_syntax.Single_var.
                            { info = [%e info_v]; name = [%e name_v] }]
                      | Sort_pattern _ -> ev ()
                      (* TODO: pattern_converter? *))
             in
             let pv, ev = v () in
             Queue.enqueue conversions_needed (pv, body_arg ev body_sort);
             slots_args @ [ ev ])
      |> List.map ~f:mk_exp_tuple
    in
    let contents = info :: contents in
    let txt = unflatten [ "Types"; op_name ] in
    let type_constr = pexp_construct { txt; loc } (mk_exp_tuple' contents) in
    conversions_needed, [%expr Ok [%e type_constr]]
  ;;

  let mk
      ~prims
      ~sort_binding_status
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = mk_nominal_pat op_def in
      let rhs =
        let conversions_needed, init = mk_exp ~var_names ~prims sort_defs op_def in
        let f (v, conversion) accum =
          [%expr
            match [%e conversion] with Error msg -> Error msg | Ok [%p v] -> [%e accum]]
        in
        conversions_needed |> Queue.to_list |> List.fold_right ~f ~init
      in
      case ~lhs ~guard ~rhs
    in
    let matching_cases = List.map op_defs ~f in
    let var_case =
      case
        ~lhs:[%pat? Lvca_syntax.Nominal.Term.Var (info, name)]
        ~guard
        ~rhs:
          [%expr
            Ok
              [%e
                pexp_construct
                  { txt = Lident (var_ctor_name sort_name); loc }
                  (Some [%expr info, name])]]
    in
    let fallthrough = case ~lhs:(pvar "tm") ~guard ~rhs:[%expr Error tm] in
    let matching_cases =
      match Hashtbl.find sort_binding_status sort_name with
      | Some Bound -> matching_cases @ [ var_case ]
      | _ -> matching_cases
    in
    let init = matching_cases @ [ fallthrough ] |> pexp_function in
    let expr = List.fold_right (List.map ~f:fst vars) ~init ~f:f_fun in
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

module Equivalent (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  let mk_rhs ~classify_sort ~args sort =
    let rec go sort =
      let loc = sort |> Sort.info |> get_range |> update_loc in
      let apply sort_fun =
        match sort with
        | Sort.Ap (_, _, args) ->
          pexp_apply
            sort_fun
            (args |> Sort.Ap_list.to_list |> List.map ~f:(fun sort -> Nolabel, go sort))
        | Name _ -> sort_fun
      in
      let names =
        match classify_sort sort with
        | External_sort mods -> mods @ [ "equivalent" ]
        | Variable | Mutual_sort -> [ sort_head sort ]
      in
      apply (pexp_ident { txt = unflatten names; loc })
    in
    pexp_apply (go sort) args
  ;;

  let mk
      ~prims
      ~sort_binding_status
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let classify_sort = classify_sort { var_names; mutual_sorts = sort_defs; prims } in
    let f (Syn.Operator_def.Operator_def (_info, _op_name, Arity (_, arity)) as op_def) =
      let lhs =
        let p1, p2 =
          ("x", "y")
          |> Tuple2.map ~f:(fun name_base ->
                 Operator_pat.mk ~match_info:true ~name_base op_def)
        in
        [%pat? [%p p1], [%p p2]]
      in
      let rhs =
        let arity_exps =
          let var_ix = ref 0 in
          let mk_xy () =
            ("x", "y")
            |> Tuple2.map ~f:(fun base -> evar (Printf.sprintf "%s%d" base !var_ix))
          in
          arity
          |> List.map ~f:(fun (Syn.Valence.Valence (slots, body_sort)) ->
                 let slots_checks =
                   slots
                   |> List.map ~f:(fun slot ->
                          Int.incr var_ix;
                          let x, y = mk_xy () in
                          match slot with
                          | Syn.Sort_slot.Sort_binding _sort ->
                            [%expr
                              Lvca_syntax.Single_var.equivalent ~info_eq [%e x] [%e y]]
                          | Sort_pattern _ ->
                            [%expr Lvca_syntax.Pattern.equivalent ~info_eq [%e x] [%e y]])
                 in
                 let body_check =
                   Int.incr var_ix;
                   let x, y = mk_xy () in
                   let args =
                     [ Labelled "info_eq", pexp_ident { txt = Lident "info_eq"; loc }
                     ; Nolabel, x
                     ; Nolabel, y
                     ]
                   in
                   mk_rhs ~classify_sort ~args body_sort
                 in
                 slots_checks @ [ body_check ])
          |> List.join
        in
        conjuntion ~loc ([%expr info_eq x0 y0] :: arity_exps)
      in
      case ~lhs ~guard ~rhs
    in
    let var_case =
      match Hashtbl.find sort_binding_status sort_name with
      | Some Bound ->
        let pat i_name v_name =
          ppat_construct
            { txt = unflatten [ "Types"; var_ctor_name sort_name ]; loc }
            (Some (ppat_tuple [ pvar i_name; pvar v_name ]))
        in
        [ case
            ~lhs:[%pat? [%p pat "i1" "n1"], [%p pat "i2" "n2"]]
            ~guard
            ~rhs:[%expr info_eq i1 i2 && Base.String.(n1 = n2)]
        ]
      | _ -> []
    in
    let branches = List.map op_defs ~f in
    let branches = branches @ var_case in
    let branches =
      match branches with
      | [] | [ _ ] -> branches
      | _ ->
        (* If there's more than one operator we need to add an extra case *)
        let last_branch = case ~lhs:[%pat? _, _] ~guard ~rhs:[%expr false] in
        branches @ [ last_branch ]
    in
    let init =
      [%expr
        fun ?(info_eq = fun _ _ -> true) t1 t2 -> [%e pexp_match [%expr t1, t2] branches]]
    in
    let info_eq_ty =
      [%type:
        ?info_eq:(Lvca_syntax.Provenance.t -> Lvca_syntax.Provenance.t -> bool)
        -> _
        -> _
        -> bool]
    in
    let expr =
      List.fold_right (List.map ~f:fst vars) ~init ~f:(fun var_name ->
          pexp_fun Nolabel None (ppat_constraint (pvar var_name) info_eq_ty))
    in
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

module Info (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  let mk
      ~prims:_
      ~sort_binding_status
      _sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let mk_case op_def =
      let lhs = Operator_pat.mk ~match_info:true ~match_non_info:false op_def in
      case ~lhs ~guard ~rhs:[%expr x0]
    in
    let expr =
      let op_defs = List.map op_defs ~f:mk_case in
      let var_case =
        case
          ~lhs:
            (ppat_construct
               { txt = unflatten [ "Types"; var_ctor_name sort_name ]; loc }
               (Some [%pat? info, _]))
          ~guard
          ~rhs:[%expr info]
      in
      let op_defs =
        match Hashtbl.find sort_binding_status sort_name with
        | Some Bound -> op_defs @ [ var_case ]
        | _ -> op_defs
      in
      List.fold_right
        (List.map ~f:fst vars)
        ~init:(pexp_function op_defs)
        ~f:(f_fun ~used:false)
    in
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

(* The wrapper module holds helper modules:
  - First, type declarations
  - Then individual modules for different helper functions: [info], etc.
  *)
module Wrapper_module (Context : Builder_context) = struct
  open Context
  open Ast
  module Type_decls = Type_decls (Context)
  module Info = Info (Context)
  module Equivalent = Equivalent (Context)
  module To_nominal = To_nominal (Context)
  module Of_nominal = Of_nominal (Context)

  let mk ~prims ~sort_def_map ~sort_dep_map ~sorted_scc_graph ~sort_binding_status =
    let ordered_sccs =
      sorted_scc_graph
      |> List.map ~f:(fun (scc_num, sort_name_set) ->
             let named_sort =
               sort_name_set
               |> Set.to_list
               |> List.map ~f:(fun name -> name, Map.find_exn sort_def_map name)
             in
             scc_num, named_sort)
      |> List.rev
    in
    let adapt
        ?definite_rec
        (maker :
          prims:string list SMap.t
          -> sort_binding_status:(string, bound_unbound) Hashtbl.t
          -> Syn.Sort_def.t SMap.t
          -> string
          -> Syn.Sort_def.t
          -> Ppxlib.value_binding)
      =
      ordered_sccs
      |> List.map ~f:(fun (_scc_num, named_sort) ->
             let value_bindings =
               List.map named_sort ~f:(fun (sort_name, sort_def) ->
                   maker ~prims ~sort_binding_status sort_def_map sort_name sort_def)
             in
             let is_rec =
               match definite_rec with
               | Some is_rec -> is_rec
               | None ->
                 (* Recursive if scc is more than one component or if self-referential. *)
                 (match named_sort with
                 | [ (sort_name, _) ] ->
                   (match Map.find sort_dep_map sort_name with
                   | Some sort_names
                     when List.mem sort_names sort_name ~equal:String.( = ) ->
                     Recursive
                   | _ -> Nonrecursive)
                 | _ -> Recursive)
             in
             pstr_value is_rec value_bindings)
      |> pmod_structure
    in
    let type_decls =
      Type_decls.mk ~sort_def_map ~sorted_scc_graph ~prims ~sort_binding_status
    in
    let defs =
      (* Each of these functions is potentially recursive (across multiple
         types), pre-declare. *)
      [%str
        module Types = struct
          [%%i pstr_type Recursive type_decls]
        end

        module Info = [%m adapt ~definite_rec:Nonrecursive Info.mk]
        module Equivalent = [%m adapt Equivalent.mk]
        module To_nominal = [%m adapt To_nominal.mk]
        module Of_nominal = [%m adapt Of_nominal.mk]]
    in
    module_binding ~name:{ txt = Some "Wrapper"; loc } ~expr:(pmod_structure defs)
    |> pstr_module
  ;;

  let mk_sigs ~prims ~sort_def_map ~sorted_scc_graph ~sort_binding_status =
    let type_decls =
      Type_decls.mk ~sort_def_map ~sorted_scc_graph ~prims ~sort_binding_status
    in
    [%sig:
      module Wrapper : sig
        module Types : sig
          [%%i psig_type Recursive type_decls]
        end
      end]
  ;;
end

module Individual_type_sig (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Type_decls = Type_decls (Context)

  let mk ~prims ~sort_def_map ~sort_binding_status sort_name sort_def =
    let (Syn.Sort_def.Sort_def (vars, _op_defs)) = sort_def in
    let var_names = List.map vars ~f:fst in
    let params = List.map var_names ~f:plain_typ_var in
    let op_ctors =
      Type_decls.mk_op_ctors
        ~type_decl_context:Individual_type_module
        ~sort_def_map
        ~prims
        ~sort_name
        ~sort_binding_status
        sort_def
    in
    let manifest =
      let type_vars = List.map var_names ~f:ptyp_var in
      let module_names = [ "Wrapper"; "Types"; sort_name ] in
      Some (ptyp_constr { txt = unflatten module_names; loc } type_vars)
    in
    psig_type
      Recursive
      [ type_declaration
          ~name:{ txt = "t"; loc }
          ~params
          ~cstrs:[]
          ~kind:(Ptype_variant op_ctors)
          ~private_:Public
          ~manifest
      ]
  ;;
end

module Individual_type_module (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Type_decls = Type_decls (Context)

  let mk ~prims ~sort_def_map ~sort_binding_status sort_name sort_def =
    let (Syn.Sort_def.Sort_def (vars, op_defs)) = sort_def in
    let var_names = List.map vars ~f:fst in
    let type_vars = List.map var_names ~f:ptyp_var in
    let params = List.map var_names ~f:plain_typ_var in
    let info_type_decl =
      let manifest =
        Some
          (ptyp_constr
             { txt = unflatten [ "Wrapper"; "Types"; sort_name ]; loc }
             type_vars)
      in
      let op_ctors =
        Type_decls.mk_op_ctors
          ~type_decl_context:Individual_type_module
          ~sort_def_map
          ~prims
          ~sort_name
          ~sort_binding_status
          sort_def
      in
      pstr_type
        Recursive
        [ type_declaration
            ~name:{ txt = "t"; loc }
            ~params
            ~cstrs:[]
            ~kind:(Ptype_variant op_ctors)
            ~private_:Public
            ~manifest
        ]
    in
    let fun_defs =
      [ Supported_function.Fun_info; Fun_equivalent; Fun_to_nominal; Fun_of_nominal ]
      |> List.map ~f:(fun fun_defn ->
             let fun_name = Supported_function.fun_name fun_defn in
             let wrapper_fun =
               pexp_ident
                 { txt =
                     unflatten
                       [ "Wrapper"; Supported_function.mod_name fun_defn; sort_name ]
                 ; loc
                 }
             in
             pstr_value
               Nonrecursive
               [ value_binding ~pat:(pvar fun_name) ~expr:wrapper_fun ])
    in
    let ctor_funs =
      List.map op_defs ~f:(fun (Operator_def (_, name, Arity (_, valences))) ->
          let var_names = List.mapi valences ~f:(fun i _ -> Fmt.str "x_%i" i) in
          let contents = evar "info" :: List.map var_names ~f:evar in
          let init = pexp_construct { txt = Lident name; loc } (mk_exp_tuple' contents) in
          let rhs =
            var_names
            |> List.map ~f:pvar
            |> List.fold_right ~init ~f:(pexp_fun Nolabel None)
          in
          let expr = pexp_fun (Labelled "info") None (pvar "info") rhs in
          pstr_value Nonrecursive [ value_binding ~pat:(pvar ("mk_" ^ name)) ~expr ])
    in
    let var_ctor_fun =
      let var_ctor_name = var_ctor_name sort_name in
      let rhs =
        pexp_construct
          { txt = Lident var_ctor_name; loc }
          (mk_exp_tuple' [ evar "info"; evar "name" ])
      in
      let expr = [%expr fun ~info name -> [%e rhs]] in
      pstr_value Nonrecursive [ value_binding ~pat:(pvar ("mk_" ^ var_ctor_name)) ~expr ]
    in
    let ctor_funs =
      match Hashtbl.find sort_binding_status sort_name with
      | Some Bound -> ctor_funs @ [ var_ctor_fun ]
      | _ -> ctor_funs
    in
    (*
    let equal_def =
      [%stri let ( = ) = equivalent ~info_eq:Lvca_syntax.Provenance.( = )]
    in
       *)
    let expr = pmod_structure ((info_type_decl :: fun_defs) @ ctor_funs) in
    module_binding ~name:{ txt = Some (module_name sort_name); loc } ~expr |> pstr_module
  ;;
end

(* The top-level container / result. *)
module Container_module (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Wrapper_module = Wrapper_module (Context)
  module Individual_type_module = Individual_type_module (Context)

  let mk ?(module_mapping = SMap.empty) (Syn.{ externals = _; sort_defs } as lang) =
    let prims = module_mapping in
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let sort_dep_map = get_sort_ref_info ~prims ~sort_defs in
    let Graph.Connected_components.{ scc_graph; sccs } =
      Graph.connected_components sort_dep_map
    in
    let sorted_scc_graph =
      scc_graph
      |> Directed_graph.Int.topsort_exn
      |> List.map ~f:(fun i -> i, Map.find_exn sccs i)
    in
    let sort_binding_status = make_binding_status sort_defs in
    let wrapper_module =
      Wrapper_module.mk
        ~prims
        ~sort_def_map
        ~sort_dep_map
        ~sorted_scc_graph
        ~sort_binding_status
    in
    let type_modules =
      List.map sort_defs ~f:(fun (sort_name, sort_def) ->
          Individual_type_module.mk
            ~prims
            ~sort_def_map
            ~sort_binding_status
            sort_name
            sort_def)
    in
    pmod_structure
      ([ wrapper_module
       ; [%stri module Types = Wrapper.Types]
       ; [%stri let language = [%e Syntax_quoter.Exp.language ~loc lang]]
       ]
      @ type_modules)
  ;;
end

module Sig (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Ctor_decl = Ctor_decl (Context)
  module Individual_type_sig = Individual_type_sig (Context)
  module Wrapper_module = Wrapper_module (Context)

  let mk ?(module_mapping = SMap.empty) Syn.{ externals = _; sort_defs } =
    let prims = module_mapping in
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let sort_dep_map = get_sort_ref_info ~prims ~sort_defs in
    let Graph.Connected_components.{ scc_graph; sccs } =
      Graph.connected_components sort_dep_map
    in
    let sorted_scc_graph =
      scc_graph
      |> Directed_graph.Int.topsort_exn
      |> List.map ~f:(fun i -> i, Map.find_exn sccs i)
    in
    let sort_binding_status = make_binding_status sort_defs in
    let type_sigs =
      List.map
        sort_defs
        ~f:(fun (sort_name, (Syn.Sort_def.Sort_def (vars, op_defs) as sort_def)) ->
          let var_names = List.map vars ~f:fst in
          let taken = SSet.of_list var_names in
          let taken, unique_var_names =
            List.fold_map vars ~init:taken ~f:(fun taken (name, _) ->
                Unique.generate_name ~base:(name ^ "_") taken)
          in
          let unique_vars = List.map unique_var_names ~f:ptyp_var in
          let t = ptyp_constr { txt = Lident "t"; loc } in
          let fold_ty ~init ~f =
            List.fold_right unique_vars ~init ~f:(fun v1v2 ty ->
                [%type: [%t f v1v2] -> [%t ty]])
          in
          let declare txt ~init ~f =
            psig_value
              (value_description ~name:{ txt; loc } ~type_:(fold_ty ~init ~f) ~prim:[])
          in
          let signature_items =
            [ Individual_type_sig.mk
                ~prims
                ~sort_def_map
                ~sort_binding_status
                sort_name
                sort_def
            ; (let template dom = [%type: [%t dom] -> Lvca_syntax.Nominal.Term.t] in
               declare "to_nominal" ~init:(template (t unique_vars)) ~f:template)
            ; (let term = [%type: Lvca_syntax.Nominal.Term.t] in
               let template codom =
                 [%type: [%t term] -> ([%t codom], [%t term]) Result.t]
               in
               declare "of_nominal" ~init:(template (t unique_vars)) ~f:template)
            ; declare
                "info"
                ~init:[%type: [%t t unique_vars] -> Lvca_syntax.Provenance.t]
                ~f:(fun _ -> ptyp_any)
            ; (let template v =
                 [%type:
                   ?info_eq:(Lvca_syntax.Provenance.t -> Lvca_syntax.Provenance.t -> bool)
                   -> [%t v]
                   -> [%t v]
                   -> bool]
               in
               declare "equivalent" ~init:(template (t unique_vars)) ~f:template)
              (*
            ; (let template v = [%type: [%t v] -> [%t v] -> bool] in
               declare "( = )" ~init:(template (t unique_vars)) ~f:template)
                 *)
            ]
          in
          let args_of_arity =
            Ctor_decl.args_of_arity
              ~type_decl_context:Individual_type_module
              ~var_names:taken
              ~mutual_sorts:sort_def_map
              ~prims
          in
          let result_ty = t (List.map ~f:ptyp_var var_names) in
          let ctor_funs =
            List.map op_defs ~f:(fun (Operator_def (_, name, arity)) ->
                (* Take the tail to remove the extra 'info arg *)
                let args = arity |> args_of_arity |> List.tl_exn in
                let type_ =
                  List.fold_right args ~init:result_ty ~f:(fun arg_ty result_ty ->
                      [%type: [%t arg_ty] -> [%t result_ty]])
                in
                let type_ = [%type: info:Lvca_syntax.Provenance.t -> [%t type_]] in
                psig_value
                  (value_description ~name:{ txt = "mk_" ^ name; loc } ~type_ ~prim:[]))
          in
          let var_ctor_fun =
            let type_ =
              [%type: info:Lvca_syntax.Provenance.t -> string -> [%t result_ty]]
            in
            psig_value
              (value_description
                 ~name:{ txt = "mk_" ^ var_ctor_name sort_name; loc }
                 ~type_
                 ~prim:[])
          in
          let ctor_funs =
            match Hashtbl.find sort_binding_status sort_name with
            | Some Bound -> ctor_funs @ [ var_ctor_fun ]
            | _ -> ctor_funs
          in
          let signature_items = signature_items @ ctor_funs in
          let type_ = pmty_signature signature_items in
          psig_module
            (module_declaration ~name:{ txt = Some (module_name sort_name); loc } ~type_))
    in
    let language = [%sigi: val language : Lvca_syntax.Abstract_syntax.t] in
    pmty_signature
      ((language
       :: Wrapper_module.mk_sigs
            ~prims
            ~sort_def_map
            ~sorted_scc_graph
            ~sort_binding_status)
      @ type_sigs)
  ;;
end

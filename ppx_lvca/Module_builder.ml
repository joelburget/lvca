open Base
open Lvca_syntax
open Lvca_provenance
open Ppxlib
module Util = Lvca_util
module SSet = Util.String.Set
module SMap = Util.String.Map
module Syn = Abstract_syntax
module Graph = Directed_graph.Make (Base.String)

let ( >> ), ( << ) = Util.(( >> ), ( << ))

(** {2 Sort variables}

    Example definition: [pair a b := Pair(a; b)]. We translate this to OCaml:

    We end up with something like [type ('a, 'b) pair = Pair of 'a * 'b] (or
    [type ('info, 'a, 'b) pair = Pair of 'info * 'a * 'b]). This is great but we can't
    easily define helpers:

    {[ let of_plain = function Plain.Pair (x1, x2) -> Types.Pair ((), ???, ???) ]} *)

module type Builder_context = sig
  (** The contents of the source file. *)
  val buf : string

  module Ast : Ast_builder.S
end

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
    Set.union (SSet.singleton name) (args |> List.map ~f:all_sort_names |> SSet.union_list)
;;

(* Returns a mapping from the name of each sort defined in this language to
   a) The name of each sort it uses without passing through an external. This
      is used to determine dependencies between sorts.
   b) The set of externals it uses directly. Note we distinguish between [list
      a] and [list b]. Because they may be represented as different types.

   In neither case do we count bound variable types.

   Example: {[
     list : * -> *

     maybe a := None() | Some(a)

     x := ...
     y := ...

     foo :=
      | Ctr1(list x)
      | Ctr2(maybe y)
  ]}

   Here foo directly uses list, maybe, and y, but not x.

   Why (for (a)) do we want only directly-used sorts? Because [maybe x] is
   represented as [x maybe] but [list x] is represented as [Nominal.Term.t]. We
   only want the former for dependency-graph-building purposes.
   *)
let get_sort_ref_info ~prim_names sort_defs =
  (* Sorts defined in this language, not externals. *)
  let known_sorts = sort_defs |> List.map ~f:fst |> SSet.of_list in
  sort_defs
  |> List.map ~f:(fun (sort_name, Syn.Sort_def.Sort_def (vars, op_defs)) ->
         let vars = List.map vars ~f:fst |> SSet.of_list in
         let external_deps =
           op_defs
           |> List.map ~f:(fun (Syn.Operator_def.Operator_def (_name, arity)) ->
                  arity
                  |> List.map ~f:(fun (Syn.Valence.Valence (_sort_slots, body_sort)) ->
                         let sort_name = sort_head body_sort in
                         if Set.mem prim_names sort_name
                         then Set.Poly.singleton body_sort
                         else Set.Poly.empty)
                  |> Set.Poly.union_list)
           |> Set.Poly.union_list
         in
         let sort_deps =
           op_defs
           |> List.map ~f:(fun (Syn.Operator_def.Operator_def (_name, arity)) ->
                  arity
                  |> List.map ~f:(fun (Syn.Valence.Valence (_sort_slots, body_sort)) ->
                         let sort_name = sort_head body_sort in
                         if Set.mem known_sorts sort_name
                         then all_sort_names body_sort
                         else SSet.singleton sort_name)
                  |> SSet.union_list)
           |> SSet.union_list
         in
         (* Only add if it's a known sort name (not an external) and not shadowed by a var. *)
         let sort_deps = Set.diff (Set.inter sort_deps known_sorts) vars in
         sort_name, (Set.to_list sort_deps, external_deps))
  |> SMap.of_alist_exn
;;

type has_info =
  | Plain
  | With_info

(** Is this sort ever bound in the language? *)
type bound_unbound =
  | Bound
  | Unbound

(** Determine whether each sort is ever bound in the language. *)
let partition_sort_defs sort_defs =
  let result =
    sort_defs
    |> List.map ~f:(fun (name, _def) -> name, Unbound)
    |> Hashtbl.of_alist_exn (module String)
  in
  List.iter sort_defs ~f:(fun (_sort_name, Syn.Sort_def.Sort_def (_vars, op_defs)) ->
      List.iter op_defs ~f:(fun (Syn.Operator_def.Operator_def (_name, arity)) ->
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
    { info : has_info
    ; var_names : SSet.t
    ; mutual_sorts : Opt_range.t Syn.Sort_def.t SMap.t
    ; prim_names : SSet.t
    }

  (** When using a sort, we treat it differently depending on if it's a variable, defined
      mutually with the current sort (as defined by [mutual_sorts]), or predefined (either
      as an imported primitive or a sort defined earlier in this language). *)
  type defn_status =
    | Variable
    | Mutual_sort
    | External_sort
    | Predefined_sort of has_info

  let classify_sort context sort =
    let { info; var_names; mutual_sorts; prim_names } = context in
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
        if Set.mem prim_names sort_name then External_sort else Predefined_sort info)
  ;;

  (** Make the type corresponding to a sort in a constructor declaration. Eg:

      [ nonempty := Nonempty (string; list string) ]

      ==>

      If [list] is defined in this module, then [string] is translated into a parameter:

      {[
        ('info, 'string) nonempty = Nonempty of 'info * 'string * ('info, 'string) list
                                                        ^^^^^^^           ^^^^^^^
                                                                  ^^^^^^^^^^^^^^^^^^^^^
      ]}

      If [list] is an external, then

      {[
        ('info, 'string, 'list) nonempty =
            | Nonempty of 'info * 'string * ('info, 'string, 'list) Higher_kinded.t
                                  ^^^^^^^           ^^^^^^^
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ]} *)
  let ptyp_of_sort ~context ~sort_indexed_external_args sort =
    let info_args = match context.info with With_info -> [ [%type: 'info] ] | _ -> [] in
    let rec go sort =
      let loc = update_loc (Sort.info sort) in
      let name, sort_args = Sort.split sort in
      let external_args () =
        name
        |> Map.find_exn sort_indexed_external_args
        |> List.map ~f:(fun sort -> sort |> sort_head |> ptyp_var)
      in
      match classify_sort context sort with
      | Variable -> ptyp_var name
      | Mutual_sort ->
        let sort_args = List.map sort_args ~f:go in
        ptyp_constr
          { txt = unflatten [ name ]; loc }
          (info_args @ sort_args @ external_args ())
      | Predefined_sort info ->
        let mod_name = match info with With_info -> "Types" | _ -> "Plain" in
        ptyp_constr
          { txt = unflatten [ "Wrapper"; mod_name; name ]; loc }
          (info_args @ external_args ())
      | External_sort -> higher_kindify ~loc sort
    and higher_kindify ~loc sort =
      match sort with
      | Sort.Name (_, name) -> ptyp_var name
      | Ap (_, name, lst) ->
        let t_n = match List.length lst with 1 -> "t" | n -> Printf.sprintf "t_%n" n in
        ptyp_constr
          { txt = unflatten [ "Higher_kinded"; t_n ]; loc }
          (List.map lst ~f:go @ [ ptyp_var name ])
    in
    go sort
  ;;

  (** Make a function application for a sort expression, eg in map_info:

      {[
        Types.Cons (x0, x1, x2) -> Types.Cons ((f x0), (a ~f x1), (list a ~f x2))
                                                       ^^^^^^^^^  ^^^^^^^^^^^^^^
      ]}

      This is used in the RHS of function definitions.

      Three cases:

      - the sort is a variable (eg [a] in [list a]): We build an expression like
        [a ~extra_args].
      - otherwise the sort is an external or defined in this language: We build an
        expression like [foo ~extra_args] or [list (a ~extra_args) ~extra_args] . *)
  let rec mk_sort_app
      ~var_names (* Set of variable names bound by the sort being applied *)
      ~sort_defs
      ~sort_indexed_external_args
      ~extra_args
      ~prim_names
      sort
    =
    let context = { info = With_info; var_names; mutual_sorts = sort_defs; prim_names } in
    let sort_name, sort_args = Sort.split sort in
    let external_args =
      match Map.find sort_indexed_external_args sort_name with
      | Some args -> args
      | None -> []
    in
    let var_args =
      match classify_sort context sort with
      | Mutual_sort | Predefined_sort _ | External_sort ->
        List.map (sort_args @ external_args) ~f:(fun sort ->
            ( Nolabel
            , mk_sort_app
                ~var_names
                ~sort_defs
                ~sort_indexed_external_args
                ~extra_args
                ~prim_names
                sort ))
      | Variable -> []
    in
    pexp_apply
      (pexp_ident { txt = Lident sort_name; loc = update_loc (Sort.info sort) })
      (var_args @ extra_args)
  ;;

  let labelled_fun name = pexp_fun (Labelled name) None (ppat_var { txt = name; loc })
  let labelled_arg name = Labelled name, pexp_ident { txt = Lident name; loc }

  (* Make a function taking an f_* argument *)
  let f_fun ?(used = true) var_name =
    pexp_fun Nolabel None (ppat_var { txt = (if used then "" else "_") ^ var_name; loc })
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

  let mk
      ~info
      ~sort_indexed_external_args
      ~var_names
      ~mutual_sorts
      ~prim_names
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    let pattern_type =
      match info with
      | With_info -> [%type: 'info Pattern.t]
      | _ -> [%type: Pattern.Plain.t]
    in
    let args_of_valence (Syn.Valence.Valence (binding_sort_slots, body_sort)) =
      let args =
        List.map binding_sort_slots ~f:(function
            | Syn.Sort_slot.Sort_binding sort ->
              let loc = update_loc (Sort.info sort) in
              (match info with
              | With_info -> [%type: 'info Lvca_syntax.Single_var.t]
              | _ -> [%type: Lvca_syntax.Single_var.Plain.t])
            | Sort_pattern _sort -> pattern_type)
      in
      let context = { info; var_names; mutual_sorts; prim_names } in
      args @ [ ptyp_of_sort ~context ~sort_indexed_external_args body_sort ]
    in
    let args =
      arity
      |> List.map ~f:args_of_valence
      |> (match info with With_info -> List.cons [ [%type: 'info] ] | _ -> Fn.id)
      |> List.map ~f:(function
             | [] -> [%type: unit]
             | [ ty ] -> ty
             | tys -> ptyp_tuple tys)
    in
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

  let mk
      ~info
      ~sort_def_map
      ~sorted_scc_graph
      ~scc_indexed_external_args
      ~sort_indexed_external_args
      ~prim_names
      ~partitioned_sorts
    =
    let info_params = match info with With_info -> [ plain_typ_var "info" ] | _ -> [] in
    sorted_scc_graph
    |> List.map ~f:(fun (scc_num, sort_name_set) ->
           let external_args = Map.find_exn scc_indexed_external_args scc_num in
           sort_name_set
           |> Set.to_list
           |> List.map ~f:(fun sort_name ->
                  let (Syn.Sort_def.Sort_def (vars, op_defs)) =
                    Map.find_exn sort_def_map sort_name
                  in
                  let params =
                    let var_names = List.map vars ~f:fst in
                    var_names @ external_args |> List.map ~f:plain_typ_var
                  in
                  let params = info_params @ params in
                  let op_ctors =
                    List.map
                      op_defs
                      ~f:
                        (Ctor_decl.mk
                           ~info
                           ~sort_indexed_external_args
                           ~var_names:(vars |> List.map ~f:fst |> SSet.of_list)
                           ~mutual_sorts:sort_def_map
                           ~prim_names)
                  in
                  let var_ctor =
                    let args =
                      match info with
                      | With_info -> [ [%type: 'info]; [%type: string] ]
                      | _ -> [ [%type: string] ]
                    in
                    constructor_declaration
                      ~name:{ txt = var_ctor_name sort_name; loc }
                      ~args:(Pcstr_tuple args)
                      ~res:None
                  in
                  let op_ctors =
                    match Hashtbl.find partitioned_sorts sort_name with
                    | Some Bound -> op_ctors @ [ var_ctor ]
                    | _ -> op_ctors
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
      ~has_info
      ?(match_info = false)
      ?(match_non_info = true)
      ?(name_base = "x")
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    if not (is_valid_ocaml_constr_name op_name)
    then Location.raise_errorf ~loc "Invalid OCaml operator name: %s" op_name;
    let var_ix = ref 0 in
    let v ix = ppat_var { txt = Printf.sprintf "%s%d" name_base ix; loc } in
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
    let contents =
      match has_info with
      | With_info -> [ (if match_info then v 0 else ppat_any) ] :: contents
      | Plain -> contents
    in
    let body =
      match contents with
      | [] -> None
      | _ -> Some (contents |> List.map ~f:mk_pat_tuple |> mk_pat_tuple)
    in
    let txt =
      let container_name =
        match has_info with With_info -> "Types" | Plain -> "Plain"
      in
      unflatten [ container_name; op_name ]
    in
    ppat_construct { txt; loc } body
  ;;
end

module Operator_exp (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  type mapping_rhs_ty =
    | Plain
    | With_info

  let mk
      ~has_info (* Building a plain or with-info data type *)
      ~var_names
      ~prim_names
      ~sort_indexed_external_args
      ?(extra_args = [])
      ?(name_base = "x")
      sort_defs (* Sorts being defined together *)
      fun_name (* The name of the function being defined *)
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    let v = evar_allocator name_base in
    let pattern_converter =
      pexp_ident { txt = unflatten [ "Lvca_syntax"; "Pattern"; fun_name ]; loc }
    in
    let body_arg =
      mk_sort_app
        ~var_names
        ~sort_defs
        ~sort_indexed_external_args
        ~extra_args:(extra_args @ [ Nolabel, v () ])
        ~prim_names
    in
    let contents =
      arity
      |> List.map ~f:(fun (Syn.Valence.Valence (slots, body_sort)) ->
             let slots_args =
               List.map slots ~f:(function
                   | Syn.Sort_slot.Sort_binding _sort ->
                     (match has_info with
                     | With_info ->
                       let v = v () in
                       let info =
                         match fun_name with
                         | "of_plain" -> [%expr ()]
                         | "map_info" -> [%expr f [%e v].info]
                         | _ ->
                           Location.Error.(
                             raise
                               (make
                                  ~loc
                                  ~sub:[]
                                  (Printf.sprintf
                                     "Operator_exp: invalid function name: %s"
                                     fun_name)))
                       in
                       [%expr
                         Lvca_syntax.Single_var.{ info = [%e info]; name = [%e v].name }]
                     | Plain ->
                       [%expr Lvca_syntax.Single_var.Plain.{ name = [%e v ()].name }])
                   | Sort_pattern _ ->
                     pexp_apply pattern_converter (extra_args @ [ Nolabel, v () ]))
             in
             slots_args @ [ body_arg body_sort ])
      |> List.map ~f:mk_exp_tuple
    in
    let contents =
      match has_info with
      | With_info ->
        let expr =
          match fun_name with
          | "of_plain" -> [%expr ()]
          | "map_info" -> [%expr f x0]
          | _ ->
            Location.Error.(
              raise
                (make
                   ~loc
                   ~sub:[]
                   (Printf.sprintf "Operator_exp: invalid function name: %s" fun_name)))
        in
        expr :: contents
      | Plain -> contents
    in
    let txt =
      let container_name =
        match has_info with With_info -> "Types" | Plain -> "Plain"
      in
      unflatten [ container_name; op_name ]
    in
    pexp_construct { txt; loc } (mk_exp_tuple' contents)
  ;;
end

module To_plain (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let mk
      ~prim_names
      ~partitioned_sorts
      ~external_args
      ~sort_indexed_external_args
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let arg_names = List.map ~f:fst vars @ external_args in
    let var_names = SSet.of_list arg_names in
    let f op_def =
      let lhs = Operator_pat.mk ~has_info:With_info op_def in
      let rhs =
        Operator_exp.mk
          ~var_names
          ~prim_names
          ~sort_indexed_external_args
          ~has_info:Plain
          sort_defs
          "to_plain"
          op_def
      in
      case ~lhs ~guard ~rhs
    in
    let op_defs = List.map op_defs ~f in
    let var_case =
      case
        ~lhs:
          (ppat_construct
             { txt = unflatten [ "Types"; var_ctor_name sort_name ]; loc }
             (Some [%pat? _, name]))
        ~guard
        ~rhs:
          (pexp_construct
             { txt = unflatten [ "Plain"; var_ctor_name sort_name ]; loc }
             (Some [%expr name]))
    in
    let op_defs =
      match Hashtbl.find partitioned_sorts sort_name with
      | Some Bound -> op_defs @ [ var_case ]
      | _ -> op_defs
    in
    let expr = List.fold_right arg_names ~init:(pexp_function op_defs) ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Of_plain (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let mk
      ~prim_names
      ~partitioned_sorts
      ~external_args
      ~sort_indexed_external_args
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~has_info:Plain op_def in
      let rhs =
        Operator_exp.mk
          ~var_names
          ~prim_names
          ~sort_indexed_external_args
          ~has_info:With_info
          sort_defs
          "of_plain"
          op_def
      in
      case ~lhs ~guard ~rhs
    in
    let op_defs = List.map op_defs ~f in
    let var_case =
      case
        ~lhs:
          (ppat_construct
             { txt = unflatten [ "Plain"; var_ctor_name sort_name ]; loc }
             (Some [%pat? name]))
        ~guard
        ~rhs:
          (pexp_construct
             { txt = unflatten [ "Types"; var_ctor_name sort_name ]; loc }
             (Some [%expr (), name]))
    in
    let op_defs =
      match Hashtbl.find partitioned_sorts sort_name with
      | Some Bound -> op_defs @ [ var_case ]
      | _ -> op_defs
    in
    let expr =
      List.fold_right
        (List.map ~f:fst vars @ external_args)
        ~init:(pexp_function op_defs)
        ~f:f_fun
    in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Map_info (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let mk
      ~prim_names
      ~partitioned_sorts
      ~external_args
      ~sort_indexed_external_args
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~has_info:With_info ~match_info:true op_def in
      let rhs =
        Operator_exp.mk
          ~var_names
          ~prim_names
          ~sort_indexed_external_args
          ~has_info:With_info
          ~extra_args:[ labelled_arg "f" ]
          sort_defs
          "map_info"
          op_def
      in
      case ~lhs ~guard ~rhs
    in
    let op_defs = List.map op_defs ~f in
    let var_case =
      case
        ~lhs:
          (ppat_construct
             { txt = unflatten [ "Types"; var_ctor_name sort_name ]; loc }
             (Some [%pat? info, name]))
        ~guard
        ~rhs:
          (pexp_construct
             { txt = unflatten [ "Types"; var_ctor_name sort_name ]; loc }
             (Some [%expr f info, name]))
    in
    let op_defs =
      match Hashtbl.find partitioned_sorts sort_name with
      | Some Bound -> op_defs @ [ var_case ]
      | _ -> op_defs
    in
    let init = op_defs |> pexp_function |> labelled_fun "f" in
    let expr = List.fold_right (List.map ~f:fst vars @ external_args) ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module To_nominal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let operator = unflatten [ "Lvca_syntax"; "Nominal"; "Term"; "Operator" ]

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
      ~prim_names
      ~sort_indexed_external_args
      sort_defs
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    (* TODO: change to name "info" *)
    let info = evar "x0" in
    let v = evar_allocator "x" in
    let body_arg =
      mk_sort_app
        ~var_names
        ~sort_defs
        ~sort_indexed_external_args
        ~extra_args:[ Nolabel, v () ]
        ~prim_names
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
    let children = List.map arity ~f:mk_scope |> Syntax_quoter.Exp.list ~loc in
    let body = mk_exp_tuple' [ info; estring op_name; children ] in
    pexp_construct { txt = operator; loc } body
  ;;

  let mk
      ~prim_names
      ~partitioned_sorts
      ~external_args
      ~sort_indexed_external_args
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~has_info:With_info ~match_info:true op_def in
      let rhs =
        mk_nominal_exp ~var_names ~prim_names ~sort_indexed_external_args sort_defs op_def
      in
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
      match Hashtbl.find partitioned_sorts sort_name with
      | Some Bound -> op_defs @ [ var_case ]
      | _ -> op_defs
    in
    let expr =
      List.fold_right
        (List.map ~f:fst vars @ external_args)
        ~init:(pexp_function op_defs)
        ~f:f_fun
    in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Of_nominal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let operator = unflatten [ "Lvca_syntax"; "Nominal"; "Term"; "Operator" ]

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
  let mk_nominal_pat (Syn.Operator_def.Operator_def (op_name, arity)) =
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
    ppat_construct { txt = operator; loc } body
  ;;

  let mk_exp
      ~var_names
      ~prim_names
      ~sort_indexed_external_args
      sort_defs
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    let v = var_allocator "x" in
    let ev = v >> snd in
    let info = evar "x0" in
    (* Queue of (variable, conversion to perform) *)
    let conversions_needed = Queue.create () in
    let body_arg ev =
      mk_sort_app
        ~var_names
        ~sort_defs
        ~sort_indexed_external_args
        ~extra_args:[ Nolabel, ev ]
        ~prim_names
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
             let conversion = body_arg ev body_sort in
             Queue.enqueue conversions_needed (pv, conversion);
             slots_args @ [ ev ])
      |> List.map ~f:mk_exp_tuple
    in
    let contents = info :: contents in
    let txt = unflatten [ "Types"; op_name ] in
    let type_constr = pexp_construct { txt; loc } (mk_exp_tuple' contents) in
    conversions_needed, [%expr Ok [%e type_constr]]
  ;;

  let mk
      ~prim_names
      ~partitioned_sorts
      ~external_args
      ~sort_indexed_external_args
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = mk_nominal_pat op_def in
      let rhs =
        let conversions_needed, init =
          mk_exp ~var_names ~prim_names ~sort_indexed_external_args sort_defs op_def
        in
        let f (v, conversion) accum =
          [%expr
            match [%e conversion] with Error msg -> Error msg | Ok [%p v] -> [%e accum]]
          (* Can also do this way but I think matching is clearer:
             [%expr [%e conversion] |> Result.bind ~f:(fun [%p v] -> [%e accum])] *)
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
    let fallthrough =
      case ~lhs:(ppat_var { txt = "tm"; loc }) ~guard ~rhs:[%expr Error tm]
    in
    let matching_cases =
      match Hashtbl.find partitioned_sorts sort_name with
      | Some Bound -> matching_cases @ [ var_case ]
      | _ -> matching_cases
    in
    let init = matching_cases @ [ fallthrough ] |> pexp_function in
    let expr = List.fold_right (List.map ~f:fst vars @ external_args) ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Equal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  let mk
      ~prim_names
      ~sort_indexed_external_args
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f (Syn.Operator_def.Operator_def (_op_name, arity) as op_def) =
      let lhs =
        let p1, p2 =
          ("x", "y")
          |> Lvca_util.Tuple2.map ~f:(fun name_base ->
                 Operator_pat.mk ~has_info:With_info ~match_info:true ~name_base op_def)
        in
        [%pat? [%p p1], [%p p2]]
      in
      let rhs =
        let arity_exps =
          let var_ix = ref 0 in
          let mk_xy () =
            ("x", "y")
            |> Lvca_util.Tuple2.map ~f:(fun base ->
                   evar (Printf.sprintf "%s%d" base !var_ix))
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
                            [%expr Base.String.( = ) [%e x] [%e y]]
                          | Sort_pattern _ ->
                            [%expr Lvca_syntax.Pattern.equal ~info_eq [%e x] [%e y]])
                 in
                 let body_check =
                   Int.incr var_ix;
                   let x, y = mk_xy () in
                   let extra_args = [ labelled_arg "info_eq"; Nolabel, x; Nolabel, y ] in
                   mk_sort_app
                     ~var_names
                     ~sort_defs
                     ~extra_args
                     ~prim_names
                     ~sort_indexed_external_args
                     body_sort
                 in
                 slots_checks @ [ body_check ])
          |> List.join
        in
        conjuntion ~loc ([%expr info_eq x0 y0] :: arity_exps)
      in
      case ~lhs ~guard ~rhs
    in
    let branches = List.map op_defs ~f in
    let branches =
      match op_defs with
      | [] | [ _ ] -> branches
      | _ ->
        (* If there's more than one operator we need to add an extra case *)
        let last_branch = case ~lhs:[%pat? _, _] ~guard ~rhs:[%expr false] in
        Lvca_util.List.snoc branches last_branch
    in
    let init =
      branches
      |> pexp_match [%expr t1, t2]
      |> pexp_fun Nolabel None (ppat_var { txt = "t2"; loc })
      |> pexp_fun Nolabel None (ppat_var { txt = "t1"; loc })
      |> labelled_fun "info_eq"
    in
    let expr = List.fold_right (List.map ~f:fst vars) ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Info (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  let mk
      ~prim_names:_
      ~partitioned_sorts
      ~external_args
      ~sort_indexed_external_args:_
      _sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let mk_case op_def =
      let lhs =
        Operator_pat.mk ~has_info:With_info ~match_info:true ~match_non_info:false op_def
      in
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
        match Hashtbl.find partitioned_sorts sort_name with
        | Some Bound -> op_defs @ [ var_case ]
        | _ -> op_defs
      in
      List.fold_right
        (List.map ~f:fst vars @ external_args)
        ~init:(pexp_function op_defs)
        ~f:(f_fun ~used:false)
    in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

(* The wrapper module holds TODO *)
module Wrapper_module (Context : Builder_context) = struct
  open Context
  open Ast
  module Type_decls = Type_decls (Context)
  module Info = Info (Context)
  module To_plain = To_plain (Context)
  module Of_plain = Of_plain (Context)
  module Map_info = Map_info (Context)
  module To_nominal = To_nominal (Context)
  module Of_nominal = Of_nominal (Context)

  let mk
      ~prim_names
      ~sort_def_map
      ~sort_dep_map
      ~scc_indexed_external_args
      ~sort_indexed_external_args
      ~sorted_scc_graph
      sort_defs
    =
    let scc_indexed_external_args =
      Map.map scc_indexed_external_args ~f:(List.map ~f:sort_head)
    in
    let partitioned_sorts = partition_sort_defs sort_defs in
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
          prim_names:SSet.t
          -> partitioned_sorts:(string, bound_unbound) Hashtbl.t
          -> external_args:string list
          -> sort_indexed_external_args:Opt_range.t Sort.t list SMap.t
          -> _ Syn.Sort_def.t SMap.t
          -> string
          -> _ Syn.Sort_def.t
          -> Ppxlib.value_binding)
      =
      ordered_sccs
      |> List.map ~f:(fun (scc_num, named_sort) ->
             let external_args = Map.find_exn scc_indexed_external_args scc_num in
             let value_bindings =
               List.map named_sort ~f:(fun (sort_name, sort_def) ->
                   maker
                     ~prim_names
                     ~partitioned_sorts
                     ~external_args
                     ~sort_indexed_external_args
                     sort_def_map
                     sort_name
                     sort_def)
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
      Type_decls.mk
        ~sort_def_map
        ~sorted_scc_graph
        ~scc_indexed_external_args
        ~sort_indexed_external_args
        ~prim_names
        ~partitioned_sorts
    in
    let info_decls = type_decls ~info:With_info in
    let plain_decls = type_decls ~info:Plain in
    let defs =
      (* Each of these functions is potentially recursive (across multiple types), pre-declare. *)
      [%str
        module Types = struct
          [%%i pstr_type Recursive info_decls]
        end

        module Plain = struct
          [%%i pstr_type Recursive plain_decls]
        end

        module Info = [%m adapt ~definite_rec:Nonrecursive Info.mk]
        module To_plain = [%m adapt To_plain.mk]
        module Of_plain = [%m adapt Of_plain.mk]
        module Map_info = [%m adapt Map_info.mk]
        module To_nominal = [%m adapt To_nominal.mk]
        module Of_nominal = [%m adapt Of_nominal.mk]]
    in
    let wrapper_module =
      module_binding ~name:{ txt = Some "Wrapper"; loc } ~expr:(pmod_structure defs)
      |> pstr_module
    in
    wrapper_module, psig_type Recursive info_decls, psig_type Recursive plain_decls
  ;;
end

module Individual_type_module (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  let mk
      ~prim_names:_
      ~sort_def_map:_
      ~external_args
      sort_name
      (Syn.Sort_def.Sort_def (vars, _op_defs))
    =
    let var_names = List.map vars ~f:fst in
    let var_names' = var_names @ external_args in
    let type_vars = List.map var_names' ~f:ptyp_var in
    let params = List.map var_names' ~f:plain_typ_var in
    let plain_type_decl =
      let manifest =
        Some
          (ptyp_constr
             { txt = unflatten [ "Wrapper"; "Plain"; sort_name ]; loc }
             type_vars)
      in
      pstr_type
        Recursive
        [ type_declaration
            ~name:{ txt = "t"; loc }
            ~params
            ~cstrs:[]
            ~kind:Ptype_abstract
            ~private_:Public
            ~manifest
        ]
    in
    let info_type_decl =
      let manifest =
        Some
          (ptyp_constr
             { txt = unflatten [ "Wrapper"; "Types"; sort_name ]; loc }
             (ptyp_var "info" :: type_vars))
      in
      pstr_type
        Recursive
        [ type_declaration
            ~name:{ txt = "t"; loc }
            ~params:(plain_typ_var "info" :: params)
            ~cstrs:[]
            ~kind:Ptype_abstract
            ~private_:Public
            ~manifest
        ]
    in
    let fun_defs =
      [ "info", "Info"
      ; "to_plain", "To_plain"
      ; "of_plain", "Of_plain"
      ; "map_info", "Map_info"
      ; "to_nominal", "To_nominal"
      ; "of_nominal", "Of_nominal"
      ]
      |> List.map ~f:(fun (fun_name, mod_name) ->
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
                 | "equal" -> [ labelled_arg "info_eq" ]
                 | "map_info" -> [ labelled_arg "f" ]
                 | _ -> []
               in
               let tm = pexp_ident { txt = Lident "tm"; loc } in
               let args = args @ Util.List.snoc labelled_args (Nolabel, tm) in
               pexp_apply wrapper_fun args
             in
             let expr = pexp_fun Nolabel None (ppat_var { txt = "tm"; loc }) expr in
             let expr =
               match fun_name with
               | "equal" -> labelled_fun "info_eq" expr
               | "map_info" -> labelled_fun "f" expr
               | _ -> expr
             in
             let pat = ppat_var { txt = fun_name; loc } in
             pstr_value Nonrecursive [ value_binding ~pat ~expr ])
    in
    let kernel_expr =
      pmod_structure
        (info_type_decl
         :: [%stri module Plain = [%m pmod_structure [ plain_type_decl ]]] :: fun_defs)
    in
    let module_name = module_name sort_name in
    (*
      module Foo = struct
        module Foo_kernel = struct ... end
        include Foo_kernel
        include Extend (Foo_kernel)
      end
    *)
    let kernel_mod = pmod_ident { txt = Lident "Kernel"; loc } in
    (*
    let extend_mod =
      pmod_ident
        { txt = unflatten [ "Lvca_syntax"; "Nominal"; "Convertible"; "Extend" ]; loc }
    in
    *)
    let expr =
      pmod_structure
        [ [%stri module Kernel = [%m kernel_expr]]
        ; pstr_include (include_infos kernel_mod)
          (* ; pstr_include (include_infos (pmod_apply extend_mod kernel_mod)) *)
        ]
    in
    module_binding ~name:{ txt = Some module_name; loc } ~expr |> pstr_module
  ;;
end

(* The top-level container / result. *)
module Container_module (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Wrapper_module = Wrapper_module (Context)
  module Individual_type_module = Individual_type_module (Context)

  let mk Syn.{ externals; sort_defs } =
    let prim_names = externals |> List.map ~f:fst |> SSet.of_list in
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let sort_info_map = get_sort_ref_info ~prim_names sort_defs in
    let sort_dep_map = Map.map sort_info_map ~f:fst in
    let sort_external_map = Map.map sort_info_map ~f:snd in
    let connected_components = Graph.connected_components sort_dep_map in
    let Graph.Connected_components.{ scc_graph; sccs } = connected_components in
    let sorted_scc_graph =
      scc_graph
      |> Directed_graph.Int.topsort_exn
      |> List.map ~f:(fun i -> i, Map.find_exn sccs i)
    in
    let scc_external_args_map =
      sccs
      |> Map.map ~f:(fun sort_set ->
             sort_set
             |> Set.to_list
             |> List.map ~f:(Map.find_exn sort_external_map)
             |> Set.Poly.union_list
             |> Set.to_list)
    in
    let scc_indexed_external_args =
      scc_graph
      |> Map.mapi ~f:(fun ~key ~data:deps ->
             let direct_externals = Map.find_exn scc_external_args_map key in
             let indirect_externals =
               deps |> List.map ~f:(Map.find_exn scc_external_args_map)
             in
             direct_externals :: indirect_externals
             |> List.join
             |> Set.Poly.of_list
             |> Set.to_list)
    in
    let sort_indexed_external_args =
      sorted_scc_graph
      |> List.map ~f:(fun (scc_num, scc) ->
             let scc_external_args = Map.find_exn scc_indexed_external_args scc_num in
             scc
             |> Set.to_list
             |> List.map ~f:(fun sort_name -> sort_name, scc_external_args))
      |> List.join
      |> SMap.of_alist_exn
    in
    (* pre-declare types *)
    let wrapper_module, info_types_sig, plain_types_sig =
      Wrapper_module.mk
        ~prim_names
        ~sort_def_map
        ~sort_dep_map
        ~scc_indexed_external_args
        ~sort_indexed_external_args
        ~sorted_scc_graph
        sort_defs
    in
    let type_modules =
      sort_defs
      |> List.map ~f:(fun (sort_name, sort_def) ->
             let external_args =
               sort_name
               |> Map.find_exn sort_indexed_external_args
               |> List.map ~f:sort_head
             in
             Individual_type_module.mk
               ~prim_names
               ~sort_def_map
               ~external_args
               sort_name
               sort_def)
    in
    (* TODO: include language?
  let sort_defs =
    [%str let language = [%e Syntax_quoter.mk_language ~loc lang]] @ sort_defs
  in
  *)
    let expr =
      pmod_structure
        ([ wrapper_module
         ; [%stri module Types = Wrapper.Types]
         ; [%stri module Plain = Wrapper.Plain]
         ]
        @ type_modules)
    in
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
      (*
    let info = ptyp_var "info" in
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
          |> List.map ~f:(fun name ->
                 ( make [ module_name name; "t" ] [ info ]
                 , make [ module_name name; "Plain"; "t" ] [] ))
          |> List.unzip
        in
        info :: info_args, plain_args
      in
      let sort_module_sigs =
        sorted_scc_graph
        |> List.map ~f:(fun (scc_num, sort_name_set) ->
               sort_name_set
               |> Set.to_list
               |> List.map ~f:(fun sort_name ->
                      let (Syn.Sort_def.Sort_def (vars, _op_defs)) =
                        Map.find_exn sort_def_map sort_name
                      in
                      let external_args =
                        Map.find_exn scc_indexed_external_args scc_num
                      in
                      let var_names = List.map ~f:fst vars in
                      let info_args, plain_args = mk_args (var_names @ external_args) in
                      let params =
                        List.map (var_names @ external_args) ~f:plain_typ_var
                      in
                      let type_decl =
                        let info_ty = { txt = unflatten [ "Types"; sort_name ]; loc } in
                        let params = (info, (NoVariance, NoInjectivity)) :: params in
                        tdecl ~params ~manifest:(Some (ptyp_constr info_ty info_args))
                      in
                      let plain_type_decl =
                        let plain_ty = { txt = unflatten [ "Plain"; sort_name ]; loc } in
                        tdecl ~params ~manifest:(Some (ptyp_constr plain_ty plain_args))
                      in
                      let type_ =
                        pmty_with
                          nominal_convertible_s
                          [ Pwith_type ({ txt = Lident "t"; loc }, type_decl)
                          ; Pwith_type
                              ({ txt = unflatten [ "Plain"; "t" ]; loc }, plain_type_decl)
                          ]
                      in
                      let name = { txt = Some (module_name sort_name); loc } in
                      psig_module (module_declaration ~name ~type_)))
        |> List.join
      in
      pmty_signature (types_sig :: plain_sig :: sort_module_sigs)
      *)
      pmty_signature [ types_sig; plain_sig ]
    in
    pmod_constraint expr mod_ty
  ;;
end

open Base
open Lvca_syntax
open Lvca_provenance
open Ppxlib
module Util = Lvca_util
module SSet = Util.String.Set
module SMap = Util.String.Map
module Syn = Abstract_syntax

let ( >> ), ( << ) = Util.(( >> ), ( << ))

(** {2 Sort variables}

    Example definition: [pair a b := Pair(a; b)]. We have two choices for how to translate
    this to OCaml:

    {3 1. type variables}

    We end up with something like [type ('a, 'b) pair = Pair of 'a * 'b] (or
    [type ('info, 'a, 'b) pair = Pair of 'info * 'a * 'b]). This is great but we can't
    easily define helpers:

    {[ let of_plain = function Plain.Pair (x1, x2) -> Types.Pair ((), ???, ???) ]}

    {3 2. functor}

    We define

    {[
      module Pair (A : Language_object_intf.S) (B : Language_object_intf.S) = struct
        type 'info t = Pair of 'info * 'info A.t * 'info B.t
        ...
      end
    ]}

    This might work actually? Except it's kind of a massive pain given that we define all
    types together and all modules separately.

    {3 3. hybrid}

    {v
integer : *
string : *
pair a b := Pair(a; b)
uses_pair := UsesPair(pair integer string)
diag a := Diag(pair a a)
    v}

    ==>

    {[
      module Types = struct
        type ('info, 'a, 'b) pair = Pair of 'info * 'a * 'b
        type 'info uses_pair = UsesPair of 'info * ('info, Integer.t, String.t) pair
        type ('info, 'a) diag = Diag of ('info, 'a, 'a) pair
      end

      module Of_plain = struct
        let pair f_a f_b = function
          | Plain.Pair (x1, x2) -> Types.Pair ((), f_a x1, f_b x2)
        ;;

        let uses_pair = function
          | Plain.UsesPair x1 ->
            Types.UsesPair ((), pair Integer.of_plain String.of_plain x1)
        ;;

        let diag f_a = function Plain.Diag x1 -> Types.Diag ((), pair f_a f_a x1)
      end

      module Pair (A : Language_object_intf.S) (B : Language_object_intf.S) = struct
        type 'info t = ('info, 'info A.t, 'info B.t) Types.pair =
          | Pair of 'info * 'info A.t * 'info B.t

        let of_plain = Of_plain.pair A.of_plain B.of_plain
      end

      module UsesPair = struct
        type 'info uses_pair = ('info, 'info Integer.t, 'info String.t) Types.uses_pair =
          | UsesPair of 'info * ('info, Integer.t, 'info String.t) pair

        let of_plain = Of_plain.uses_pair
      end

      module Diag (A : Language_object_intf.S) = struct
        type 'info diag = ('info, 'info A.t) Types.diag =
          | Diag of 'info * ('info, 'info A.t, 'info A.t) pair

        let of_plain = Of_plain.diag A.of_plain
      end
    ]}

    {2 Handling of sorts}

    There are a few factors determining the definition and use of a given sort:

    {ul
     {- The use of ['info] or not }
     {- Whether
        the
        sort
        is:

        {ul
         {- an external }
         {- a variable }
         {- or
            defined
            in
            this
            language,
            in
            which
            case,
            whether

            - the sort is part of the SCC we're currently defining
            - or, was previously defined
         }
        }
     }
    } *)

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
let guard = None

let conjuntion ~loc exps =
  let exps, last = Lvca_util.List.unsnoc exps in
  List.fold_right exps ~init:last ~f:(fun e1 e2 -> [%expr [%e e1] && [%e e2]])
;;

let rec all_sort_names = function
  | Sort.Name (_, name) -> SSet.singleton name
  | Sort.Ap (_, name, args) ->
    Set.union (SSet.singleton name) (args |> List.map ~f:all_sort_names |> SSet.union_list)
;;

let get_sort_ref_map sort_defs =
  let known_sorts = sort_defs |> List.map ~f:fst |> SSet.of_list in
  sort_defs
  |> List.map ~f:(fun (sort_name, Syn.Sort_def.Sort_def (vars, op_defs)) ->
         let vars = List.map vars ~f:fst |> SSet.of_list in
         let connections =
           op_defs
           |> List.map ~f:(fun (Syn.Operator_def.Operator_def (_name, arity)) ->
                  arity
                  |> List.map ~f:(fun (Syn.Valence.Valence (_sort_slots, body_sort)) ->
                         all_sort_names body_sort)
                  |> SSet.union_list)
           |> SSet.union_list
         in
         (* Only add if it's a known sort name (not an external) and not shadowed by a var. *)
         let connections = Set.diff (Set.inter connections known_sorts) vars in
         sort_name, Set.to_list connections)
  |> SMap.of_alist_exn
;;

module Helpers (Context : Builder_context) = struct
  open Context
  open Ast
  module Update_loc = Update_loc (Context)

  let update_loc rng = Update_loc.go rng

  (** Context for classifying a sort ([classify_sort]) as [defn_status]. *)
  type context =
    { info : bool
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
    | Predefined_sort of
        { prim : bool
        ; info : bool
        }

  let classify_sort context sort =
    let { info; var_names; mutual_sorts; prim_names } = context in
    let sort_name, sort_args = Sort.split sort in
    if Set.mem var_names sort_name
    then (
      assert (List.(is_empty sort_args));
      Variable)
    else (
      match Map.find mutual_sorts sort_name with
      | Some (Syn.Sort_def.Sort_def (vars, _op_defs)) ->
        assert (List.(Int.(length vars = length sort_args)));
        Mutual_sort
      | None -> Predefined_sort { prim = Set.mem prim_names sort_name; info })
  ;;

  (** Make the type corresponding to a sort in a constructor declaration. Eg:

      {v nonempty := Nonempty (string; list string) v}

      ==>

      {[
        Nonempty of 'info * 'info String.t * ('info, 'info String.t) list
                            ^^^^^^^^^^^^^^            ^^^^^^^^^^^^^^
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ]} *)
  let ptyp_of_sort context sort =
    (* let { info; var_names; mutual_sorts; prim_names } = context in *)
    let args = if context.info then [ [%type: 'info] ] else [] in
    let rec go sort =
      let loc = update_loc (Sort.info sort) in
      let name, sort_args = Sort.split sort in
      match classify_sort context sort with
      | Variable -> ptyp_var name
      | Mutual_sort ->
        let extra_args = List.map sort_args ~f:go in
        ptyp_constr { txt = unflatten [ name ]; loc } (args @ extra_args)
      | Predefined_sort { prim; info } ->
        let qualified_name =
          match prim, info with
          | true, true -> [ module_name name; "t" ]
          | true, false -> [ module_name name; "Plain"; "t" ]
          | false, true -> [ "Wrapper"; "Types"; name ]
          | false, false -> [ "Wrapper"; "Plain"; name ]
        in
        ptyp_constr { txt = unflatten qualified_name; loc } args
    in
    go sort
  ;;

  (** Make a function application for a sort expression, eg in map_info:

      {[
        Types.Cons (x0, x1, x2) -> Types.Cons ((f x0), (f_a ~f x1), (list f_a ~f x2))
                                                       ^^^^^^^^^^^  ^^^^^^^^^^^^^^^^
      ]}

      This is used in the RHS of function definitions.

      Three cases:

      - the sort is defined in this language: * We build an expression like
        [sort_name f_a ~extra_args]
      - the sort is a variable (eg [a] in [list a]): * We build an expression like
        [f_sort_name ~extra_args]
      - otherwise, it's an external * We build an expression
        [Sort_name.fun_name ~extra_args] *)
  let mk_sort_app
      ~var_names (* Set of variable names bound by the sort being applied *)
      ~fun_name (* The name of the function being defined *)
      ~sort_defs
      ~sort
      ~extra_args
      ~prim_names
    =
    let context = { info = true; var_names; mutual_sorts = sort_defs; prim_names } in
    let sort_name, sort_args = Sort.split sort in
    (* Make the extra arguments to a function specific to that type, eg in map_info:

    {[
      Types.Cons (x0, x1, x2) -> Types.Cons ((f x0), (f_a ~f x1), (list ~f f_a x2))
                                                                           ^^^
    ]}

    Call this once for each argument applied to a sort. Cases:

      - If the argument is a variable, eg [a] in [list a], then use [f_a].
      - If it's a sort being defined mutually with this one, the appropriate
        function is in scope as [sort_name]
      - Otherwise, it's predefined, either as an earlier defined sort in this
        module, or an external.
    *)
    let mk_var_arg sort' =
      let sort_name, _sort_args = Sort.split sort' in
      match classify_sort context sort' with
      | Variable -> evar ("f_" ^ sort_name)
      | Mutual_sort -> evar sort_name (* XXX args? *)
      | Predefined_sort { prim; info } ->
        let qualified_name =
          match prim, info with
          | true, true -> [ module_name sort_name; fun_name ]
          | true, false -> [ module_name sort_name; "Plain"; fun_name ]
          | false, true -> [ "Wrapper"; "Types"; sort_name ]
          | false, false -> [ "Wrapper"; "Plain"; sort_name ]
        in
        pexp_ident { txt = unflatten qualified_name; loc = update_loc (Sort.info sort') }
    in
    let txt, var_args =
      match classify_sort context sort with
      | Variable -> Lident ("f_" ^ sort_name), []
      | Mutual_sort ->
        let sort_args = List.map sort_args ~f:(fun sort -> Nolabel, mk_var_arg sort) in
        Lident sort_name, sort_args
      | Predefined_sort _ -> unflatten [ module_name sort_name; fun_name ], []
    in
    pexp_apply
      (pexp_ident { txt; loc = update_loc (Sort.info sort) })
      (var_args @ extra_args)
  ;;

  let labelled_fun name = pexp_fun (Labelled name) None (ppat_var { txt = name; loc })
  let labelled_arg name = Labelled name, pexp_ident { txt = Lident name; loc }

  (* Make a function taking an f_* argument *)
  let f_fun ?(used = true) (var_name, _kind_opt) =
    let txt = (if used then "f_" else "_f_") ^ var_name in
    pexp_fun Nolabel None (ppat_var { txt; loc })
  ;;

  let all_term_s = pmty_ident { txt = unflatten [ "Language_object_intf"; "S" ]; loc }

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

  let raise_kind_err name (Syn.Kind.Kind (info, _) as kind) =
    let loc = update_loc info in
    Location.Error.(
      raise
        (make
           ~loc
           ~sub:[]
           (Fmt.str
              "Code generation currently only supports external modules of kind * (`%s` \
               is %a)"
              name
              Syn.Kind.pp
              kind)))
  ;;
end

(** Helper for declaring a constructor. *)
module Ctor_decl (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  let mk
      ~info
      ~var_names
      ~mutual_sorts
      ~prim_names
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    let pattern_type =
      if info then [%type: 'info Pattern.t] else [%type: Pattern.Plain.t]
    in
    let args_of_valence (Syn.Valence.Valence (binding_sort_slots, body_sort)) =
      let args =
        List.map binding_sort_slots ~f:(function
            | Syn.Sort_slot.Sort_binding sort ->
              let loc = update_loc (Sort.info sort) in
              [%type: string]
            | Sort_pattern _sort -> pattern_type)
      in
      let context = { info; var_names; mutual_sorts; prim_names } in
      args @ [ ptyp_of_sort context body_sort ]
    in
    let args =
      arity
      |> List.map ~f:args_of_valence
      |> (if info then List.cons [ [%type: 'info] ] else Fn.id)
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

  let mk ~info ~sort_def_map ~prim_names =
    let params0 = if info then [ ptyp_var "info", (NoVariance, NoInjectivity) ] else [] in
    sort_def_map
    |> Map.to_alist
    |> List.map ~f:(fun (sort_name, Syn.Sort_def.Sort_def (vars, op_defs)) ->
           let var_names = vars |> List.map ~f:fst |> SSet.of_list in
           let mk_var name = ptyp_var name, (NoVariance, NoInjectivity) in
           let params = vars |> List.map ~f:fst |> List.map ~f:mk_var in
           let params = params0 @ params in
           let kind =
             Ptype_variant
               (List.map
                  op_defs
                  ~f:
                    (Ctor_decl.mk ~info ~var_names ~mutual_sorts:sort_def_map ~prim_names))
           in
           type_declaration
             ~name:{ txt = sort_name; loc }
             ~params
             ~cstrs:[]
             ~kind
             ~private_:Public
             ~manifest:None)
  ;;
end

module Operator_pat (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  type ctor_type =
    | Plain
    | With_info

  let is_valid_ocaml_constr_name str =
    (not (String.is_empty str)) && Char.is_uppercase str.[0]
  ;;

  let mk
      ~ctor_type
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
      match ctor_type with
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
        match ctor_type with With_info -> "Types" | Plain -> "Plain"
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
    | With_info of expression

  let mk
      ~ctor_type (* Building a plain or with-info data type *)
      ~var_names
      ~prim_names
      ?(extra_args = [])
      ?(name_base = "x")
      sort_defs (* Sorts being defined together *)
      fun_name (* The name of the function being defined *)
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    let v = evar_allocator name_base in
    let pattern_converter = pexp_ident { txt = unflatten [ "Pattern"; fun_name ]; loc } in
    let body_arg sort =
      mk_sort_app
        ~var_names
        ~fun_name
        ~sort_defs
        ~sort
        ~extra_args:(extra_args @ [ Nolabel, v () ])
        ~prim_names
    in
    let contents =
      arity
      |> List.map ~f:(fun (Syn.Valence.Valence (slots, body_sort)) ->
             let slots_args =
               slots
               |> List.map ~f:(fun slot ->
                      match slot with
                      | Syn.Sort_slot.Sort_binding _sort -> v ()
                      | Sort_pattern _ ->
                        pexp_apply pattern_converter (extra_args @ [ Nolabel, v () ]))
             in
             slots_args @ [ body_arg body_sort ])
      |> List.map ~f:mk_exp_tuple
    in
    let contents =
      match ctor_type with With_info expr -> expr :: contents | Plain -> contents
    in
    let txt =
      let container_name =
        match ctor_type with With_info _ -> "Types" | Plain -> "Plain"
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

  let mk ~prim_names sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~ctor_type:With_info op_def in
      let rhs =
        Operator_exp.mk
          ~var_names
          ~prim_names
          ~ctor_type:Plain
          sort_defs
          "to_plain"
          op_def
      in
      case ~lhs ~guard ~rhs
    in
    let init = op_defs |> List.map ~f |> pexp_function in
    let expr = List.fold_right vars ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Of_plain (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let mk ~prim_names sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~ctor_type:Plain op_def in
      let rhs =
        Operator_exp.mk
          ~var_names
          ~prim_names
          ~ctor_type:(With_info [%expr ()])
          sort_defs
          "of_plain"
          op_def
      in
      case ~lhs ~guard ~rhs
    in
    let init = op_defs |> List.map ~f |> pexp_function in
    let expr = List.fold_right vars ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Map_info (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let mk ~prim_names sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~ctor_type:With_info ~match_info:true op_def in
      let rhs =
        Operator_exp.mk
          ~var_names
          ~prim_names
          ~ctor_type:(With_info [%expr f x0])
          ~extra_args:[ labelled_arg "f" ]
          sort_defs
          "map_info"
          op_def
      in
      case ~lhs ~guard ~rhs
    in
    let init = op_defs |> List.map ~f |> pexp_function |> labelled_fun "f" in
    let expr = List.fold_right vars ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module To_nominal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let operator = unflatten [ "Nominal"; "Term"; "Operator" ]

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
      sort_defs
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    (* TODO: change to name "info" *)
    let info = evar "x0" in
    let v = evar_allocator "x" in
    let body_arg sort =
      mk_sort_app
        ~var_names
        ~fun_name:"to_nominal"
        ~sort_defs
        ~sort
        ~extra_args:[ Nolabel, v () ]
        ~prim_names
    in
    let mk_scope (Syn.Valence.Valence (slots, body_sort)) =
      let args =
        List.map slots ~f:(function
            | Syn.Sort_slot.Sort_binding _ -> [%expr Pattern.Var (x0, [%e v ()])]
            | Sort_pattern _ -> v ())
        |> Syntax_quoter.Exp.list ~loc
      in
      [%expr Nominal.Scope.Scope ([%e args], [%e body_arg body_sort])]
    in
    let children = List.map arity ~f:mk_scope |> Syntax_quoter.Exp.list ~loc in
    let body = mk_exp_tuple' [ info; estring op_name; children ] in
    pexp_construct { txt = operator; loc } body
  ;;

  let mk ~prim_names sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~ctor_type:With_info ~match_info:true op_def in
      let rhs = mk_nominal_exp ~var_names ~prim_names sort_defs op_def in
      case ~lhs ~guard ~rhs
    in
    let init = op_defs |> List.map ~f |> pexp_function in
    let expr = List.fold_right vars ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Of_nominal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let operator = unflatten [ "Nominal"; "Term"; "Operator" ]

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
            | Syn.Sort_slot.Sort_binding _ -> [%pat? Pattern.Var (_, [%p v ()])]
            | Sort_pattern _ -> v ())
        |> Syntax_quoter.Pat.list ~loc
      in
      [%pat? Nominal.Scope.Scope ([%p args], [%p v ()])]
    in
    let children = List.map arity ~f:mk_scope |> Syntax_quoter.Pat.list ~loc in
    let body = mk_pat_tuple' [ info; pstring op_name; children ] in
    ppat_construct { txt = operator; loc } body
  ;;

  let mk_exp
      ~var_names
      ~prim_names
      sort_defs
      (Syn.Operator_def.Operator_def (op_name, arity))
    =
    let v = var_allocator "x" in
    let ev = v >> snd in
    let info = evar "x0" in
    (* Queue of (variable, conversion to perform) *)
    let conversions_needed = Queue.create () in
    let body_arg sort ev =
      mk_sort_app
        ~var_names
        ~fun_name:"of_nominal"
        ~sort_defs
        ~sort
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
                      | Syn.Sort_slot.Sort_binding _sort -> ev ()
                      | Sort_pattern _ -> ev ()
                      (* TODO: pattern_converter? *))
             in
             let pv, ev = v () in
             let conversion = body_arg body_sort ev in
             Queue.enqueue conversions_needed (pv, conversion);
             slots_args @ [ ev ])
      |> List.map ~f:mk_exp_tuple
    in
    let contents = info :: contents in
    let txt = unflatten [ "Types"; op_name ] in
    let type_constr = pexp_construct { txt; loc } (mk_exp_tuple' contents) in
    conversions_needed, [%expr Ok [%e type_constr]]
  ;;

  let mk ~prim_names sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = mk_nominal_pat op_def in
      let rhs =
        let conversions_needed, init = mk_exp ~var_names ~prim_names sort_defs op_def in
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
    let fallthrough =
      case ~lhs:(ppat_var { txt = "tm"; loc }) ~guard ~rhs:[%expr Error tm]
    in
    let init = Util.List.snoc matching_cases fallthrough |> pexp_function in
    let expr = List.fold_right vars ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Equal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  let mk ~prim_names sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f (Syn.Operator_def.Operator_def (_op_name, arity) as op_def) =
      let lhs =
        let p1, p2 =
          ("x", "y")
          |> Lvca_util.Tuple2.map ~f:(fun name_base ->
                 Operator_pat.mk ~ctor_type:With_info ~match_info:true ~name_base op_def)
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
                          | Sort_pattern _ -> [%expr Pattern.equal ~info_eq [%e x] [%e y]])
                 in
                 let body_check =
                   Int.incr var_ix;
                   let x, y = mk_xy () in
                   let extra_args = [ labelled_arg "info_eq"; Nolabel, x; Nolabel, y ] in
                   mk_sort_app
                     ~var_names
                     ~fun_name:"equal"
                     ~sort_defs
                     ~sort:body_sort
                     ~extra_args
                     ~prim_names
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
    let expr = List.fold_right vars ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module Info (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  let mk ~prim_names:_ _sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let mk_case op_def =
      let lhs =
        Operator_pat.mk ~ctor_type:With_info ~match_info:true ~match_non_info:false op_def
      in
      case ~lhs ~guard ~rhs:[%expr x0]
    in
    let expr =
      let init = op_defs |> List.map ~f:mk_case |> pexp_function in
      List.fold_right vars ~init ~f:(f_fun ~used:false)
    in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

(* The wrapper module holds TODO *)
module Wrapper_module (Context : Builder_context) = struct
  open Context
  open Ast
  module Graph = Directed_graph.Make (Base.String)
  module Type_decls = Type_decls (Context)
  module Info = Info (Context)
  module To_plain = To_plain (Context)
  module Of_plain = Of_plain (Context)
  module Equal = Equal (Context)
  module Map_info = Map_info (Context)
  module To_nominal = To_nominal (Context)
  module Of_nominal = Of_nominal (Context)

  let mk ~prim_names sort_defs =
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let sort_ref_map = get_sort_ref_map sort_defs in
    let Graph.Connected_components.{ scc_graph; sccs } =
      Graph.connected_components sort_ref_map
    in
    let ordered_sccs =
      scc_graph
      |> Directed_graph.Int.topsort_exn
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
          prim_names:SSet.t
          -> _ Syn.Sort_def.t SMap.t
          -> string
          -> _ Syn.Sort_def.t
          -> Ppxlib.value_binding)
      =
      ordered_sccs
      |> List.map ~f:(fun scc ->
             let value_bindings =
               List.map scc ~f:(fun (sort_name, sort_def) ->
                   maker ~prim_names sort_def_map sort_name sort_def)
             in
             let is_rec =
               match definite_rec with
               | Some is_rec -> is_rec
               | None ->
                 (* Recursive if scc is more than one component or if self-referential. *)
                 (match scc with
                 | [ (sort_name, _) ] ->
                   (match Map.find sort_ref_map sort_name with
                   | Some sort_names
                     when List.mem sort_names sort_name ~equal:String.( = ) ->
                     Recursive
                   | _ -> Nonrecursive)
                 | _ -> Recursive)
             in
             pstr_value is_rec value_bindings)
    in
    let info_decls = Type_decls.mk ~info:true ~sort_def_map ~prim_names in
    let plain_decls = Type_decls.mk ~info:false ~sort_def_map ~prim_names in
    let defs =
      (* Each of these functions is potentially recursive (across multiple types), pre-declare. *)
      [%str
        module Types = struct
          [%%i pstr_type Recursive info_decls]
        end

        module Plain = struct
          [%%i pstr_type Recursive plain_decls]
        end

        module Info = [%m pmod_structure (adapt ~definite_rec:Nonrecursive Info.mk)]
        module To_plain = [%m pmod_structure (adapt To_plain.mk)]
        module Of_plain = [%m pmod_structure (adapt Of_plain.mk)]
        module Equal = [%m pmod_structure (adapt Equal.mk)]
        module Map_info = [%m pmod_structure (adapt Map_info.mk)]
        module To_nominal = [%m pmod_structure (adapt To_nominal.mk)]
        module Of_nominal = [%m pmod_structure (adapt Of_nominal.mk)]]
    in
    let wrapper_module =
      module_binding
        ~name:{ txt = Some "Wrapper" (* (module_name sort_name) *); loc }
        ~expr:(pmod_structure defs)
      |> pstr_module
    in
    wrapper_module, psig_type Recursive info_decls, psig_type Recursive plain_decls
  ;;
end

module Individual_type_module (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  let mk ~prim_names:_ ~sort_def_map:_ sort_name (Syn.Sort_def.Sort_def (vars, _op_defs)) =
    let _var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let plain_type_decl =
      let manifest_arg_list =
        List.map vars ~f:(fun (name, _) ->
            ptyp_constr { txt = unflatten [ module_name name; "Plain"; "t" ]; loc } [])
      in
      let manifest =
        Some
          (ptyp_constr
             { txt = unflatten [ "Wrapper"; "Plain"; sort_name ]; loc }
             manifest_arg_list)
      in
      pstr_type
        Recursive
        [ type_declaration
            ~name:{ txt = "t"; loc }
            ~params:[]
            ~cstrs:[]
            ~kind:Ptype_abstract
            ~private_:Public
            ~manifest
        ]
    in
    let info_type_decl =
      let manifest_arg_list =
        List.map vars ~f:(fun (name, _) ->
            ptyp_constr
              { txt = unflatten [ module_name name; "t" ]; loc }
              [ ptyp_var "info" ])
      in
      let manifest =
        Some
          (ptyp_constr
             { txt = unflatten [ "Wrapper"; "Types"; sort_name ]; loc }
             (ptyp_var "info" :: manifest_arg_list))
      in
      pstr_type
        Recursive
        [ type_declaration
            ~name:{ txt = "t"; loc }
            ~params:[ ptyp_var "info", (NoVariance, NoInjectivity) ]
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
      ; "equal", "Equal"
      ; "map_info", "Map_info"
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
    (* TODO: implement *)
    let fun_defs =
      List.append
        fun_defs
        [ [%stri
            let pp_generic ~open_loc:_ ~close_loc:_ ppf _tm =
              Fmt.pf ppf "TODO: pp_generic"
            ;;]
        ; [%stri
            module Parse = struct
              let t = Lvca_parsing.fail "TODO: parse"
            end]
        ; [%stri let jsonify _tm = failwith "TODO: jsonify"]
        ; [%stri let unjsonify _json = failwith "TODO: unjsonify"]
        ]
    in
    let init =
      pmod_structure
        (info_type_decl
         :: [%stri module Plain = [%m pmod_structure [ plain_type_decl ]]] :: fun_defs)
    in
    let f (name, kind_opt) accum =
      match kind_opt with
      | None ->
        (* XXX should do kind inference instead of assuming it's * *)
        let mod_param = Named ({ txt = Some (module_name name); loc }, all_term_s) in
        pmod_functor mod_param accum
      | Some (Syn.Kind.Kind (info, 1)) ->
        let mod_param =
          Named ({ txt = Some (module_name name); loc = update_loc info }, all_term_s)
        in
        pmod_functor mod_param accum
      | Some kind -> raise_kind_err name kind
    in
    let expr = List.fold_right vars ~init ~f in
    module_binding ~name:{ txt = Some (module_name sort_name); loc } ~expr |> pstr_module
  ;;
end

(* The top-level container / result (which is really a functor if there are externals). *)
module Container_module (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Wrapper_module = Wrapper_module (Context)
  module Individual_type_module = Individual_type_module (Context)

  let mk Syn.{ externals; sort_defs } =
    let prim_names = externals |> List.map ~f:fst |> SSet.of_list in
    (* pre-declare types *)
    let wrapper_module, info_types_sig, plain_types_sig =
      Wrapper_module.mk ~prim_names sort_defs
    in
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let type_modules =
      List.map
        sort_defs
        ~f:(Util.Tuple2.uncurry (Individual_type_module.mk ~prim_names ~sort_def_map))
    in
    (* TODO: include language?
  let sort_defs =
    [%str let language = [%e Syntax_quoter.mk_language ~loc lang]] @ sort_defs
  in
  *)
    (* Turn into a functor over externals *)
    let mod_param (name, kind) =
      match kind with
      | Some (Syn.Kind.Kind (info, 1)) ->
        Named ({ txt = Some (module_name name); loc = update_loc info }, all_term_s)
      | None -> Named ({ txt = Some (module_name name); loc }, all_term_s)
      | Some kind -> raise_kind_err name kind
    in
    let mod_param' (name, kind) = mod_param (name, Some kind) in
    let expr =
      let init =
        pmod_structure
          ([ wrapper_module
           ; [%stri module Types = Wrapper.Types]
           ; [%stri module Plain = Wrapper.Plain]
           ]
          @ type_modules)
      in
      List.fold_right externals ~init ~f:(mod_param' >> pmod_functor)
    in
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
        |> List.map ~f:(fun (sort_name, Syn.Sort_def.Sort_def (vars, _op_defs)) ->
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
               let type_ = List.fold_right vars ~init ~f:(mod_param >> pmty_functor) in
               psig_module (module_declaration ~name ~type_))
      in
      let init = pmty_signature (types_sig :: plain_sig :: sort_module_sigs) in
      List.fold_right externals ~init ~f:(mod_param' >> pmty_functor)
    in
    pmod_constraint expr mod_ty
  ;;
end

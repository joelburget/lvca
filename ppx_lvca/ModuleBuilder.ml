open Base
open Lvca_syntax
open Lvca_provenance
open Ppxlib
module Util = Lvca_util
module SSet = Util.String.Set
module SMap = Util.String.Map
module Syn = AbstractSyntax
module ParseAbstract = Syn.Parse (ParseUtil.CComment)

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
      module Pair (A : LanguageObject_intf.S) (B : LanguageObject_intf.S) = struct
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

      module OfPlain = struct
        let pair f_a f_b = function
          | Plain.Pair (x1, x2) -> Types.Pair ((), f_a x1, f_b x2)
        ;;

        let uses_pair = function
          | Plain.UsesPair x1 ->
            Types.UsesPair ((), pair Integer.of_plain String.of_plain x1)
        ;;

        let diag f_a = function Plain.Diag x1 -> Types.Diag ((), pair f_a f_a x1)
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
   *)
  let go : OptRange.t -> Location.t =
   fun optrange ->
    match optrange with
    | None -> loc
    | Some { start; finish } ->
      let before_buf = String.subo ~len:start buf in
      let internal_buf = String.sub ~pos:start ~len:(finish - start) buf in
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
          ; pos_cnum = start
          }
      ; loc_end =
          { loc.loc_end with
            pos_lnum = loc.loc_start.pos_lnum + buf_start_lnum + internal_lines
          ; pos_bol = finish_bol
          ; pos_cnum = finish
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

      let loc =
        Location.
          { loc_start = mk_pos 1 0 0
          ; loc_end = mk_pos 2 12 (String.length buf - 1)
          ; loc_ghost = false
          }
      ;;

      module Ast = Ast_builder.Make (struct
        let loc = loc
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
      File "test", line 1, characters 0-14:
      File "test", line 1, characters 0-13:
      File "test", line 2, characters 1-9:
      File "test", line 3, characters 1-3: |}]
    ;;

    module Context2 = struct
      let buf = "quick brown fox"
      (*         ^     ^       ^
                 012345678901234
                 1     2       3 *)

      let loc =
        Location.
          { loc_start = mk_pos 0 0 0
          ; loc_end = mk_pos 2 12 (String.length buf - 1)
          ; loc_ghost = false
          }
      ;;

      module Ast = Ast_builder.Make (struct
        let loc = loc
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
      File "test", line 0, characters 0-14:
      File "test", line 0, characters 0-13:
      File "test", line 0, characters 6-14:
      File "test", line 0, characters 12-14: |}]
    ;;
  end)
;;

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
  |> List.map ~f:(fun (sort_name, Syn.SortDef.SortDef (vars, op_defs)) ->
         let vars = List.map vars ~f:fst |> SSet.of_list in
         let connections =
           op_defs
           |> List.map ~f:(fun (Syn.OperatorDef.OperatorDef (_name, arity)) ->
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

  (** Context for classifying a sort ([classify_sort]) as [defn_status]. *)
  type context =
    { info : bool
    ; var_names : SSet.t
    ; mutual_sorts : OptRange.t Syn.SortDef.t SMap.t
    ; prim_names : SSet.t
    }

  (** When using a sort, we treat it differently depending on if it's a variable, defined
      mutually with the current sort (as defined by [mutual_sorts]), or predefined (either
      as an imported primitive or a sort defined earlier in this language). *)
  type defn_status =
    | Variable
    | MutualSort
    | PredefinedSort of
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
      | Some (Syn.SortDef.SortDef (vars, _op_defs)) ->
        assert (List.(Int.(length vars = length sort_args)));
        MutualSort
      | None -> PredefinedSort { prim = Set.mem prim_names sort_name; info })
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
      let name, sort_args = Sort.split sort in
      match classify_sort context sort with
      | Variable -> ptyp_var name
      | MutualSort ->
        let extra_args = List.map sort_args ~f:go in
        ptyp_constr { txt = unflatten [ name ]; loc } (args @ extra_args)
      | PredefinedSort { prim; info } ->
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
      | MutualSort -> evar sort_name (* XXX args? *)
      | PredefinedSort { prim; info } ->
        let qualified_name =
          match prim, info with
          | true, true -> [ module_name sort_name; fun_name ]
          | true, false -> [ module_name sort_name; "Plain"; fun_name ]
          | false, true -> [ "Wrapper"; "Types"; sort_name ]
          | false, false -> [ "Wrapper"; "Plain"; sort_name ]
        in
        pexp_ident { txt = unflatten qualified_name; loc }
    in
    let txt, var_args =
      match classify_sort context sort with
      | Variable -> Lident ("f_" ^ sort_name), []
      | MutualSort ->
        let sort_args = List.map sort_args ~f:(fun sort -> Nolabel, mk_var_arg sort) in
        Lident sort_name, sort_args
      | PredefinedSort _ -> unflatten [ module_name sort_name; fun_name ], []
    in
    pexp_apply (pexp_ident { txt; loc }) (var_args @ extra_args)
  ;;

  let labelled_fun name = pexp_fun (Labelled name) None (ppat_var { txt = name; loc })
  let labelled_arg name = Labelled name, pexp_ident { txt = Lident name; loc }

  (* Make a function taking an f_* argument *)
  let f_fun ?(used = true) (var_name, _kind_opt) =
    let txt = (if used then "f_" else "_f_") ^ var_name in
    pexp_fun Nolabel None (ppat_var { txt; loc })
  ;;

  let all_term_s = pmty_ident { txt = unflatten [ "LanguageObject"; "AllTermS" ]; loc }
end

(** Helper for declaring a constructor. *)
module CtorDecl (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Update_loc = Update_loc (Context)

  let mk
      ~info
      ~var_names
      ~mutual_sorts
      ~prim_names
      (Syn.OperatorDef.OperatorDef (op_name, arity))
    =
    let pattern_type =
      if info then [%type: 'info Pattern.t] else [%type: Pattern.Plain.t]
    in
    let args_of_valence (Syn.Valence.Valence (binding_sort_slots, body_sort)) =
      let args =
        List.map binding_sort_slots ~f:(function
            | Syn.SortSlot.SortBinding sort ->
              let loc = Update_loc.go (Sort.info sort) in
              [%type: string]
            | SortPattern _sort -> pattern_type)
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

module TypeDecls (Context : Builder_context) = struct
  open Context
  open Ast
  module CtorDecl = CtorDecl (Context)

  let mk ~info ~sort_def_map ~prim_names =
    let params0 = if info then [ ptyp_var "info", (NoVariance, NoInjectivity) ] else [] in
    sort_def_map
    |> Map.to_alist
    |> List.map ~f:(fun (sort_name, Syn.SortDef.SortDef (vars, op_defs)) ->
           let var_names = vars |> List.map ~f:fst |> SSet.of_list in
           let mk_var name = ptyp_var name, (NoVariance, NoInjectivity) in
           let params = vars |> List.map ~f:fst |> List.map ~f:mk_var in
           let params = params0 @ params in
           let kind =
             Ptype_variant
               (List.map
                  op_defs
                  ~f:(CtorDecl.mk ~info ~var_names ~mutual_sorts:sort_def_map ~prim_names))
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

module OperatorPat (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  type ctor_type =
    | Plain
    | WithInfo

  let is_valid_ocaml_constr_name str =
    (not (String.is_empty str)) && Char.is_uppercase str.[0]
  ;;

  let mk_pat_tuple = function
    | [] -> [%pat? ()]
    | [ elem ] -> elem
    | elems -> ppat_tuple elems
  ;;

  let mk
      ~ctor_type
      ?(match_info = false)
      ?(match_non_info = true)
      ?(name_base = "x")
      (Syn.OperatorDef.OperatorDef (op_name, arity))
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
      | WithInfo -> [ (if match_info then v 0 else ppat_any) ] :: contents
      | Plain -> contents
    in
    let body =
      match contents with
      | [] -> None
      | _ -> Some (contents |> List.map ~f:mk_pat_tuple |> mk_pat_tuple)
    in
    let txt =
      let container_name =
        match ctor_type with WithInfo -> "Types" | Plain -> "Plain"
      in
      unflatten [ container_name; op_name ]
    in
    ppat_construct { txt; loc } body
  ;;
end

module OperatorExp (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  type mapping_rhs_ty =
    | Plain
    | WithInfo of expression

  let mk_exp_tuple = function
    | [] -> [%expr ()]
    | [ elem ] -> elem
    | elems -> pexp_tuple elems
  ;;

  let mk
      ~ctor_type (* Building a plain or with-info data type *)
      ~var_names
      ~prim_names
      ?(extra_args = [])
      ?(name_base = "x")
      sort_defs (* Sorts being defined together *)
      fun_name (* The name of the function being defined *)
      (Syn.OperatorDef.OperatorDef (op_name, arity))
    =
    let var_ix = ref 0 in
    let v () = evar (Printf.sprintf "%s%d" name_base !var_ix) in
    let pattern_converter = pexp_ident { txt = unflatten [ "Pattern"; fun_name ]; loc } in
    let body_arg sort =
      Int.incr var_ix;
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
                      Int.incr var_ix;
                      match slot with
                      | Syn.SortSlot.SortBinding _sort -> v ()
                      | SortPattern _ ->
                        pexp_apply pattern_converter (extra_args @ [ Nolabel, v () ]))
             in
             slots_args @ [ body_arg body_sort ])
      |> List.map ~f:mk_exp_tuple
    in
    let contents =
      match ctor_type with WithInfo expr -> expr :: contents | Plain -> contents
    in
    let body = match contents with [] -> None | _ -> Some (mk_exp_tuple contents) in
    let txt =
      let container_name =
        match ctor_type with WithInfo _ -> "Types" | Plain -> "Plain"
      in
      unflatten [ container_name; op_name ]
    in
    pexp_construct { txt; loc } body
  ;;
end

module ToPlain (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module OperatorPat = OperatorPat (Context)
  module OperatorExp = OperatorExp (Context)

  let mk ~prim_names sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = OperatorPat.mk ~ctor_type:WithInfo op_def in
      let rhs =
        OperatorExp.mk ~var_names ~prim_names ~ctor_type:Plain sort_defs "to_plain" op_def
      in
      case ~lhs ~guard ~rhs
    in
    let init = op_defs |> List.map ~f |> pexp_function in
    let expr = List.fold_right vars ~init ~f:f_fun in
    value_binding ~pat:(ppat_var { txt = sort_name; loc }) ~expr
  ;;
end

module OfPlain (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module OperatorPat = OperatorPat (Context)
  module OperatorExp = OperatorExp (Context)

  let mk ~prim_names sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = OperatorPat.mk ~ctor_type:Plain op_def in
      let rhs =
        OperatorExp.mk
          ~var_names
          ~prim_names
          ~ctor_type:(WithInfo [%expr ()])
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

module MapInfo (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module OperatorPat = OperatorPat (Context)
  module OperatorExp = OperatorExp (Context)

  let mk ~prim_names sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = OperatorPat.mk ~ctor_type:WithInfo ~match_info:true op_def in
      let rhs =
        OperatorExp.mk
          ~var_names
          ~prim_names
          ~ctor_type:(WithInfo [%expr f x0])
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

module Equal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module OperatorPat = OperatorPat (Context)

  let mk ~prim_names sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
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
                          | Syn.SortSlot.SortBinding _sort ->
                            [%expr Base.String.( = ) [%e x] [%e y]]
                          | SortPattern _ -> [%expr Pattern.equal ~info_eq [%e x] [%e y]])
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
  module OperatorPat = OperatorPat (Context)

  let mk ~prim_names:_ _sort_defs sort_name (Syn.SortDef.SortDef (vars, op_defs)) =
    let mk_case op_def =
      let lhs =
        OperatorPat.mk ~ctor_type:WithInfo ~match_info:true ~match_non_info:false op_def
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
module WrapperModule (Context : Builder_context) = struct
  open Context
  open Ast
  module Graph = DirectedGraph.Make (Base.String)
  module TypeDecls = TypeDecls (Context)
  module Info = Info (Context)
  module ToPlain = ToPlain (Context)
  module OfPlain = OfPlain (Context)
  module Equal = Equal (Context)
  module MapInfo = MapInfo (Context)

  let mk ~prim_names sort_defs =
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
          prim_names:SSet.t
          -> _ Syn.SortDef.t SMap.t
          -> string
          -> _ Syn.SortDef.t
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
    let info_decls = TypeDecls.mk ~info:true ~sort_def_map ~prim_names in
    let plain_decls = TypeDecls.mk ~info:false ~sort_def_map ~prim_names in
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
        module ToPlain = [%m pmod_structure (adapt ToPlain.mk)]
        module OfPlain = [%m pmod_structure (adapt OfPlain.mk)]
        module Equal = [%m pmod_structure (adapt Equal.mk)]
        module MapInfo = [%m pmod_structure (adapt MapInfo.mk)]

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
      module_binding
        ~name:{ txt = Some "Wrapper" (* (module_name sort_name) *); loc }
        ~expr:(pmod_structure defs)
      |> pstr_module
    in
    wrapper_module, psig_type Recursive info_decls, psig_type Recursive plain_decls
  ;;
end

module IndividualTypeModule (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Update_loc = Update_loc (Context)

  let mk ~prim_names:_ ~sort_def_map:_ sort_name (Syn.SortDef.SortDef (vars, _op_defs)) =
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
      ; "to_plain", "ToPlain"
      ; "of_plain", "OfPlain"
      ; "equal", "Equal"
      ; "map_info", "MapInfo"
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
            module Parse (Comment : ParseUtil.Comment_int) = struct
              let t = failwith "TODO"
            end]
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
          Named ({ txt = Some (module_name name); loc = Update_loc.go info }, all_term_s)
        in
        pmod_functor mod_param accum
      | Some (Syn.Kind.Kind (info, _) as kind) ->
        Location.raise_errorf
          ~loc:(Update_loc.go info)
          "Code generation currently only supports external modules of kind * (`%s` is \
           %s)"
          name
          (Fmt.to_to_string Syn.Kind.pp kind)
    in
    let expr = List.fold_right vars ~init ~f in
    module_binding ~name:{ txt = Some (module_name sort_name); loc } ~expr |> pstr_module
  ;;
end

(* The top-level container / result (which is really a functor if there are externals). *)
module ContainerModule (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module WrapperModule = WrapperModule (Context)
  module IndividualTypeModule = IndividualTypeModule (Context)
  module Update_loc = Update_loc (Context)

  let mk Syn.{ externals; sort_defs } =
    let prim_names = externals |> List.map ~f:fst |> SSet.of_list in
    (* pre-declare types *)
    let wrapper_module, info_types_sig, plain_types_sig =
      WrapperModule.mk ~prim_names sort_defs
    in
    let sort_def_map = SMap.of_alist_exn sort_defs in
    let type_modules =
      List.map
        sort_defs
        ~f:(Util.Tuple2.uncurry (IndividualTypeModule.mk ~prim_names ~sort_def_map))
    in
    (* TODO: include language?
  let sort_defs =
    [%str let language = [%e SyntaxQuoter.mk_language ~loc lang]] @ sort_defs
  in
  *)
    (* Turn into a functor over externals *)
    let mod_param (name, kind) =
      match kind with
      | Some (Syn.Kind.Kind (info, 1)) ->
        Named ({ txt = Some (module_name name); loc = Update_loc.go info }, all_term_s)
      | None -> Named ({ txt = Some (module_name name); loc }, all_term_s)
      | Some (Syn.Kind.Kind (info, _) as kind) ->
        Location.raise_errorf
          ~loc:(Update_loc.go info)
          "Code generation currently only supports external modules of kind * (`%s` is \
           %s)"
          name
          (Fmt.to_to_string Syn.Kind.pp kind)
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
               let type_ = List.fold_right vars ~init ~f:(mod_param >> pmty_functor) in
               psig_module (module_declaration ~name ~type_))
      in
      let init = pmty_signature (types_sig :: plain_sig :: sort_module_sigs) in
      List.fold_right externals ~init ~f:(mod_param' >> pmty_functor)
    in
    pmod_constraint expr mod_ty
  ;;
end

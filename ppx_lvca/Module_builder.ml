open Base
open Lvca_syntax
open Lvca_provenance
open Ppxlib
module Util = Lvca_util
module SSet = Util.String.Set
module SMap = Util.String.Map
module Syn = Abstract_syntax
module Graph = Directed_graph.Make (Base.String)
module Tuple2 = Util.Tuple2

let ( >> ), ( << ) = Util.(( >> ), ( << ))

(** {2 Sort variables}

    Example definition: [pair a b := Pair(a; b)]. We translate this to OCaml:

    We end up with something like [type ('a, 'b) pair = Pair of 'a * 'b] (or
    [type ('info, 'a, 'b) pair = Pair of 'info * 'a * 'b]). This is great but we can't
    easily define helpers:

    {[ let of_plain = function Plain.Pair (x1, x2) -> Types.Pair ((), ???, ???) ]} *)

module Supported_function = struct
  type t =
    | Fun_of_nominal
    | Fun_to_nominal
    | Fun_map_info
    | Fun_to_plain
    | Fun_of_plain
    | Fun_equal
    | Fun_info

  let fun_name = function
    | Fun_of_nominal -> "of_nominal"
    | Fun_to_nominal -> "to_nominal"
    | Fun_map_info -> "map_info"
    | Fun_to_plain -> "to_plain"
    | Fun_of_plain -> "of_plain"
    | Fun_equal -> "equal"
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
  let prim_list = prims |> Map.filter ~f:Option.is_some |> Map.keys in
  let known_sorts_1 = sort_defs |> List.map ~f:fst |> SSet.of_list in
  let known_sorts_2 = SSet.of_list prim_list in
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

let alphabet =
  "abcdefghijklmnopqrstuvwxyz" |> String.to_list |> List.map ~f:String.of_char
;;

(** The sequence "a"; "b"; ...; "z"; "aa"; ... "az"; ... "zz"; "aaa"; ... "aaaa"; ... *)
let name_sequence =
  let letter_seq = Sequence.of_list alphabet in
  let init = Sequence.of_list [ "" ] in
  Sequence.unfold ~init ~f:(fun (prev : string Sequence.t) ->
      let next =
        Sequence.cartesian_product prev letter_seq
        |> Sequence.map ~f:(fun (x, y) -> x ^ y)
      in
      Some (next, next))
  |> Sequence.concat
;;

let empty_name_sequence = Sequence.append (Sequence.singleton "") name_sequence

let%test_module "name_sequence" =
  (module struct
    let%expect_test _ =
      Stdio.print_string (Sequence.nth_exn name_sequence 0);
      [%expect "a"]
    ;;

    let%expect_test _ =
      Stdio.print_string (Sequence.nth_exn name_sequence 25);
      [%expect "z"]
    ;;

    let%expect_test _ =
      Stdio.print_string (Sequence.nth_exn name_sequence 26);
      [%expect "aa"]
    ;;

    let%expect_test _ =
      Stdio.print_string (Sequence.nth_exn name_sequence 51);
      [%expect "az"]
    ;;

    let%expect_test _ =
      Stdio.print_string (Sequence.nth_exn name_sequence 52);
      [%expect "ba"]
    ;;

    let%expect_test _ =
      Stdio.print_string (Sequence.nth_exn name_sequence 77);
      [%expect "bz"]
    ;;
  end)
;;

let generate_unique_name ?(base = "") taken =
  if String.(base = "")
  then Sequence.find_exn name_sequence ~f:(fun name -> not (Set.mem taken name))
  else (
    let sequence = empty_name_sequence |> Sequence.map ~f:(fun name -> base ^ name) in
    Sequence.find_exn sequence ~f:(fun name -> not (Set.mem taken name)))
;;

let generate_unique_names ?(base = "") taken =
  if String.(base = "")
  then Sequence.filter name_sequence ~f:(fun name -> not (Set.mem taken name))
  else (
    let sequence = empty_name_sequence |> Sequence.map ~f:(fun name -> base ^ name) in
    Sequence.filter sequence ~f:(fun name -> not (Set.mem taken name)))
;;

let%test_module "generate_unique_name" =
  (module struct
    let%expect_test _ =
      let taken = SSet.empty in
      Fmt.pr "%s\n" (generate_unique_name taken);
      Fmt.pr "%s\n" (generate_unique_name ~base:"b" taken);
      Sequence.take (generate_unique_names ~base:"b" taken) 5
      |> Sequence.iter ~f:(Fmt.pr "%s\n");
      [%expect {|
      a
      b
      b
      ba
      bb
      bc
      bd |}]
    ;;

    let%expect_test _ =
      let taken = SSet.of_list [ "a"; "b" ] in
      Fmt.pr "%s\n" (generate_unique_name taken);
      Fmt.pr "%s\n" (generate_unique_name ~base:"a" taken);
      Sequence.take (generate_unique_names taken) 5 |> Sequence.iter ~f:(Fmt.pr "%s\n");
      [%expect {|
      c
      aa
      c
      d
      e
      f
      g |}]
    ;;

    let%expect_test _ =
      let taken = SSet.of_list alphabet in
      Fmt.pr "%s\n" (generate_unique_name taken);
      Fmt.pr "%s\n" (generate_unique_name ~base:"c" taken);
      Sequence.take (generate_unique_names taken) 5 |> Sequence.iter ~f:(Fmt.pr "%s\n");
      [%expect {|
      aa
      ca
      aa
      ab
      ac
      ad
      ae |}]
    ;;

    let%expect_test _ =
      let taken = SSet.empty in
      let base = "the quick brown fox jumps over the lazy dog" in
      Fmt.pr "%s\n" (generate_unique_name ~base taken);
      Sequence.take (generate_unique_names ~base taken) 5
      |> Sequence.iter ~f:(Fmt.pr "%s\n");
      [%expect
        {|
        the quick brown fox jumps over the lazy dog
        the quick brown fox jumps over the lazy dog
        the quick brown fox jumps over the lazy doga
        the quick brown fox jumps over the lazy dogb
        the quick brown fox jumps over the lazy dogc
        the quick brown fox jumps over the lazy dogd |}]
    ;;
  end)
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
    ; mutual_sorts : string Commented.t Syn.Sort_def.t SMap.t
    ; prims : string list option SMap.t
    }

  (** When using a sort, we treat it differently depending on if it's a variable, defined
      mutually with the current sort (as defined by [mutual_sorts]), or predefined (either
      as an imported primitive or a sort defined earlier in this language). *)
  type defn_status =
    | Variable
    | Mutual_sort
    | External_sort of string list option
    | Predefined_sort of has_info

  let classify_sort context sort =
    let { info; var_names; mutual_sorts; prims } = context in
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
        | Some mods_opt -> External_sort mods_opt
        | None -> Predefined_sort info))
  ;;

  let nominal_term = [ "Lvca_syntax"; "Nominal"; "Term" ]
  let nominal_operator = [ "Lvca_syntax"; "Nominal"; "Term"; "Operator" ]
  let nominal_operator' = unflatten nominal_operator
  let nominal_plain = [ "Lvca_syntax"; "Nominal"; "Term"; "Plain" ]

  (** Make the type corresponding to a sort in a constructor declaration. Eg:

      [ nonempty := Nonempty (string; list string) ]

      ==>

      If [list] is defined in this module:

      {[
        'info nonempty = Nonempty of 'info * 'info Nominal.t * 'info list
                                             ^^^^^^^^^^^^^^^   ^^^^^^^^^^
      ]}

      If [list] is an external:

      {[
        'info nonempty =
           | Nonempty of 'info * 'info Nominal.t * 'info Nominal.t
                                 ^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^
      ]} *)
  let ptyp_of_sort ~type_decl_context ~context sort =
    let info_args = match context.info with With_info -> [ [%type: 'info] ] | _ -> [] in
    let rec go sort =
      let loc = sort |> Sort.info |> Commented.get_range |> update_loc in
      let name, sort_args = Sort.split sort in
      match classify_sort context sort with
      | Variable -> ptyp_var name
      | Mutual_sort ->
        let sort_args = List.map sort_args ~f:go in
        let names =
          match type_decl_context, context.info with
          | Predefinition, _ -> [ name ]
          | Individual_type_module, With_info -> [ "Wrapper"; "Types"; name ]
          | Individual_type_module, Plain -> [ "Wrapper"; "Plain"; name ]
        in
        ptyp_constr { txt = unflatten names; loc } (info_args @ sort_args)
      | Predefined_sort info ->
        let mod_name = match info with With_info -> "Types" | _ -> "Plain" in
        ptyp_constr { txt = unflatten [ "Wrapper"; mod_name; name ]; loc } info_args
      | External_sort mods_opt ->
        let module_path, args =
          match mods_opt with
          | Some mods -> mods, info_args @ List.map sort_args ~f:go
          | None -> nominal_term, info_args
        in
        let module_path =
          match context.info with
          | With_info -> module_path
          | _ -> module_path @ [ "Plain" ]
        in
        ptyp_constr { txt = unflatten (module_path @ [ "t" ]); loc } args
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
        [a ~args].
      - otherwise the sort is an external or defined in this language: We build an
        expression like [foo ~args] or [list (a ~args) ~args] . *)
  let mk_sort_app ~fun_defn ~classify_sort ~args sort =
    let rec go sort =
      let loc = sort |> Sort.info |> Commented.get_range |> update_loc in
      let txt = Lident (sort_head sort) in
      let apply sort_fun =
        match sort with
        | Sort.Ap (_, _, args) ->
          pexp_apply
            sort_fun
            (args |> Sort.Ap_list.to_list |> List.map ~f:(fun sort -> Nolabel, go sort))
        | Name _ -> sort_fun
      in
      match classify_sort sort with
      | External_sort None ->
        pexp_ident
          { txt = unflatten (nominal_term @ [ Supported_function.fun_name fun_defn ])
          ; loc
          }
      | External_sort (Some mods) ->
        apply
          (pexp_ident
             { txt = unflatten (mods @ [ Supported_function.fun_name fun_defn ]); loc })
      | Variable | Mutual_sort | Predefined_sort _ -> apply (pexp_ident { txt; loc })
    in
    pexp_apply (go sort) args
  ;;

  let labelled_fun name = pexp_fun (Labelled name) None (pvar name)
  let labelled_arg name = Labelled name, pexp_ident { txt = Lident name; loc }

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

  let parse_external_module =
    let open Lvca_parsing in
    whitespace
    *> Ws.string "module"
    *> sep_by1
         (No_ws.char '.')
         (No_ws.satisfy Char.is_uppercase
         >>= fun c0 ->
         many (Ws.satisfy Char.(fun c -> is_alphanum c || c = '_' || c = '\''))
         >>| fun cs -> String.of_char_list (c0 :: cs))
  ;;

  let prims_of_externals externals =
    externals
    |> List.map
         ~f:(fun (name, Abstract_syntax.Kind.Kind (Commented.{ comment; _ }, _kind)) ->
           let m_opt =
             Option.bind comment ~f:(fun comment ->
                 match Lvca_parsing.parse_string parse_external_module comment with
                 | Ok m -> Some m
                 | Error _ -> None)
           in
           name, m_opt)
    |> SMap.of_alist_exn
  ;;
end

(** Helper for declaring a constructor. *)
module Ctor_decl (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)

  let mk
      ~info
      ~type_decl_context
      ~var_names
      ~mutual_sorts
      ~prims
      (Syn.Operator_def.Operator_def (_info, op_name, Arity (_, arity)))
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
              let loc = sort |> Sort.info |> Commented.get_range |> update_loc in
              (match info with
              | With_info -> [%type: 'info Lvca_syntax.Single_var.t]
              | _ -> [%type: Lvca_syntax.Single_var.Plain.t])
            | Sort_pattern _sort -> pattern_type)
      in
      let context = { info; var_names; mutual_sorts; prims } in
      args @ [ ptyp_of_sort ~type_decl_context ~context body_sort ]
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

  let mk_op_ctors
      ~type_decl_context
      ~sort_def_map:mutual_sorts
      ~prims
      ~info
      ~sort_name
      ~partitioned_sorts
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let op_ctors =
      List.map
        op_defs
        ~f:(Ctor_decl.mk ~info ~type_decl_context ~var_names ~mutual_sorts ~prims)
    in
    let var_ctor =
      let args =
        match info with
        | With_info -> [ [%type: 'info]; [%type: string] ]
        | Plain -> [ [%type: string] ]
      in
      constructor_declaration
        ~name:{ txt = var_ctor_name sort_name; loc }
        ~args:(Pcstr_tuple args)
        ~res:None
    in
    (* If this type is ever bound, add a variable constructor. *)
    match Hashtbl.find partitioned_sorts sort_name with
    | Some Bound -> op_ctors @ [ var_ctor ]
    | _ -> op_ctors
  ;;

  let mk ~info ~sort_def_map ~sorted_scc_graph ~prims ~partitioned_sorts =
    let info_params = match info with With_info -> [ plain_typ_var "info" ] | _ -> [] in
    sorted_scc_graph
    |> List.map ~f:(fun (_scc_num, sort_name_set) ->
           sort_name_set
           |> Set.to_list
           |> List.map ~f:(fun sort_name ->
                  let sort_def = Map.find_exn sort_def_map sort_name in
                  let (Syn.Sort_def.Sort_def (vars, _op_defs)) = sort_def in
                  let params = vars |> List.map ~f:fst |> List.map ~f:plain_typ_var in
                  let params = info_params @ params in
                  let op_ctors =
                    mk_op_ctors
                      ~sort_def_map
                      ~type_decl_context:Predefinition
                      ~prims
                      ~info
                      ~sort_name
                      ~partitioned_sorts
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
      ~has_info
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
      ~prims
      ?(f = false)
      sort_defs (* Sorts being defined together *)
      fun_defn (* The name of the function being defined *)
      (Syn.Operator_def.Operator_def (_info, op_name, Arity (_, arity)))
    =
    let v = evar_allocator "x" in
    let pattern_converter =
      pexp_ident
        { txt =
            unflatten [ "Lvca_syntax"; "Pattern"; Supported_function.fun_name fun_defn ]
        ; loc
        }
    in
    let f_args = if f then [ labelled_arg "f" ] else [] in
    let classify_sort =
      classify_sort { info = With_info; var_names; mutual_sorts = sort_defs; prims }
    in
    let body_arg sort =
      mk_sort_app ~fun_defn ~classify_sort ~args:(f_args @ [ Nolabel, v () ]) sort
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
                         match fun_defn with
                         | Fun_of_plain -> [%expr ()]
                         | Fun_map_info -> [%expr f [%e v].info]
                         | _ -> failwith "Not supported by Operator_exp.mk (With_info)"
                       in
                       [%expr
                         Lvca_syntax.Single_var.{ info = [%e info]; name = [%e v].name }]
                     | Plain ->
                       [%expr Lvca_syntax.Single_var.Plain.{ name = [%e v ()].name }])
                   | Sort_pattern _ ->
                     pexp_apply pattern_converter (f_args @ [ Nolabel, v () ]))
             in
             slots_args @ [ body_arg body_sort ])
      |> List.map ~f:mk_exp_tuple
    in
    let contents =
      match has_info with
      | With_info ->
        let expr =
          match fun_defn with
          | Fun_of_plain -> [%expr ()]
          | Fun_map_info -> [%expr f x0]
          | _ -> failwith "Not supported by Operator_exp.mk (With_info)"
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
      ~prims
      ~partitioned_sorts
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let arg_names = List.map ~f:fst vars in
    let var_names = SSet.of_list arg_names in
    let f op_def =
      let lhs = Operator_pat.mk ~has_info:With_info op_def in
      let rhs =
        Operator_exp.mk ~var_names ~prims ~has_info:Plain sort_defs Fun_to_plain op_def
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
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

module Of_plain (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let mk
      ~prims
      ~partitioned_sorts
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
          ~prims
          ~has_info:With_info
          sort_defs
          Fun_of_plain
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
      List.fold_right (List.map ~f:fst vars) ~init:(pexp_function op_defs) ~f:f_fun
    in
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

module Map_info (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)
  module Operator_exp = Operator_exp (Context)

  let mk
      ~prims
      ~partitioned_sorts
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
          ~prims
          ~has_info:With_info
          ~f:true
          sort_defs
          Fun_map_info
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
    let expr = List.fold_right (List.map ~f:fst vars) ~init ~f:f_fun in
    value_binding ~pat:(pvar sort_name) ~expr
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
    let classify_sort =
      classify_sort { info = With_info; var_names; mutual_sorts = sort_defs; prims }
    in
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
      ~partitioned_sorts
      sort_defs
      sort_name
      (Syn.Sort_def.Sort_def (vars, op_defs))
    =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let f op_def =
      let lhs = Operator_pat.mk ~has_info:With_info ~match_info:true op_def in
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
      match Hashtbl.find partitioned_sorts sort_name with
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
    let classify_sort =
      classify_sort { info = With_info; var_names; mutual_sorts = sort_defs; prims }
    in
    let body_arg ev sort =
      match classify_sort sort with
      | External_sort None -> None
      | _ ->
        Some
          (mk_sort_app ~fun_defn:Fun_of_nominal ~classify_sort ~args:[ Nolabel, ev ] sort)
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
             (match body_arg ev body_sort with
             | None -> ()
             | Some conversion -> Queue.enqueue conversions_needed (pv, conversion));
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
      ~partitioned_sorts
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
      match Hashtbl.find partitioned_sorts sort_name with
      | Some Bound -> matching_cases @ [ var_case ]
      | _ -> matching_cases
    in
    let init = matching_cases @ [ fallthrough ] |> pexp_function in
    let expr = List.fold_right (List.map ~f:fst vars) ~init ~f:f_fun in
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

module Equal (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Operator_pat = Operator_pat (Context)

  let mk ~prims sort_defs sort_name (Syn.Sort_def.Sort_def (vars, op_defs)) =
    let var_names = vars |> List.map ~f:fst |> SSet.of_list in
    let classify_sort =
      classify_sort { info = With_info; var_names; mutual_sorts = sort_defs; prims }
    in
    let f (Syn.Operator_def.Operator_def (_info, _op_name, Arity (_, arity)) as op_def) =
      let lhs =
        let p1, p2 =
          ("x", "y")
          |> Tuple2.map ~f:(fun name_base ->
                 Operator_pat.mk ~has_info:With_info ~match_info:true ~name_base op_def)
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
                            [%expr Base.String.( = ) [%e x] [%e y]]
                          | Sort_pattern _ ->
                            [%expr Lvca_syntax.Pattern.equal ~info_eq [%e x] [%e y]])
                 in
                 let body_check =
                   Int.incr var_ix;
                   let x, y = mk_xy () in
                   let args = [ labelled_arg "info_eq"; Nolabel, x; Nolabel, y ] in
                   mk_sort_app ~fun_defn:Fun_equal ~classify_sort ~args body_sort
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
      |> pexp_fun Nolabel None (pvar "t2")
      |> pexp_fun Nolabel None (pvar "t1")
      |> labelled_fun "info_eq"
    in
    let expr = List.fold_right (List.map ~f:fst vars) ~init ~f:f_fun in
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
      ~partitioned_sorts
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
        (List.map ~f:fst vars)
        ~init:(pexp_function op_defs)
        ~f:(f_fun ~used:false)
    in
    value_binding ~pat:(pvar sort_name) ~expr
  ;;
end

(* The wrapper module holds helper modules:
  - First, type declarations
  - Then individual modules for different helper functions: [info], [to_plain], etc.
  *)
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

  let mk ~prims ~sort_def_map ~sort_dep_map ~sorted_scc_graph ~partitioned_sorts =
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
          prims:string list option SMap.t
          -> partitioned_sorts:(string, bound_unbound) Hashtbl.t
          -> _ Syn.Sort_def.t SMap.t
          -> string
          -> _ Syn.Sort_def.t
          -> Ppxlib.value_binding)
      =
      ordered_sccs
      |> List.map ~f:(fun (_scc_num, named_sort) ->
             let value_bindings =
               List.map named_sort ~f:(fun (sort_name, sort_def) ->
                   maker ~prims ~partitioned_sorts sort_def_map sort_name sort_def)
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
      Type_decls.mk ~sort_def_map ~sorted_scc_graph ~prims ~partitioned_sorts
    in
    let info_decls = type_decls ~info:With_info in
    let plain_decls = type_decls ~info:Plain in
    let defs =
      (* Each of these functions is potentially recursive (across multiple
         types), pre-declare. *)
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
    module_binding ~name:{ txt = Some "Wrapper"; loc } ~expr:(pmod_structure defs)
    |> pstr_module
  ;;

  let mk_sigs ~prims ~sort_def_map ~sorted_scc_graph ~partitioned_sorts =
    let type_decls =
      Type_decls.mk ~sort_def_map ~sorted_scc_graph ~prims ~partitioned_sorts
    in
    let info_decls = type_decls ~info:With_info in
    let plain_decls = type_decls ~info:Plain in
    [%sig:
      module Wrapper : sig
        module Types : sig
          [%%i psig_type Recursive info_decls]
        end

        module Plain : sig
          [%%i psig_type Recursive plain_decls]
        end
      end]
  ;;
end

module Individual_type_sig (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Type_decls = Type_decls (Context)

  let mk ~prims ~sort_def_map ~partitioned_sorts ~info sort_name sort_def =
    let (Syn.Sort_def.Sort_def (vars, _op_defs)) = sort_def in
    let params = vars |> List.map ~f:(fst >> plain_typ_var) in
    let op_ctors =
      Type_decls.mk_op_ctors
        ~type_decl_context:Individual_type_module
        ~sort_def_map
        ~prims
        ~info
        ~sort_name
        ~partitioned_sorts
        sort_def
    in
    let params =
      match info with With_info -> plain_typ_var "info" :: params | _ -> params
    in
    psig_type
      Recursive
      [ type_declaration
          ~name:{ txt = "t"; loc }
          ~params
          ~cstrs:[]
          ~kind:(Ptype_variant op_ctors)
          ~private_:Public
          ~manifest:None
      ]
  ;;
end

module Individual_type_module (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Type_decls = Type_decls (Context)

  let mk ~prims ~sort_def_map ~partitioned_sorts sort_name sort_def =
    let (Syn.Sort_def.Sort_def (vars, _op_defs)) = sort_def in
    let var_names = List.map vars ~f:fst in
    let type_vars = List.map var_names ~f:ptyp_var in
    let params = List.map var_names ~f:plain_typ_var in
    let plain_type_decl =
      let manifest =
        Some
          (ptyp_constr
             { txt = unflatten [ "Wrapper"; "Plain"; sort_name ]; loc }
             type_vars)
      in
      let op_ctors =
        Type_decls.mk_op_ctors
          ~type_decl_context:Individual_type_module
          ~sort_def_map
          ~prims
          ~info:Plain
          ~sort_name
          ~partitioned_sorts
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
    let info_type_decl =
      let manifest =
        Some
          (ptyp_constr
             { txt = unflatten [ "Wrapper"; "Types"; sort_name ]; loc }
             (ptyp_var "info" :: type_vars))
      in
      let op_ctors =
        Type_decls.mk_op_ctors
          ~type_decl_context:Individual_type_module
          ~sort_def_map
          ~prims
          ~info:With_info
          ~sort_name
          ~partitioned_sorts
          sort_def
      in
      pstr_type
        Recursive
        [ type_declaration
            ~name:{ txt = "t"; loc }
            ~params:(plain_typ_var "info" :: params)
            ~cstrs:[]
            ~kind:(Ptype_variant op_ctors)
            ~private_:Public
            ~manifest
        ]
    in
    let fun_defs =
      [ Supported_function.Fun_info
      ; Fun_to_plain
      ; Fun_of_plain
      ; Fun_map_info
      ; Fun_to_nominal
      ; Fun_of_nominal
      ]
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
    let taken = vars |> List.map ~f:fst |> SSet.of_list in
    let _taken, unique_var_names =
      List.fold_map vars ~init:taken ~f:(fun taken (name, _) ->
          (* let name' = generate_unique_name ~base:(name ^ "'") taken in *)
          let name' = generate_unique_name ~base:("_" ^ name) taken in
          let taken = Set.add taken name' in
          taken, name')
    in
    let plain_fun_def name arg_names rhs =
      let pat = pvar name in
      let expr =
        List.fold_right (unique_var_names @ arg_names) ~init:rhs ~f:(fun arg rhs ->
            pexp_fun Nolabel None (pvar arg) rhs)
      in
      pstr_value Nonrecursive [ value_binding ~pat ~expr ]
    in
    (*
    let mk_helper name =
      pexp_apply (evar name) (List.map unique_var_names ~f:(fun var -> Nolabel, evar var))
    in
    let of_plain = mk_helper "of_plain" in
    let to_plain = mk_helper "to_plain" in
    let to_nominal = mk_helper "to_nominal" in
    let of_nominal = mk_helper "of_nominal" in
    *)
    let plain_mod =
      let equal =
        plain_fun_def
          (if List.is_empty vars then "=" else "equal")
          [ "_x"; "_y" ]
          [%expr false (* TODO *)]
        (*
          [%expr
            let x = x |> [%e of_plain] |> [%e to_nominal] in
            let y = y |> [%e of_plain] |> [%e to_nominal] in
            Lvca_syntax.Nominal.Term.(equal ~info_eq:Base.Unit.( = ) (erase x) (erase y))]
            *)
      in
      let jsonify =
        plain_fun_def
          "jsonify"
          [ "_tm" ]
          (*
          [%expr
            tm |> [%e of_plain] |> [%e to_nominal] |> Lvca_syntax.Nominal.Term.jsonify]
          *)
          [%expr Lvca_util.Json.Int 1 (* TODO *)]
      in
      let unjsonify =
        plain_fun_def
          "unjsonify"
          [ "_json" ]
          (*
          [%expr
            json
            |> Lvca_syntax.Nominal.Term.unjsonify
            |> Base.Option.bind ~f:(fun tm ->
                   match [%e of_nominal] tm with
                   | Ok tm -> Some ([%e to_plain] tm)
                   | Error _ -> None)]
                   *)
          [%expr None (* TODO *)]
      in
      let pp =
        plain_fun_def
          "pp"
          [ "ppf"; "_tm" ]
          (*
          [%expr
            tm |> [%e of_plain] |> [%e to_nominal] |> Lvca_syntax.Nominal.Term.pp ppf]
            *)
          [%expr Fmt.pf ppf "TODO"]
      in
      let parse =
        plain_fun_def
          "parse"
          []
          (*
          [%expr
            let parse_prim =
              Lvca_parsing.fail "Generated parser parse_prim always fails"
            in
            Lvca_parsing.(
              Lvca_syntax.Nominal.Term.parse ~comment:Lvca_parsing.c_comment ~parse_prim
              >>= fun tm ->
              match [%e of_nominal] tm with
              | Ok tm -> return ([%e to_plain] tm)
              | Error _ -> fail "Generated parser failed nominal conversion")]
              *)
          [%expr Lvca_parsing.fail "TODO"]
      in
      [%stri
        module Plain = struct
          [%%i plain_type_decl]
          [%%i equal]
          [%%i jsonify]
          [%%i unjsonify]
          [%%i pp]
          [%%i parse]
        end]
    in
    let expr = pmod_structure (info_type_decl :: fun_defs @ [ plain_mod ]) in
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

  let mk (Syn.{ externals; sort_defs } as lang) =
    let prims = prims_of_externals externals in
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
    let partitioned_sorts = partition_sort_defs sort_defs in
    let wrapper_module =
      Wrapper_module.mk
        ~prims
        ~sort_def_map
        ~sort_dep_map
        ~sorted_scc_graph
        ~partitioned_sorts
    in
    let type_modules =
      List.map sort_defs ~f:(fun (sort_name, sort_def) ->
          Individual_type_module.mk
            ~prims
            ~sort_def_map
            ~partitioned_sorts
            sort_name
            sort_def)
    in
    pmod_structure
      ([ wrapper_module
       ; [%stri module Types = Wrapper.Types]
       ; [%stri module Plain = Wrapper.Plain]
       ; [%stri let language = [%e Syntax_quoter.Exp.language ~loc lang]]
       ]
      @ type_modules)
  ;;
end

module Sig (Context : Builder_context) = struct
  open Context
  open Ast
  open Helpers (Context)
  module Individual_type_sig = Individual_type_sig (Context)
  module Wrapper_module = Wrapper_module (Context)

  let mk Syn.{ externals; sort_defs } =
    let prims = prims_of_externals externals in
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
    let partitioned_sorts = partition_sort_defs sort_defs in
    let type_sigs =
      List.map
        sort_defs
        ~f:(fun (sort_name, (Syn.Sort_def.Sort_def (vars, _op_defs) as sort_def)) ->
          let mk_ty info =
            Individual_type_sig.mk
              ~prims
              ~sort_def_map
              ~partitioned_sorts
              ~info
              sort_name
              sort_def
          in
          let taken = vars |> List.map ~f:fst |> SSet.of_list in
          let taken, unique_var_names =
            List.fold_map vars ~init:taken ~f:(fun taken (name, _) ->
                let name' = generate_unique_name ~base:(name ^ "_") taken in
                let taken = Set.add taken name' in
                let name'' = generate_unique_name ~base:(name ^ "__") taken in
                let taken = Set.add taken name'' in
                taken, (name', name''))
          in
          let unique_vars = List.map unique_var_names ~f:(Tuple2.map ~f:ptyp_var) in
          let unique_vars_1, unique_vars_2 = List.unzip unique_vars in
          let info_v = generate_unique_name ~base:"info" taken in
          let taken = Set.add taken info_v in
          let t = ptyp_constr { txt = Lident "t"; loc } in
          let plain_t = ptyp_constr { txt = unflatten [ "Plain"; "t" ]; loc } in
          let info_a, info_b =
            match
              Sequence.take (generate_unique_names ~base:"info" taken) 2
              |> Sequence.to_list
            with
            | [ x; y ] -> ptyp_var x, ptyp_var y
            | _ -> Lvca_util.invariant_violation ~here:[%here] "expected two names"
          in
          let fold_ty ~init ~f =
            List.fold_right unique_vars ~init ~f:(fun v1v2 ty ->
                [%type: [%t f v1v2] -> [%t ty]])
          in
          let declare txt ~init ~f =
            psig_value
              (value_description ~name:{ txt; loc } ~type_:(fold_ty ~init ~f) ~prim:[])
          in
          let type_ =
            pmty_signature
              [ mk_ty With_info
              ; [%sigi:
                  module Plain : [%m
                  let declare name ~f =
                    declare name ~init:(f (t unique_vars_1)) ~f:(fun (v1, _v2) -> f v1)
                  in
                  pmty_signature
                    [ mk_ty Plain
                    ; declare "pp" ~f:(fun ty -> [%type: [%t ty] Fmt.t])
                    ; declare
                        (if List.is_empty vars then "=" else "equal")
                        ~f:(fun ty -> [%type: [%t ty] -> [%t ty] -> bool])
                    ; declare "parse" ~f:(fun ty -> [%type: [%t ty] Lvca_parsing.t])
                    ; declare "jsonify" ~f:(fun ty ->
                          [%type: [%t ty] Lvca_util.Json.serializer])
                    ; declare "unjsonify" ~f:(fun ty ->
                          [%type: [%t ty] Lvca_util.Json.deserializer])
                    ]]]
              ; declare
                  "to_plain"
                  ~init:
                    [%type:
                      [%t t (ptyp_any :: unique_vars_1)] -> [%t plain_t unique_vars_2]]
                  ~f:(fun (v1, v2) -> ptyp_arrow Nolabel v1 v2)
              ; declare
                  "of_plain"
                  ~init:
                    [%type:
                      [%t plain_t unique_vars_1]
                      -> [%t t ([%type: unit] :: unique_vars_2)]]
                  ~f:(fun (v1, v2) -> ptyp_arrow Nolabel v1 v2)
              ; (let template dom =
                   [%type: [%t dom] -> [%t info_a] Lvca_syntax.Nominal.Term.t]
                 in
                 declare
                   "to_nominal"
                   ~init:(template (t (info_a :: unique_vars_1)))
                   ~f:(fun (v1, _) -> template v1))
              ; (let term = [%type: [%t info_a] Lvca_syntax.Nominal.Term.t] in
                 let template codom =
                   [%type: [%t term] -> ([%t codom], [%t term]) Result.t]
                 in
                 declare
                   "of_nominal"
                   ~init:(template (t (info_a :: unique_vars_1)))
                   ~f:(fun (v1, _) -> template v1))
              ; declare
                  "info"
                  ~init:[%type: [%t t (ptyp_var info_v :: unique_vars_2)] -> 'info]
                  ~f:(fun _ -> ptyp_any)
              ; declare
                  "map_info"
                  ~init:
                    [%type:
                      f:([%t info_a] -> [%t info_b])
                      -> [%t t (info_a :: unique_vars_1)]
                      -> [%t t (info_b :: unique_vars_2)]]
                  ~f:(fun (v1, v2) ->
                    [%type: f:([%t info_a] -> [%t info_b]) -> [%t v1] -> [%t v2]])
              ]
          in
          psig_module
            (module_declaration ~name:{ txt = Some (module_name sort_name); loc } ~type_))
    in
    let language =
      [%sigi:
        val language : string Lvca_provenance.Commented.t Lvca_syntax.Abstract_syntax.t]
    in
    pmty_signature
      (language
       :: Wrapper_module.mk_sigs ~prims ~sort_def_map ~sorted_scc_graph ~partitioned_sorts
      @ type_sigs)
  ;;
end

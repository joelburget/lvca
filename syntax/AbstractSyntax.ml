(** Types for representing languages *)

open Base

type 'info pattern_sort =
  { pattern_sort : 'info Sort.t
  ; var_sort : 'info Sort.t
  }

type 'info sort_slot =
  | SortBinding of 'info Sort.t
  | SortPattern of 'info pattern_sort

type 'info valence = Valence of 'info sort_slot list * 'info Sort.t
type 'info arity = 'info valence list
type 'info operator_def = OperatorDef of string * 'info arity
type 'info sort_def = SortDef of string list * 'info operator_def list
type 'info abstract_syntax = (string * 'info sort_def) list
type 'info t = 'info abstract_syntax
type 'info unordered = 'info sort_def Lvca_util.String.Map.t

let unordered = Lvca_util.String.Map.of_alist

let equal info_eq =
  let slot_eq slot1 slot2 =
    match slot1, slot2 with
    | SortBinding s1, SortBinding s2 -> Sort.equal info_eq s1 s2
    | SortPattern ps1, SortPattern ps2 ->
      Sort.equal info_eq ps1.pattern_sort ps2.pattern_sort
      && Sort.equal info_eq ps1.var_sort ps2.var_sort
    | _, _ -> false
  in
  let valence_eq (Valence (slots1, sort1)) (Valence (slots2, sort2)) =
    List.equal slot_eq slots1 slots2 && Sort.equal info_eq sort1 sort2
  in
  let arity_eq = List.equal valence_eq in
  let op_def_eq (OperatorDef (name1, arity1)) (OperatorDef (name2, arity2)) =
    String.(name1 = name2) && arity_eq arity1 arity2
  in
  let sort_def_eq (SortDef (vs1, ops1)) (SortDef (vs2, ops2)) =
    List.equal String.( = ) vs1 vs2 && List.equal op_def_eq ops1 ops2
  in
  List.equal (Lvca_util.Tuple2.equal String.( = ) sort_def_eq)
;;

let sort_slot_map_info ~f = function
  | SortBinding s -> SortBinding (Sort.map_info ~f s)
  | SortPattern { pattern_sort; var_sort } ->
    SortPattern
      { pattern_sort = Sort.map_info ~f pattern_sort
      ; var_sort = Sort.map_info ~f var_sort
      }
;;

let valence_map_info ~f (Valence (slots, sort)) =
  Valence (List.map ~f:(sort_slot_map_info ~f) slots, Sort.map_info ~f sort)
;;

let arity_map_info ~f = List.map ~f:(valence_map_info ~f)
let erase_arity = arity_map_info ~f:(Fn.const ())

let operator_def_map_info ~f (OperatorDef (name, arity)) =
  OperatorDef (name, arity_map_info ~f arity)
;;

let sort_def_map_info ~f (name, SortDef (vars, op_defs)) =
  name, SortDef (vars, op_defs |> List.map ~f:(operator_def_map_info ~f))
;;

let erase_sort_def = sort_def_map_info ~f:(Fn.const ())
let map_info ~f = List.map ~f:(sort_def_map_info ~f)
let erase_info t = map_info ~f:(Fn.const ()) t

let string_of_sort_slot = function
  | SortBinding sort -> Sort.to_string sort
  | SortPattern { pattern_sort; var_sort } ->
    Printf.sprintf
      (match pattern_sort with Sort.Name _ -> "%s[%s]" | _ -> "(%s)[%s]")
      (Sort.to_string pattern_sort)
      (Sort.to_string var_sort)
;;

let string_of_valence : _ valence -> string = function
  | Valence (binders, result) ->
    let result_str = Sort.to_string result in
    (match binders with
    | [] -> result_str
    | _ ->
      Printf.sprintf
        "%s. %s"
        (binders |> List.map ~f:string_of_sort_slot |> String.concat ~sep:". ")
        result_str)
;;

let string_of_arity : _ arity -> string =
 fun valences -> valences |> List.map ~f:string_of_valence |> String.concat ~sep:"; "
;;

let instantiate_sort_slot env = function
  | SortBinding s -> SortBinding (Sort.instantiate env s)
  | SortPattern { pattern_sort; var_sort } ->
    SortPattern
      { pattern_sort = Sort.instantiate env pattern_sort
      ; var_sort = Sort.instantiate env var_sort
      }
;;

let instantiate_valence env = function
  | Valence (binding_sort_slots, body_sort) ->
    Valence
      ( List.map binding_sort_slots ~f:(instantiate_sort_slot env)
      , Sort.instantiate env body_sort )
;;

let instantiate_arity env = List.map ~f:(instantiate_valence env)

let lookup_operator sort_defs sort_name op_name =
  let open Option.Let_syntax in
  let%bind (SortDef (vars, operator_defs)) =
    List.find_map sort_defs ~f:(fun (name, def) ->
        if String.(name = sort_name) then Some def else None)
  in
  let%bind result =
    List.find operator_defs ~f:(fun (OperatorDef (op_def_name, _)) ->
        String.(op_def_name = op_name))
  in
  Some (vars, result)
;;

(* TODO
let pp_sort_def ppf (SortDef (sort_vars, operator_defs)) =

let pp = Fmt.list ~sep:(Fmt.sps 0) (Fmt.pair ~sep:(Fmt.any ":=") Fmt.string pp_sort_def)
*)

type kind_map = int Lvca_util.String.Map.t
type kind_mismap = Lvca_util.Int.Set.t Lvca_util.String.Map.t

let update_env env name n =
  Map.update env name ~f:(function
      | None -> Lvca_util.Int.Set.singleton n
      | Some set -> Set.add set n)
;;

let rec kind_check_sort env sort =
  match sort with
  | Sort.Name (_, name) -> update_env env name 0
  | Sort.Ap (_, name, args) ->
    let env = List.fold args ~init:env ~f:kind_check_sort in
    update_env env name (List.length args)
;;

let kind_check_sort_slot env = function
  | SortBinding sort -> kind_check_sort env sort
  | SortPattern { pattern_sort; var_sort } ->
    [ pattern_sort; var_sort ] |> List.fold ~init:env ~f:kind_check_sort
;;

let kind_check_valence env (Valence (binding_slots, value_sort)) =
  let env = binding_slots |> List.fold ~init:env ~f:kind_check_sort_slot in
  kind_check_sort env value_sort
;;

(* XXX finish *)
(* value_slot :: binding_slots |> List.map ~f:fst |> List.fold ~init:env ~f:kind_check_sort *)

let kind_check_operator_def env (OperatorDef (_name, arity)) =
  arity |> List.fold ~init:env ~f:kind_check_valence
;;

let kind_check_sort_def env sort_name (SortDef (vars, operators)) =
  let env = update_env env sort_name (List.length vars) in
  List.fold operators ~init:env ~f:kind_check_operator_def
;;

let kind_check ?(env = Lvca_util.String.Map.empty) syntax =
  let env = env |> Map.map ~f:Lvca_util.Int.Set.singleton in
  let mismap =
    syntax
    |> List.fold ~init:env ~f:(fun env (sort_name, sort_def) ->
           kind_check_sort_def env sort_name sort_def)
  in
  let fine_vars, mismapped_vars =
    mismap
    |> Map.to_alist
    |> List.partition_map ~f:(fun (name, mismap) ->
           match Set.length mismap with
           | 1 -> Either.First (name, Set.min_elt_exn mismap)
           | _ -> Either.Second (name, mismap))
  in
  match mismapped_vars with
  | [] -> Ok (Lvca_util.String.Map.of_alist_exn fine_vars)
  | _ -> Error (Lvca_util.String.Map.of_alist_exn mismapped_vars)
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  open Parsers
  module ParseSort = Sort.Parse (Comment)

  exception BuildArityError of string

  (* punctuation *)
  let assign = string ":="
  let bar = char '|'
  let dot = char '.'
  let semi = char ';'

  type sort_sequence_entry =
    | Sort of OptRange.t Sort.t
    | Bracketed of OptRange.t Sort.t
    | Dot
    | Semi

  type sort_sequence = sort_sequence_entry list

  let str_of_entry = function
    | Sort sort -> Sort.to_string sort
    | Bracketed sort -> "[" ^ Sort.to_string sort ^ "]"
    | Dot -> "."
    | Semi -> ";"
  ;;

  (* Fold a sequence of sorts, '.'s, and ';'s to an arity. *)
  let build_arity : sort_sequence -> OptRange.t arity =
   fun sort_sequence ->
    let binding_sorts : OptRange.t sort_slot Queue.t = Queue.create () in
    let rec go : sort_sequence -> OptRange.t valence list = function
      | [] -> []
      (* s. ss *)
      | Sort sort :: Dot :: sorts ->
        Queue.enqueue binding_sorts (SortBinding sort);
        go sorts
      (* s1[s2]. ss *)
      | Sort pattern_sort :: Bracketed var_sort :: Dot :: sorts ->
        Queue.enqueue binding_sorts (SortPattern { pattern_sort; var_sort });
        go sorts
      (* s; ss *)
      | Sort sort :: Semi :: sorts | Sort sort :: ([] as sorts) ->
        let binding_sorts_lst = Queue.to_list binding_sorts in
        Queue.clear binding_sorts;
        Valence (binding_sorts_lst, sort) :: go sorts
      | sorts ->
        let entries_str = sorts |> List.map ~f:str_of_entry |> String.concat ~sep:" " in
        let binding_sorts_lst = Queue.to_list binding_sorts in
        let sorts_str =
          binding_sorts_lst |> List.map ~f:string_of_sort_slot |> String.concat ~sep:". "
        in
        let msg =
          match binding_sorts_lst with
          | [] -> entries_str
          | _ -> entries_str ^ " " ^ sorts_str
        in
        raise (BuildArityError (Printf.sprintf "Unexpected sequence of sorts: %s" msg))
    in
    go sort_sequence
 ;;

  let arity : OptRange.t arity Parsers.t =
    parens
      (many
         (choice
            [ semi >>| Fn.const Semi
            ; dot >>| Fn.const Dot
            ; (ParseSort.t >>| fun sort -> Sort sort)
            ; (brackets ParseSort.t >>| fun sort -> Bracketed sort)
            ])
      >>== fun ~pos sort_sequence ->
      try return ~pos (build_arity sort_sequence) with BuildArityError msg -> fail msg)
    <?> "arity"
  ;;

  let operator_def : OptRange.t operator_def Parsers.t =
    lift2 (fun ident arity -> OperatorDef (ident, arity)) identifier arity
  ;;

  let sort_def : (string * OptRange.t sort_def) Parsers.t =
    lift4
      (fun ident bound_names _assign op_defs -> ident, SortDef (bound_names, op_defs))
      identifier
      (many identifier)
      assign
      (* TODO: allow empty sorts? *)
      (option '|' bar *> sep_by1 bar operator_def)
    <?> "sort definition"
  ;;

  let t : OptRange.t abstract_syntax Parsers.t = many1 sort_def <?> "abstract syntax"
  let whitespace_t = junk *> t
end

let%test_module "AbstractSyntax_Parser" =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let parse_with : 'a ParseUtil.t -> string -> 'a =
     fun p str ->
      match Angstrom.parse_string ~consume:All p str with
      | Ok (t, _pos) -> t
      | Error msg -> failwith msg
   ;;

    let tm = Sort.Name ((), "tm")
    let tm_v = Valence ([], tm)
    let integer = Sort.Name ((), "integer")
    let integer_v = Valence ([], integer)

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(integer)" |> erase_arity = [ integer_v ]))
    ;;

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm; tm)" |> erase_arity = [ tm_v; tm_v ]))
    ;;

    let%test_unit _ =
      assert (
        Caml.(
          parse_with Parse.arity "(tm. tm)"
          |> erase_arity
          = [ Valence ([ SortBinding tm ], tm) ]))
    ;;

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm)" |> erase_arity = [ Valence ([], tm) ]))
    ;;

    let%test_unit _ =
      assert (
        Caml.(
          parse_with Parse.arity "(tm[tm]. tm)"
          |> erase_arity
          = [ Valence ([ SortPattern { pattern_sort = tm; var_sort = tm } ], tm) ]))
    ;;

    let expect_okay str =
      match Angstrom.parse_string ~consume:All Parse.arity str with
      | Ok _ -> ()
      | Error msg -> Stdio.print_string msg
    ;;

    let%expect_test _ =
      expect_okay "(tm[tm]. tm[tm]. tm)";
      [%expect]
    ;;

    let%expect_test _ =
      expect_okay "((foo bar)[baz quux]. tm)";
      [%expect]
    ;;

    let%test_unit _ = assert (Caml.(parse_with Parse.arity "()" = []))

    let%test_unit _ =
      assert (Caml.(parse_with Parse.operator_def "foo()" = OperatorDef ("foo", [])))
    ;;

    let%test_unit _ =
      assert (
        Caml.(
          parse_with Parse.sort_def {|foo := foo()|}
          = ("foo", SortDef ([], [ OperatorDef ("foo", []) ]))))
    ;;

    let%test_unit _ =
      assert (
        Caml.(
          parse_with Parse.sort_def {|foo x := foo()|}
          = ("foo", SortDef ([ "x" ], [ OperatorDef ("foo", []) ]))))
    ;;

    let tm_def =
      ( "tm"
      , SortDef
          ([], [ OperatorDef ("add", [ tm_v; tm_v ]); OperatorDef ("lit", [ integer_v ]) ])
      )
    ;;

    let%test_unit _ =
      assert (
        Caml.(
          parse_with Parse.sort_def {|tm :=
  | add(tm; tm)
  | lit(integer)
      |}
          |> erase_sort_def
          = tm_def))
    ;;

    let%test_unit _ =
      assert (
        let parsed =
          parse_with Parse.whitespace_t {|
tm :=
  | add(tm; tm)
  | lit(integer)
      |}
          |> erase_info
        in
        equal Unit.( = ) parsed [ tm_def ])
    ;;

    let kind_check str =
      let lang = parse_with Parse.whitespace_t str in
      match kind_check lang with
      | Ok map ->
        Stdio.printf "okay\n";
        map |> Map.iteri ~f:(fun ~key ~data -> Stdio.printf "%s: %d\n" key data)
      | Error mismap ->
        Stdio.printf "failed\n";
        mismap
        |> Map.iteri ~f:(fun ~key ~data ->
               let inferred_kinds_str =
                 data
                 |> Set.to_list
                 |> List.map ~f:Int.to_string
                 |> String.concat ~sep:", "
               in
               Stdio.printf "%s: %s\n" key inferred_kinds_str)
    ;;

    let%expect_test _ =
      kind_check {|tm a := foo(a)|};
      [%expect {|
        okay
        a: 0
        tm: 1 |}]
    ;;

    let%expect_test _ =
      kind_check {|tm a := foo(a) | bar(a b)|};
      [%expect {|
        failed
        a: 0, 1 |}]
    ;;

    let%expect_test _ =
      kind_check {|tm := foo(a) | bar(a b)|};
      [%expect {|
        failed
        a: 0, 1 |}]
    ;;
  end)
;;

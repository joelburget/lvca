(** Types for representing languages *)

open Base

type starred =
  | Starred
  | Unstarred

type sort_slot = Sort.t * starred
type valence = Valence of sort_slot list * sort_slot
type arity = valence list
type operator_def = OperatorDef of string * arity
type sort_def = SortDef of string list * operator_def list
type abstract_syntax = (string * sort_def) list
type t = abstract_syntax

let ( = ) = List.equal Caml.( = )

let string_of_sort_slot : sort_slot -> string =
 fun (sort, starred) ->
  Printf.sprintf
    "%s%s"
    (Sort.to_string sort)
    (match starred with Starred -> "*" | Unstarred -> "")
;;

let string_of_valence : valence -> string = function
  | Valence (binders, result) ->
    (match binders with
    | [] -> string_of_sort_slot result
    | _ ->
      Printf.sprintf
        "%s. %s"
        (binders |> List.map ~f:string_of_sort_slot |> String.concat ~sep:". ")
        (string_of_sort_slot result))
;;

let string_of_arity : arity -> string =
 fun valences -> valences |> List.map ~f:string_of_valence |> String.concat ~sep:"; "
;;

(* TODO
let pp_sort_def ppf (SortDef (sort_vars, operator_defs)) =

let pp = Fmt.list ~sep:(Fmt.sps 0) (Fmt.pair ~sep:(Fmt.any ":=") Fmt.string pp_sort_def)
*)

type kind_map = int Lvca_util.String.Map.t
type kind_mismap = Lvca_util.Int.Set.t Lvca_util.String.Map.t

let rec kind_check_sort env sort =
  let name, n, env =
    match sort with
    | Sort.Name name -> name, 0, env
    | Sort.Ap (name, args) ->
      name, List.length args, List.fold args ~init:env ~f:kind_check_sort
  in
  Map.update env name ~f:(function
      | None -> Lvca_util.Int.Set.singleton n
      | Some set -> Set.add set n)
;;

let kind_check_valence env (Valence (binding_slots, value_slot)) =
  value_slot :: binding_slots |> List.map ~f:fst |> List.fold ~init:env ~f:kind_check_sort
;;

let kind_check_operator_def env (OperatorDef (_name, arity)) =
  arity |> List.fold ~init:env ~f:kind_check_valence
;;

let kind_check_sort env sort_name (SortDef (vars, operators)) =
  let n = List.length vars in
  let env =
    Map.update env sort_name ~f:(function
        | None -> Lvca_util.Int.Set.singleton n
        | Some set -> Set.add set n)
  in
  List.fold operators ~init:env ~f:kind_check_operator_def
;;

let kind_check ?(env = Lvca_util.String.Map.empty) syntax =
  let env = env |> Map.map ~f:Lvca_util.Int.Set.singleton in
  let mismap =
    syntax
    |> List.fold ~init:env ~f:(fun env (sort_name, sort_def) ->
           kind_check_sort env sort_name sort_def)
  in
  let fine_vars, mismapped_vars =
    mismap
    |> Map.to_alist
    |> List.partition_map ~f:(fun (name, mismap) ->
           match Set.to_list mismap with
           | [ x ] -> Either.First (name, x)
           | xs -> Either.Second (name, Lvca_util.Int.Set.of_list xs))
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
  let star = char '*'

  type sort_sequence_entry =
    | Sort of Sort.t * starred
    | Dot
    | Semi

  type sort_sequence = sort_sequence_entry list

  let str_of_entry = function
    | Sort (sort, Starred) -> Sort.to_string sort ^ "*"
    | Sort (sort, Unstarred) -> Sort.to_string sort
    | Dot -> "."
    | Semi -> ";"
  ;;

  (* Fold a sequence of sorts, '.'s, and ';'s to an arity. *)
  let build_arity : sort_sequence -> arity =
   fun sort_sequence ->
    let binding_sorts : sort_slot Queue.t = Queue.create () in
    let rec go : sort_sequence -> valence list = function
      | [] -> []
      | Sort (sort, starred) :: Dot :: sorts ->
        Queue.enqueue binding_sorts (sort, starred);
        go sorts
      | Sort (sort, starred) :: Semi :: sorts | Sort (sort, starred) :: ([] as sorts) ->
        let binding_sorts_lst = Queue.to_list binding_sorts in
        Queue.clear binding_sorts;
        Valence (binding_sorts_lst, (sort, starred)) :: go sorts
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

  let arity : arity Parsers.t =
    parens
      (many
         (choice
            [ semi >>| Fn.const Semi
            ; dot >>| Fn.const Dot
            ; (ParseSort.t
              >>== fun ~pos:p1 sort' ->
              choice
                [ (star
                  >>== fun ~pos:p2 _ ->
                  let pos = OptRange.union p1 p2 in
                  return ~pos (Sort (sort', Starred)))
                ; return ~pos:p1 (Sort (sort', Unstarred))
                ])
            ])
      >>== fun ~pos sort_sequence ->
      try return ~pos (build_arity sort_sequence) with BuildArityError msg -> fail msg)
    <?> "arity"
  ;;

  let operator_def : operator_def Parsers.t =
    lift2 (fun ident arity -> OperatorDef (ident, arity)) identifier arity
  ;;

  let sort_def : (string * sort_def) Parsers.t =
    lift4
      (fun ident bound_names _assign op_defs -> ident, SortDef (bound_names, op_defs))
      identifier
      (many identifier)
      assign
      (* TODO: allow empty sorts? *)
      (option '|' bar *> sep_by1 bar operator_def)
    <?> "sort definition"
  ;;

  let t : abstract_syntax Parsers.t = many1 sort_def <?> "abstract syntax"
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

    let tm = Sort.Name "tm"
    let tm_s = tm, Starred
    let tm_u = tm, Unstarred
    let tm_v = Valence ([], tm_u)
    let integer = Sort.Name "integer"
    let integer_v = Valence ([], (integer, Unstarred))

    let%test_unit _ = assert (Caml.(parse_with Parse.arity "(integer)" = [ integer_v ]))
    let%test_unit _ = assert (Caml.(parse_with Parse.arity "(tm; tm)" = [ tm_v; tm_v ]))

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm. tm)" = [ Valence ([ tm_u ], tm_u) ]))
    ;;

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm*)" = [ Valence ([], tm_s) ]))
    ;;

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm*. tm)" = [ Valence ([ tm_s ], tm_u) ]))
    ;;

    let expect_okay str =
      match Angstrom.parse_string ~consume:All Parse.arity str with
      | Ok _ -> ()
      | Error msg -> Stdio.print_string msg
    ;;

    let%expect_test _ =
      expect_okay "(tm*. tm*. tm)";
      [%expect]
    ;;

    let%expect_test _ =
      expect_okay "(tm*. tm*. tm*)";
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
          = tm_def))
    ;;

    let%test_unit _ =
      assert (
        parse_with Parse.whitespace_t {|
tm :=
  | add(tm; tm)
  | lit(integer)
      |}
        = [ tm_def ])
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
      [%expect{|
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

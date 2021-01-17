(** Types for representing languages *)

module Util = Lvca_util
module String = Util.String
module List = Base.List
module Map = Base.Map
module Result = Base.Result

type sort_name = string

(** Sorts divide ASTs into syntactic categories.

    Notes about our representation:

    - Concrete sorts are always represented by a [SortAp], even if not applied to
      anything. For example, [integer] is represented as [SortAp ("integer", * \[\])].
    - We don't allow higher-order sorts. In other words, no functions at the sort level.
      In other words, the head of an application is always concrete. *)
type sort =
  | SortAp of sort_name * sort list (** A higher-kinded sort can be applied *)
  | SortVar of string

type starred =
  | Starred
  | Unstarred

type sort_slot = sort * starred

(** A valence represents the sort of an argument (to an operator), as well as the number
    and sorts of the variables bound within it *)
type valence = Valence of sort_slot list * sort_slot

type arity = valence list

type operator_def =
  | OperatorDef of string * arity (** An operator is defined by its tag and arity *)

type sort_def =
  | SortDef of string list * operator_def list
      (** A sort is defined by a set of variables and a set of operators *)

type sort_defs =
  | SortDefs of (string * sort_def) list (** A language is defined by its sorts *)

type abstract_syntax = { sort_defs : sort_defs }
type t = abstract_syntax

(* functions: *)

let sort_defs_eq (SortDefs x) (SortDefs y) = List.equal Caml.( = ) x y

let ( = ) : abstract_syntax -> abstract_syntax -> bool =
 fun x y -> sort_defs_eq x.sort_defs y.sort_defs
;;

let rec pp_sort : Format.formatter -> sort -> unit =
 fun ppf ->
  Format.(
    function
    | SortAp (name, args) -> fprintf ppf "%s(@[%a@])" name pp_sort_args args
    | SortVar name -> fprintf ppf "%s" name)

and pp_sort_args ppf = function
  | [] -> ()
  | [ x ] -> Format.fprintf ppf "%a" pp_sort x
  | x :: xs -> Format.fprintf ppf "%a; %a" pp_sort x pp_sort_args xs
;;

let rec string_of_sort : sort -> string = function
  | SortAp (name, args) ->
    Printf.sprintf "%s(%s)" name (args |> List.map ~f:string_of_sort |> String.concat)
  | SortVar name -> name
;;

let string_of_sort_slot : sort_slot -> string =
 fun (sort, starred) ->
  Printf.sprintf
    "%s%s"
    (string_of_sort sort)
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

let rec instantiate_sort : sort String.Map.t -> sort -> sort =
 fun arg_mapping -> function
  | SortVar name ->
    (match Map.find arg_mapping name with
    | None -> failwith "TODO: error"
    | Some sort' -> sort')
  | SortAp (name, args) -> SortAp (name, List.map args ~f:(instantiate_sort arg_mapping))
;;

(* _of_term: *)

let rec sort_of_term tm =
  match tm with
  | Nominal.Var (_, name) -> Ok (SortVar name)
  | Operator (_, name, args) ->
    let open Result.Let_syntax in
    let%map args' =
      args
      |> List.map ~f:(function Scope ([], [ arg ]) -> sort_of_term arg | _ -> Error tm)
      |> Result.all
    in
    SortAp (name, args')
  | _ -> Error tm
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  open Base
  module Parsers = ParseUtil.Mk (Comment)
  open Parsers

  exception BuildArityError of string

  (* punctuation *)
  let assign = string ":="
  let bar = char '|'
  let dot = char '.'
  let semi = char ';'
  let star = char '*'

  let sort : sort Parsers.t =
    fix (fun sort ->
        identifier
        >>== fun ~pos ident ->
        choice
          [ (parens (sep_by semi sort) >>| fun sorts -> SortAp (ident, sorts))
          ; return ~pos (SortVar ident)
          ])
  ;;

  type sort_sequence_entry =
    | Sort of sort * starred
    | Dot
    | Semi

  type sort_sequence = sort_sequence_entry list

  let str_of_entry = function
    | Sort (sort, Starred) -> string_of_sort sort ^ "*"
    | Sort (sort, Unstarred) -> string_of_sort sort
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
            ; (sort
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
      (option [] (parens (sep_by semi identifier)))
      assign
      (* TODO: allow empty sorts? *)
      (option '|' bar *> sep_by1 bar operator_def)
    <?> "sort definition"
  ;;

  let t : abstract_syntax Parsers.t =
    lift (fun sort_defs -> { sort_defs = SortDefs sort_defs }) (many1 sort_def)
    <?> "abstract syntax"
  ;;

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

    let tm = SortAp ("tm", [])
    let tm_s = tm, Starred
    let tm_u = tm, Unstarred
    let tm_v = Valence ([], tm_u)
    let integer = SortAp ("integer", [])
    let integer_v = Valence ([], (integer, Unstarred))

    let%test_unit _ = assert (Caml.(parse_with Parse.sort "tm()" = tm))
    let%test_unit _ = assert (Caml.(parse_with Parse.arity "(integer())" = [ integer_v ]))

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm(); tm())" = [ tm_v; tm_v ]))
    ;;

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm(). tm())" = [ Valence ([ tm_u ], tm_u) ]))
    ;;

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm()*)" = [ Valence ([], tm_s) ]))
    ;;

    let%test_unit _ =
      assert (Caml.(parse_with Parse.arity "(tm()*. tm())" = [ Valence ([ tm_s ], tm_u) ]))
    ;;

    let expect_okay str =
      match Angstrom.parse_string ~consume:All Parse.arity str with
      | Ok _ -> ()
      | Error msg -> print_string msg
    ;;

    let%expect_test _ =
      expect_okay "(tm()*. tm()*. tm())";
      [%expect]
    ;;

    let%expect_test _ =
      expect_okay "(tm()*. tm()*. tm()*)";
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
          parse_with Parse.sort_def {|foo(x) := foo()|}
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
          parse_with
            Parse.sort_def
            {|tm :=
  | add(tm(); tm())
  | lit(integer())
      |}
          = tm_def))
    ;;

    let%test_unit _ =
      assert (
        parse_with
          Parse.whitespace_t
          {|
tm :=
  | add(tm(); tm())
  | lit(integer())
      |}
        = { sort_defs = SortDefs [ tm_def ] })
    ;;
  end)
;;

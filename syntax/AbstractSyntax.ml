(** Types for representing languages *)

module Util = Lvca_util
module String = Util.String
module List = Base.List
module Map = Base.Map

type sort_name = string

(** Sorts divide ASTs into syntactic categories.

 Notes about our representation:
   - Concrete sorts are always represented by a [SortAp], even if not applied to
     anything. For example, [integer] is represented as [SortAp ("integer", * \[\])].
   - We don't allow higher-order sorts. In other words, no functions at the sort
     level. In other words, the head of an application is always concrete.
*)
type sort =
  | SortAp of sort_name * sort list (** A higher-kinded sort can be applied *)
  | SortVar of string

type starred = Starred | Unstarred

type sort_slot = sort * starred

(** A valence represents the sort of an argument (to an operator), as well as the number
    and sorts of the variables bound within it *)
type valence = Valence of sort_slot list * sort_slot

type arity = valence list

type operator_def = OperatorDef of string * arity
  (** An operator is defined by its tag and arity *)

type sort_def = SortDef of string list * operator_def list
  (** A sort is defined by a set of variables and a set of operators *)

(* TODO: should this be a list so the ordering is fixed / deterministic? *)
type sort_defs = SortDefs of sort_def String.Map.t
  (** A language is defined by its sorts *)

type import =
  { imported_symbols : (string * string option) list
  ; location : string
  }

type abstract_syntax =
  { imports : import list
  ; sort_defs : sort_defs
  }

type t = abstract_syntax

(* functions: *)

let sort_defs_eq (SortDefs x) (SortDefs y) = Map.equal Caml.(=) x y

let (=) : abstract_syntax -> abstract_syntax -> bool
  = fun x y -> Caml.(x.imports = y.imports) &&
    sort_defs_eq x.sort_defs y.sort_defs

let pp_import_symbol : Format.formatter -> string * string option -> unit
  = fun ppf (name1, name2) -> match name2 with
      | None -> Fmt.string ppf name1
      | Some name2' -> Fmt.pf ppf "%s as %s" name1 name2'

let pp_import : Format.formatter -> import -> unit
  = fun ppf { imported_symbols; location }
    -> Fmt.pf ppf "import { %a } from \"%s\""
       (Fmt.list ~sep:(Fmt.any ", ") pp_import_symbol) imported_symbols
       location

let rec pp_sort : Format.formatter -> sort -> unit
  = fun ppf -> Format.(function
    | SortAp (name, args) -> fprintf ppf "%s(@[%a@])"
      name
      pp_sort_args args
    | SortVar name -> fprintf ppf "%s" name)

and pp_sort_args ppf = function
  | [] -> ()
  | [ x ] -> Format.fprintf ppf "%a" pp_sort x
  | x :: xs -> Format.fprintf ppf "%a; %a" pp_sort x pp_sort_args xs
;;

let rec string_of_sort : sort -> string
  = function
    | SortAp (name, args) -> Printf.sprintf "%s(%s)"
      name
      (args |> List.map ~f:string_of_sort |> String.concat)
    | SortVar name -> name
;;

let string_of_sort_slot : sort_slot -> string
  = fun (sort, starred) -> Printf.sprintf "%s%s"
    (string_of_sort sort)
    (match starred with Starred -> "*" | Unstarred -> "")

let string_of_valence : valence -> string
  = function
    | Valence (binders, result) -> (match binders with
      | [] -> string_of_sort_slot result
      | _ -> Printf.sprintf "%s. %s"
        (binders
          |> List.map ~f:string_of_sort_slot
          |> String.concat ~sep:". ")
        (string_of_sort_slot result))

let string_of_arity : arity -> string
  = fun valences -> valences
    |> List.map ~f:string_of_valence
    |> String.concat ~sep:"; "

let rec instantiate_sort : sort String.Map.t -> sort -> sort =
 fun arg_mapping -> function
  | SortVar name ->
    (match Map.find arg_mapping name with
    | None -> failwith "TODO: error"
    | Some sort' -> sort')
  | SortAp (name, args) -> SortAp (name, List.map args ~f:(instantiate_sort arg_mapping))
;;

(* term_of_: *)

(*
let rec term_of_sort : sort -> NonBinding.term
  = function
    | SortAp (name, sorts) -> Operator ("sort_ap",
      [ Primitive (PrimString name)
      ; Sequence (List.map sorts ~f:term_of_sort)
      ])
    | SortVar name -> Operator ("sort_var", [Primitive (PrimString name)])

let term_of_valence : valence -> NonBinding.term
  = function
    | FixedValence (binding_sorts, result_sort)
    -> Operator ("fixed_valence",
    [ Sequence (binding_sorts |> List.map ~f:term_of_sort)
    ; term_of_sort result_sort
    ])
    | VariableValence (s1, s2)
    -> Operator ("variable_valence",
    [ term_of_sort s1
    ; term_of_sort s2
    ])

let term_of_arity : arity -> NonBinding.term
  = function
    | FixedArity valences -> Operator ("fixed_arity",
      [ Sequence (List.map valences ~f:term_of_valence)
      ])
    | VariableArity sort -> Operator ("variable_arity", [ term_of_sort sort ])

let term_of_operator_def : operator_def -> NonBinding.term
  = fun (OperatorDef (op_name, arity)) -> Operator ("operator_def",
    [ Primitive (PrimString (op_name))
    ; term_of_arity arity
    ])

let term_of_sort_def : sort_def -> NonBinding.term
  = fun (SortDef (vars, op_defs)) -> Operator ("sort_def",
    [ Sequence (vars
      |> List.map ~f:(fun str -> NonBinding.Primitive (PrimString str)))
    ; Sequence (op_defs
      |> List.map ~f:term_of_operator_def)
    ])

let term_of_sort_defs : sort_defs -> NonBinding.term
  = fun (SortDefs sort_defs) -> Sequence (sort_defs
    |> Map.to_alist
    |> List.map ~f:(fun (sort_name, sort_def) -> NonBinding.Operator ("pair",
      [ Primitive (PrimString sort_name)
      ; term_of_sort_def sort_def
      ])
    ))

let term_of_option : ('a -> NonBinding.term) -> 'a option -> NonBinding.term
  = fun f -> function
    | None -> Operator ("none", [])
    | Some a -> Operator ("some", [f a])

let term_of_import : import -> NonBinding.term
  = fun { imported_symbols; location } -> NonBinding.Operator ("import",
    [
      Sequence (imported_symbols
      |> List.map ~f:(fun (original_name, binding_name) -> NonBinding.Sequence
        [ Primitive (PrimString original_name)
        ; term_of_option
          (fun binding_name -> Primitive (PrimString binding_name))
          binding_name
        ]))
    ; Primitive (PrimString location)
    ])

let to_term : abstract_syntax -> NonBinding.term
  = fun { imports; sort_defs } -> (Operator ("abstract_syntax",
      [ Sequence (List.map imports ~f:term_of_import)
      ; term_of_sort_defs sort_defs
      ]))
*)

(* _of_term: *)

exception OfTermFailure of string * unit Binding.Nominal.term

(** @raise [OfTermFailure] *)
let rec sort_of_term_exn : 'a Binding.Nominal.term -> sort
  = let erase = Binding.Nominal.erase in
    fun tm -> match tm with
    | Var (_, name) -> SortVar name
    | Operator (_, name, args)
    -> SortAp (name, List.map args ~f:(function
      | Scope ([], [arg]) -> sort_of_term_exn arg
      | _ -> raise (OfTermFailure ("sort_of_term", erase tm))))
    | _ -> raise (OfTermFailure ("sort_of_term", erase tm))

module Parse (Comment : Util.Angstrom.Comment_int) = struct
  open Angstrom
  open Base
  module Parsers = Util.Angstrom.Mk(Comment)

  exception BuildArityError of string

  let braces, char, identifier, parens, string, string_lit =
    Parsers.(braces, char, identifier, parens, string, string_lit)

  (* punctuation *)
  let assign = string ":="
  let bar = char '|'
  let comma = char ','
  let dot = char '.'
  let semi = char ';'
  let star = char '*'

  let sort : sort Angstrom.t
    = fix (fun sort ->
        identifier >>= fun ident ->
        choice
          [ parens (sep_by semi sort) >>| (fun sorts -> SortAp (ident, sorts))
          ; return (SortVar ident)
          ])

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

  (* Fold a sequence of sorts, '.'s, and ';'s to an arity. *)
  let build_arity : sort_sequence -> arity
    = fun sort_sequence ->

      let binding_sorts : sort_slot Queue.t = Queue.create () in

      let rec go : sort_sequence -> valence list
        = function

        | [] -> []

        | Sort (sort, starred) :: Dot :: sorts
        -> Queue.enqueue binding_sorts (sort, starred);
           go sorts

        | Sort (sort, starred) :: Semi :: sorts
        | Sort (sort, starred) :: ([] as sorts)
        -> let binding_sorts_lst = Queue.to_list binding_sorts in
           Queue.clear binding_sorts;
           Valence (binding_sorts_lst, (sort, starred)) :: go sorts


        | sorts ->
          let entries_str = sorts
            |> List.map ~f:str_of_entry
            |> String.concat ~sep:" "
          in

          let binding_sorts_lst = Queue.to_list binding_sorts in

          let sorts_str = binding_sorts_lst
            |> List.map ~f:string_of_sort_slot
            |> String.concat ~sep:". "
          in

          let msg = match binding_sorts_lst with
            | [] -> entries_str
            | _ -> entries_str ^ " " ^ sorts_str
          in

          raise (BuildArityError (Printf.sprintf
            "Unexpected sequence of sorts: %s"
            msg
          ))
      in

      go sort_sequence

  let arity : arity Angstrom.t
    = parens (many (choice
        [ semi >>| Fn.const Semi
        ; dot >>| Fn.const Dot
        ; lift2 (fun sort starred -> Sort (sort, starred))
          sort
          (option Unstarred (star >>| Fn.const Starred))
        ])
        >>= fun sort_sequence ->
          try
            return (build_arity sort_sequence)
          with
            BuildArityError msg -> fail msg
      ) <?> "arity"

  let operator_def : operator_def Angstrom.t
    = lift2 (fun ident arity -> OperatorDef (ident, arity)) identifier arity

  let sort_def : (string * sort_def) Angstrom.t
    = lift4
        (fun ident bound_names _assign op_defs -> ident, SortDef (bound_names, op_defs))
        identifier
        (option [] (parens (sep_by semi identifier)))
        assign
        (* TODO: allow empty sorts? *)
        (option '|' bar *> sep_by1 bar operator_def)
        <?> "sort definition"

  let import_symbol : (string * string option) Angstrom.t
    = lift2 (fun ident as_ident -> ident, as_ident)
      identifier
      (option None ((fun x -> Some x) <$> string "as" *> identifier))
      <?> "import symbol"

  let import : import Angstrom.t
    = lift4
        (fun _import imported_symbols _from location -> { imported_symbols; location })
        (string "import")
        (braces (sep_by1 comma import_symbol))
        (string "from")
        string_lit
        <?> "import"


  let t : abstract_syntax Angstrom.t
    = lift2 (fun imports sort_defs ->
        { imports
        (* XXX remove exn *)
        ; sort_defs = SortDefs (Util.String.Map.of_alist_exn sort_defs)
        })
      (many import)
      (many1 sort_def)
      <?> "abstract syntax"
end

let%test_module "AbstractSyntax_Parser" = (module struct
  module Parse = Parse(Util.Angstrom.NoComment)

  let parse_with : 'a Angstrom.t -> string -> 'a
    = fun p str ->
    match
      Angstrom.parse_string ~consume:All p str
    with
      | Ok t -> t
      | Error msg -> failwith msg

  let tm = SortAp ("tm", [])
  let tm_s = tm, Starred
  let tm_u = tm, Unstarred
  let tm_v = Valence ([], tm_u)
  let integer = SortAp ("integer", [])
  let integer_v = Valence ([], (integer, Unstarred))

  let%test_unit _ =
    assert Caml.(parse_with Parse.sort "tm()" = tm)

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(integer())" = [integer_v])

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm(); tm())" = [tm_v; tm_v])

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm(). tm())" = [Valence ([tm_u], tm_u)])

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm()*)" = [Valence ([], tm_s)])

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm()*. tm())" = [Valence ([tm_s], tm_u)])

  let expect_okay str =
    match
      Angstrom.parse_string ~consume:All Parse.arity str
    with
      | Ok _ -> ()
      | Error msg -> print_string msg

  let%expect_test _ = expect_okay "(tm()*. tm()*. tm())"; [%expect]
  let%expect_test _ = expect_okay "(tm()*. tm()*. tm()*)"; [%expect]

  let%test_unit _ = assert Caml.(parse_with Parse.arity "()" = [])

  let integer_import =
    { imported_symbols = [ "integer", None ]
    ; location = "lvca/builtin"
    }

  let%test_unit _ = assert Caml.(
    parse_with Parse.import {|import {integer} from "lvca/builtin"|} = integer_import)

  let%test_unit _ = assert Caml.(
    parse_with Parse.operator_def "foo()" = OperatorDef ("foo", []))

  let%test_unit _ = assert Caml.(
    parse_with Parse.sort_def {|foo := foo()|}
    =
    ("foo", SortDef ([], [OperatorDef ("foo", [])])))

  let%test_unit _ = assert Caml.(
    parse_with Parse.sort_def {|foo(x) := foo()|}
    =
    ("foo", SortDef (["x"], [OperatorDef ("foo", [])])))

  let tm_def = ("tm", SortDef
    ( []
    , [ OperatorDef ("add", [tm_v; tm_v])
      ; OperatorDef ("lit", [integer_v])
      ]
    ))

  let%test_unit _ = assert Caml.(
    parse_with Parse.sort_def
      {|tm :=
  | add(tm(); tm())
  | lit(integer())
      |}
    = tm_def)

  let%test_unit _ = assert (
    parse_with Angstrom.(Util.Angstrom.whitespace *> Parse.t)
      {|
import {integer} from "lvca/builtin"

tm :=
  | add(tm(); tm())
  | lit(integer())
      |}
      =
      { imports = [ integer_import ]
      ; sort_defs = SortDefs (Util.String.Map.of_alist_exn [ tm_def ])
      })
end);;
(** Types for representing languages *)

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

(** A valence represents the sort of an argument (to an operator), as well as the number
    and sorts of the variables bound within it *)
type valence =
  | FixedValence of sort list * sort (** A fixed valence is known a priori *)
  | VariableValence of sort * sort
      (** A variable valence binds a number of variables not known a priori. All must be
          of the same sort. *)

(** An arity specifies the arguments to an operator *)
type arity =
  | FixedArity of valence list
  (** A fixed arity operator always has the same number of children *)
  | VariableArity of sort
  (** A variable arity operator has a variable number of children (all of the same
      sort (non-binding valence)) *)

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

let string_of_valence : valence -> string
  = function
    | FixedValence (binders, result) -> (match binders with
      | [] -> string_of_sort result
      | _ -> Printf.sprintf "%s. %s"
        (binders
          |> List.map ~f:string_of_sort
          |> String.concat ~sep:". ")
        (string_of_sort result))
      | VariableValence (binder, result) -> Printf.sprintf "%s*. %s"
        (string_of_sort binder)
        (string_of_sort result)

let string_of_arity : arity -> string
  = function
  | FixedArity valences -> valences
    |> List.map ~f:string_of_valence
    |> String.concat ~sep:"; "
  | VariableArity sort -> string_of_sort sort ^ "*"

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

exception OfTermFailure of string * Binding.Nominal.term

(** @raise [OfTermFailure] *)
let rec sort_of_term_exn : Binding.Nominal.term -> sort
  = fun tm -> match tm with
    | Var name -> SortVar name
    | Operator (name, args)
    -> SortAp (name, List.map args ~f:(function
      | Scope ([], arg) -> sort_of_term_exn arg
      | _ -> raise (OfTermFailure ("sort_of_term", tm))))
    | _ -> raise (OfTermFailure ("sort_of_term", tm))

module Parse (Lex : Util.Angstrom.Lexical_int) = struct
  open Angstrom
  open Base
  module Parsers = Util.Angstrom.Mk(Lex)

  let char, identifier, parens, string, string_lit =
    Parsers.(char, identifier, parens, string, string_lit)

  let sort : sort Angstrom.t
    = fix (fun sort ->
        identifier >>= fun ident -> (* TODO: identifier'? *)
        choice
          [ parens (sep_by (char ';') sort) >>| (fun sorts -> SortAp (ident, sorts))
          ; return (SortVar ident)
          ])

  type starred = Starred | Unstarred

  type sort_sequence = (char, sort * starred) Either.t list

  (* Accumulate *)
  let build_arity : sort_sequence -> arity
    = fun sort_sequence ->

      let rec go : sort list -> sort_sequence -> valence list
        = fun binding_sorts sorts -> match binding_sorts, sorts with

        | [], [] -> []

        (* First, handle variable valences: [sort*. sort] *)
        | [], Second (sort1, Starred) ::
              First '.' ::
              Second (sort2, Unstarred) ::
              First ';' ::
              sorts'
        -> VariableValence (sort1, sort2) :: go [] sorts'
        | [], [Second (sort1, Starred); First '.'; Second (sort2, Unstarred)]
        -> [VariableValence (sort1, sort2)]

        | _, Second (sort, Unstarred) :: First '.' :: sorts'
        -> go (binding_sorts @ [sort]) sorts'
        | _, [Second (sort, Unstarred)]
        -> [FixedValence (binding_sorts, sort)]
        | _, Second (sort, Unstarred) :: First ';' :: sorts'
        -> FixedValence (binding_sorts, sort) :: go [] sorts'
        | _ -> failwith "TODO"
      in

      match sort_sequence with
        | [Second (sort, Starred)] -> VariableArity sort
        | _ -> FixedArity (go [] sort_sequence)

  let arity : arity Angstrom.t
    = parens (many (choice
        [ (char ';' >>| fun _ -> Either.First ';')
        ; (char '.' >>| fun _ -> Either.First '.')
        ; (sort >>= fun sort ->
           option Unstarred (char '*' >>| fun _ -> Starred) >>| fun starred ->
           Either.Second (sort, starred))
        ])
        >>| build_arity)

  let operator_def : operator_def Angstrom.t
    (* TODO: identifier'? *)
    = lift2 (fun ident arity -> OperatorDef (ident, arity)) identifier arity

  let assign = string ":="

  let sort_def : (string * sort_def) Angstrom.t
    = identifier >>= fun ident ->
      option [] (parens (sep_by (char ';') identifier)) >>= fun bound_names ->
      assign >>= fun _ ->
      option '|' (char '|') >>= fun _ ->
      (* TODO: allow empty sorts? *)
      sep_by1 (char '|') operator_def >>| fun op_defs ->
      ident, SortDef (bound_names, op_defs)

  let import_symbol : (string * string option) Angstrom.t
    = identifier >>= fun ident ->
      option None ((fun x -> Some x) <$> string "as" *> identifier) >>| fun as_ident ->
      ident, as_ident

  let import : import Angstrom.t
    = string "import" >>= fun _ ->
      char '{' >>= fun _ ->
      sep_by1 (char ',') import_symbol >>= fun imported_symbols ->
      char '}' >>= fun _ ->
      string "from" >>= fun _ ->
      string_lit >>| fun location ->
      (* TODO: Do we want semicolons? *)
      { imported_symbols; location }

  let t : abstract_syntax Angstrom.t
    = lift2 (fun imports sort_defs ->
        { imports
        (* XXX remove exn *)
        ; sort_defs = SortDefs (Util.String.Map.of_alist_exn sort_defs)
        })
      (many import)
      (many1 sort_def)
end

let%test_module "AbstractSyntax_Parser" = (module struct
  module Parse = Parse(struct
    let comment = Angstrom.fail "no comment"
  end)

  let parse_with : 'a Angstrom.t -> string -> 'a
    = fun p str ->
    match
      Angstrom.parse_string ~consume:All p str
    with
      | Ok t -> t
      | Error msg -> failwith msg

  let tm = SortAp ("tm", [])
  let tm_v = FixedValence ([], tm)
  let integer = SortAp ("integer", [])
  let integer_v = FixedValence ([], integer)

  let%test_unit _ =
    assert Caml.(parse_with Parse.sort "tm()" = tm)

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(integer())" = FixedArity [integer_v])

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm(); tm())" = FixedArity [tm_v; tm_v])

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm(). tm())" =
      FixedArity [FixedValence ([tm], tm)])

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm()*)" = VariableArity tm)

  let%test_unit _ =
    assert Caml.(parse_with Parse.arity "(tm()*. tm())" =
      FixedArity [VariableValence (tm, tm)])

  let%test_unit _ = assert Caml.(parse_with Parse.arity "()" = FixedArity [])

  let integer_import =
    { imported_symbols = [ "integer", None ]
    ; location = "builtins"
    }

  let%test_unit _ = assert Caml.(
    parse_with Parse.import {|import {integer} from "builtins"|} = integer_import)

  let%test_unit _ = assert Caml.(
    parse_with Parse.operator_def "foo()" = OperatorDef ("foo", FixedArity []))

  let%test_unit _ = assert Caml.(
    parse_with Parse.sort_def {|foo := foo()|}
    =
    ("foo", SortDef ([], [OperatorDef ("foo", FixedArity [])])))

  let%test_unit _ = assert Caml.(
    parse_with Parse.sort_def {|foo(x) := foo()|}
    =
    ("foo", SortDef (["x"], [OperatorDef ("foo", FixedArity [])])))

  let tm_def = ("tm", SortDef
    ( []
    , [ OperatorDef ("add", FixedArity [tm_v; tm_v])
      ; OperatorDef ("lit", FixedArity [integer_v])
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
import {integer} from "builtins"

tm :=
  | add(tm(); tm())
  | lit(integer())
      |}
      =
      { imports = [ integer_import ]
      ; sort_defs = SortDefs (Util.String.Map.of_alist_exn [ tm_def ])
      })
end);;

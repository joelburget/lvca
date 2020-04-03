(** Types for representing languages *)

open Core_kernel

type sort_name = string

(** Sorts divide ASTs into syntactic categories. * * Notes about our representation: * -
    Concrete sorts are always represented by a [SortAp], even if not applied * to
    anything. For example, [integer] is represented as [SortAp ("integer", * \[||\])]. *
    \- We don't allow higher-order sorts. In other words, no functions at the * sort
    level. In other words, the head of an application is always concrete. *)
type sort =
  | SortAp of sort_name * sort array (** A higher-kinded sort can be applied *)
  | SortVar of string

(** A valence represents the sort of an argument (to an operator), as well as * the number
    and sorts of the variables bound within it *)
type valence =
  | FixedValence of sort list * sort (** A fixed valence is known a priori *)
  | VariableValence of sort * sort
      (** A variable valence binds a number of variables not known a priori. All must be
          of the same sort. *)

(** An arity specifies the arguments to an operator *)
type arity = Arity of string list * valence list
  (** An arity is defined its arity indices and valences. Arity indices are variables
      bound in an arity rule specifying the length of variable-length slots. *)

type operator_def = OperatorDef of string * arity
  (** An operator is defined by its tag and arity *)

type sort_def = SortDef of string list * operator_def list
  (** A sort is defined by a set of variables and a set of operators *)

(* TODO: should this be a list so the ordering is fixed / deterministic? *)
type sort_defs = SortDefs of sort_def String.Map.t
  (** A language is defined by its sorts *)

type import =
  { imported_symbols : (string * string) list
  ; location : string
  }

type abstract_syntax =
  { imports : import list
  ; sort_defs : sort_defs
  }

type t = abstract_syntax

(* functions: *)

let sort_defs_eq (SortDefs x) (SortDefs y) = String.Map.equal Caml.(=) x y

let eq : abstract_syntax -> abstract_syntax -> bool
  = fun x y -> Caml.(x.imports = y.imports) &&
    sort_defs_eq x.sort_defs y.sort_defs

let sort_names : abstract_syntax -> String.Set.t =
 fun { sort_defs = SortDefs sorts; _ } -> sorts |> String.Map.keys |> String.Set.of_list
;;

let rec string_of_sort : sort -> string
  = function
    | SortAp (name, args) -> Printf.sprintf "%s(%s)"
      name
      (args |> Array.map ~f:string_of_sort |> String.concat_array)
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

let rec instantiate_sort : sort String.Map.t -> sort -> sort =
 fun arg_mapping -> function
  | SortVar name ->
    (match String.Map.find arg_mapping name with
    | None -> failwith "TODO: error"
    | Some sort' -> sort')
  | SortAp (name, args) -> SortAp (name, Array.map args ~f:(instantiate_sort arg_mapping))
;;

(* term_of_: *)

let rec term_of_sort : sort -> NonBinding.term
  = function
    | SortAp (name, sorts) -> Operator ("sort_ap",
      [ Primitive (PrimString name)
      ; Sequence (sorts
        |> Array.to_list
        |> List.map ~f:term_of_sort)
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
  = fun (Arity (args, valences)) -> Operator ("arity",
    [ Sequence (args |> List.map ~f:(fun name -> NonBinding.Primitive (PrimString name)))
    ; Sequence (valences |> List.map ~f:term_of_valence)
    ])

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

let term_of_import : import -> NonBinding.term
  = fun { imported_symbols; location } -> NonBinding.Operator ("import",
    [
      Sequence (imported_symbols
      |> List.map ~f:(fun (original_name, binding_name) -> NonBinding.Sequence
        [ Primitive (PrimString original_name)
        ; Primitive (PrimString binding_name)
        ]))
    ; Primitive (PrimString location)
    ])

let to_term : abstract_syntax -> NonBinding.term
  = fun { imports; sort_defs } -> (Operator ("abstract_syntax",
      [ Sequence (List.map imports ~f:term_of_import)
      ; term_of_sort_defs sort_defs
      ]))

(* _of_term: *)

exception OfTermFailure of string * NonBinding.term

let rec sort_of_term : NonBinding.term -> sort
  = function
    | Operator ("sort_ap", [ Primitive (PrimString name) ; Sequence tms ])
    -> SortAp (name, tms |> List.map ~f:sort_of_term |> Array.of_list)
    | Operator ("sort_var", [Primitive (PrimString name)]) -> SortVar name
    | tm -> raise (OfTermFailure ("sort_of_term", tm))
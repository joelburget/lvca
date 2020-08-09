(** Types for describing the abstract syntax of a language. *)

type sort_name = string

(** Sorts divide ASTs into syntactic categories.

 Notes about our representation:
   - Concrete sorts are always represented by a [SortAp], even if not applied to
     anything. For example, [integer] is represented as [SortAp ("integer", \[\])].
   - We don't allow higher-order sorts. In other words, no functions at the sort
     level. In other words, the head of an application is always concrete.
*)
type sort =
  | SortAp of sort_name * sort list (** A higher-kinded sort can be applied *)
  | SortVar of string

(** A sort can be starred to indicate it's repeated, or not. *)
type starred = Starred | Unstarred

(** Represents a place where a sort can go in a valence. *)
type sort_slot = sort * starred

(** A valence represents the sort of an argument (to an operator), as well as the number
    and sorts of the variables bound within it *)
type valence = Valence of sort_slot list * sort_slot

(** An arity specifies the arguments to an operator *)
type arity = valence list

type operator_def = OperatorDef of string * arity
  (** An operator is defined by its tag and arity *)

type sort_def = SortDef of string list * operator_def list
  (** A sort is defined by a set of variables and a set of operators *)

(* TODO: should this be a list so the ordering is fixed / deterministic? *)
type sort_defs = SortDefs of sort_def Lvca_util.String.Map.t
  (** A language is defined by its sorts *)

(** An import includes sorts from another language. It's possible to rename sorts from
  other languages so they don't collide with sorts defined in this language. *)
type import =
  { imported_symbols : (string * string option) list
  ; location : string
  }

(** The abstract syntax of a language is represented by all of the external dependencies
 of the language plus the sorts it defines. *)
type abstract_syntax =
  { imports : import list
  ; sort_defs : sort_defs
  }

type t = abstract_syntax

val (=) : abstract_syntax -> abstract_syntax -> bool

val pp_import : Format.formatter -> import -> unit
val pp_sort : Format.formatter -> sort -> unit
val string_of_sort : sort -> string
val string_of_valence : valence -> string
val string_of_arity : arity -> string
val instantiate_sort : sort Lvca_util.String.Map.t -> sort -> sort

module Parse (Comment : ParseUtil.Angstrom.Comment_int) : sig
  val sort : sort Angstrom.t
  val import : import Angstrom.t
  val t : t Angstrom.t
end

exception OfTermFailure of string * unit Binding.Nominal.term

(** @raise [OfTermFailure] *)
val sort_of_term_exn : 'a Binding.Nominal.term -> sort

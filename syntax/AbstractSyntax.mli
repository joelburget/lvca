(** Types for describing the abstract syntax of a language. *)

(** A sort can be starred to indicate it's repeated, or not. *)
type starred =
  | Starred
  | Unstarred

(** Represents a place where a sort can go in a valence. *)
type sort_slot = Sort.t * starred

(** A valence represents the sort of an argument (to an operator), as well as the number
    and sorts of the variables bound within it *)
type valence = Valence of sort_slot list * sort_slot

(** An arity specifies the arguments to an operator *)
type arity = valence list

type operator_def =
  | OperatorDef of string * arity (** An operator is defined by its tag and arity *)

type sort_def =
  | SortDef of string list * operator_def list
      (** A sort is defined by a set of variables and a set of operators *)

(** The abstract syntax of a language is the sorts it defines. *)
type abstract_syntax = (string * sort_def) list

type t = abstract_syntax

val ( = ) : abstract_syntax -> abstract_syntax -> bool
val string_of_valence : valence -> string
val string_of_arity : arity -> string

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : t ParseUtil.t
  val whitespace_t : t ParseUtil.t
end

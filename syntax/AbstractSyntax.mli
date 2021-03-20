(** Types for describing the abstract syntax of a language. *)

(** A pattern_sort represents the sort of a pattern with variables of some sort. This is
    written as [pattern_sort\[var_sort\]]. *)
type 'info pattern_sort =
  { pattern_sort : 'info Sort.t
  ; var_sort : 'info Sort.t
  }

(** Represents a place where a sort can go in a valence. *)
type 'info sort_slot =
  | SortBinding of 'info Sort.t
  | SortPattern of 'info pattern_sort

(** The kind of a sort is the number of arguments it takes. Invariant: must be a natural
    number. *)
type kind = Kind of int

(** A valence represents a sort, as well as the number and sorts of the variables bound
    within it. Valences are most often used to represent slots in an operator. *)
type 'info valence = Valence of 'info sort_slot list * 'info Sort.t

(** An arity specifies the arguments to an operator. *)
type 'info arity = 'info valence list

type 'info operator_def =
  | OperatorDef of string * 'info arity
      (** An operator is defined by its tag and arity. *)

type 'info sort_def =
  | SortDef of (string * kind option) list * 'info operator_def list
      (** A sort is defined by a set of variables and a set of operators. *)

(** The abstract syntax of a language is the sorts it defines. *)
type 'info abstract_syntax =
  { externals : (string * kind) list
  ; sort_defs : (string * 'info sort_def) list
  }

(** The abstract syntax of a language is the sorts it defines. Definition order is
    significant (so we'll always print definitions in the same order they were parsed. For
    the definition of a language without significant ordering, see [unordered]. *)
type 'info t = 'info abstract_syntax

module Unordered : sig
  (** The same as [abstract_syntax] but definition order is not significant (this is a map
      rather than a list). *)
  type 'info t =
    { externals : kind Lvca_util.String.Map.t
    ; sort_defs : 'info sort_def Lvca_util.String.Map.t
    }
end

val unordered : 'info t -> [ `Ok of 'info Unordered.t | `Duplicate_key of string ]

val equal
  :  ('info -> 'info -> bool)
  -> 'info abstract_syntax
  -> 'info abstract_syntax
  -> bool

val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase_info : _ t -> unit t
val string_of_valence : 'info valence -> string
val string_of_arity : 'info arity -> string

(** {1 Misc} *)

val instantiate_sort_slot
  :  'info Sort.t Lvca_util.String.Map.t
  -> 'info sort_slot
  -> 'info sort_slot

val instantiate_valence
  :  'info Sort.t Lvca_util.String.Map.t
  -> 'info valence
  -> 'info valence

val instantiate_arity : 'info Sort.t Lvca_util.String.Map.t -> 'info arity -> 'info arity

val lookup_operator
  :  'info t
  -> string (** sort name *)
  -> string (** operator_name *)
  -> ((string * kind option) list * 'info operator_def) option

(* TODO val pp : Format.formatter -> t -> unit *)

(** A mapping from the name of a sort to its arity -- the number of arguments it takes. *)
type kind_map = int Lvca_util.String.Map.t

(** A mapping from the name of a sort to the different arities it was asserted or infered
    to have. *)
type kind_mismap = Lvca_util.Int.Set.t Lvca_util.String.Map.t

(** Check that each sort in the syntax has a consistent arity. *)
val kind_check : _ t -> (kind_map, kind_mismap) Result.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : OptRange.t t ParseUtil.t
  val whitespace_t : OptRange.t t ParseUtil.t
end

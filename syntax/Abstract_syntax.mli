open Lvca_util

(** Types for describing the abstract syntax of a language. *)

(** A mapping from the name of a sort to its arity -- the number of arguments it takes. *)
type kind_map = int String.Map.t

(** A mapping from the name of a sort to the different arities it was asserted or infered
    to have. *)
type kind_mismap = Int.Set.t String.Map.t

(** The abstract syntax of a language is the sorts it defines. Definition order is
    significant (so we'll always print definitions in the same order they were parsed. For
    the definition of a language without significant ordering, see [unordered]. *)
type t =
  { externals : (string * Kind.t) list
  ; sort_defs : (string * Sort_def.t) list
  }

module Unordered : sig
  (** The same as [t] but definition order is not significant (this is a map rather than a
      list). *)
  type t =
    { externals : Kind.t String.Map.t
    ; sort_defs : Sort_def.t String.Map.t
    }
end

val mk_unordered : t -> [ `Ok of Unordered.t | `Duplicate_key of string ]
val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
val ( = ) : t -> t -> bool

(** {1 Misc} *)

module Lookup_error : sig
  type t =
    | Sort_not_found of String.Set.t
    | Operator_not_found of String.Set.t

  val pp : t Fmt.t
end

val lookup_operator
  :  t
  -> string (** sort name *)
  -> string (** operator name *)
  -> ((string * Kind.t option) list * Operator_def.t, Lookup_error.t) Result.t

module Find_error : sig
  type t =
    | Ambiguous_operator
    | Operator_not_found

  val pp : t Fmt.t
end

val find_operator
  :  t
  -> string (** operator name *)
  -> (string * Sort_def.t * Operator_def.t, Find_error.t) Result.t

val pp : t Fmt.t

(** Check that each sort in the syntax has a consistent arity. *)
val kind_check : t -> (kind_map, kind_mismap) Result.t

val parse : t Lvca_parsing.Parser.t

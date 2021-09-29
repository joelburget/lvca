open Lvca_util

(** Types for describing the abstract syntax of a language. *)

(** A mapping from the name of a sort to its arity -- the number of arguments it takes. *)
type kind_map = int String.Map.t

(** A mapping from the name of a sort to the different arities it was asserted or infered
    to have. *)
type kind_mismap = Int.Set.t String.Map.t

(** The kind of a sort is the number of arguments it takes. Invariant: must be a natural
    number. *)
module Kind : sig
  type t = Kind of Provenance.t * int

  val mk : ?provenance:Provenance.t -> int -> t
  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val info : t -> Provenance.t
  val pp : t Fmt.t

  module Parse : sig
    val t : t Lvca_parsing.t
    val decl : (string * t) Lvca_parsing.t
  end
end

(** A pattern sort represents the sort of a pattern with variables of some sort. This is
    written as [pattern_sort\[var_sort\]]. *)
module Pattern_sort : sig
  type t =
    { pattern_sort : Sort.t
    ; var_sort : Sort.t
    }

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
  val instantiate : Sort.t String.Map.t -> t -> t
end

(** Represents a place where a sort can go in a valence. *)
module Sort_slot : sig
  type t =
    | Sort_binding of Sort.t
    | Sort_pattern of Pattern_sort.t

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
  val kind_check : Int.Set.t String.Map.t -> t -> Int.Set.t String.Map.t

  (** Instantiate concrete vars in a sort *)
  val instantiate : Sort.t String.Map.t -> t -> t

  val parse : t Lvca_parsing.t
end

(** A valence represents a sort, as well as the number and sorts of the variables bound
    within it. Valences are most often used to represent slots in an operator. *)
module Valence : sig
  type t = Valence of Sort_slot.t list * Sort.t

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t

  (** Instantiate concrete vars in a valence *)
  val instantiate : Sort.t String.Map.t -> t -> t

  val parse : t Lvca_parsing.t
end

(** An arity specifies the arguments to an operator. *)
module Arity : sig
  type t = Arity of Provenance.t * Valence.t list

  val mk : ?provenance:Provenance.t -> Valence.t list -> t
  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t

  (** Instantiate concrete vars in an arity *)
  val instantiate : Sort.t String.Map.t -> t -> t

  val parse : t Lvca_parsing.t
end

module Operator_def : sig
  type t =
    | Operator_def of Provenance.t * string * Arity.t
        (** An operator is defined by its tag and arity. *)

  val mk : ?provenance:Provenance.t -> string -> Arity.t -> t
  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : t Fmt.t
  val parse : t Lvca_parsing.t
end

module Sort_def : sig
  type t =
    | Sort_def of (string * Kind.t option) list * Operator_def.t list
        (** A sort is defined by a set of variables and a set of operators. *)

  val equivalent : ?info_eq:(Provenance.t -> Provenance.t -> bool) -> t -> t -> bool
  val ( = ) : t -> t -> bool
  val pp : name:string -> t Fmt.t
  val kind_check : Int.Set.t String.Map.t -> string -> t -> Int.Set.t String.Map.t
  val parse : (string * t) Lvca_parsing.t
end

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

val lookup_operator
  :  t
  -> string (** sort name *)
  -> string (** operator_name *)
  -> ((string * Kind.t option) list * Operator_def.t) option

val pp : t Fmt.t

(** Check that each sort in the syntax has a consistent arity. *)
val kind_check : t -> (kind_map, kind_mismap) Result.t

val parse : t Lvca_parsing.t

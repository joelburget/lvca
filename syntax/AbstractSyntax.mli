(** Types for describing the abstract syntax of a language. *)

(** A mapping from the name of a sort to its arity -- the number of arguments it takes. *)
type kind_map = int Lvca_util.String.Map.t

(** A mapping from the name of a sort to the different arities it was asserted or infered
    to have. *)
type kind_mismap = Lvca_util.Int.Set.t Lvca_util.String.Map.t

(** The kind of a sort is the number of arguments it takes. Invariant: must be a natural
    number. *)
module Kind : sig
  type 'info t = Kind of 'info * int

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val info : 'info t -> 'info
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp : _ t Fmt.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
    val decl : (string * OptRange.t t) ParseUtil.t
  end
end

(** A pattern sort represents the sort of a pattern with variables of some sort. This is
    written as [pattern_sort\[var_sort\]]. *)
module PatternSort : sig
  type 'info t =
    { pattern_sort : 'info Sort.t
    ; var_sort : 'info Sort.t
    }

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp : _ t Fmt.t
  val instantiate : 'info Sort.t Lvca_util.String.Map.t -> 'info t -> 'info t
end

(** Represents a place where a sort can go in a valence. *)
module SortSlot : sig
  type 'info t =
    | SortBinding of 'info Sort.t
    | SortPattern of 'info PatternSort.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp : _ t Fmt.t

  val kind_check
    :  Lvca_util.Int.Set.t Lvca_util.String.Map.t
    -> 'info t
    -> Lvca_util.Int.Set.t Lvca_util.String.Map.t

  (** Instantiate concrete vars in a sort *)
  val instantiate : 'info Sort.t Lvca_util.String.Map.t -> 'info t -> 'info t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

(** A valence represents a sort, as well as the number and sorts of the variables bound
    within it. Valences are most often used to represent slots in an operator. *)
module Valence : sig
  type 'info t = Valence of 'info SortSlot.t list * 'info Sort.t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp : _ t Fmt.t

  (** Instantiate concrete vars in a valence *)
  val instantiate : 'info Sort.t Lvca_util.String.Map.t -> 'info t -> 'info t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

(** An arity specifies the arguments to an operator. *)
module Arity : sig
  type 'info t = 'info Valence.t list

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val erase : _ t -> unit t
  val pp : _ t Fmt.t

  (** Instantiate concrete vars in an arity *)
  val instantiate : 'info Sort.t Lvca_util.String.Map.t -> 'info t -> 'info t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

module OperatorDef : sig
  type 'info t =
    | OperatorDef of string * 'info Arity.t
        (** An operator is defined by its tag and arity. *)

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp : _ t Fmt.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

module SortDef : sig
  type 'info t =
    | SortDef of (string * 'info Kind.t option) list * 'info OperatorDef.t list
        (** A sort is defined by a set of variables and a set of operators. *)

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val erase : _ t -> unit t
  val pp : name:string -> _ t Fmt.t

  val kind_check
    :  Lvca_util.Int.Set.t Lvca_util.String.Map.t
    -> string
    -> 'info t
    -> Lvca_util.Int.Set.t Lvca_util.String.Map.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : (string * OptRange.t t) ParseUtil.t
  end
end

(** The abstract syntax of a language is the sorts it defines. Definition order is
    significant (so we'll always print definitions in the same order they were parsed. For
    the definition of a language without significant ordering, see [unordered]. *)
type 'info t =
  { externals : (string * 'info Kind.t) list
  ; sort_defs : (string * 'info SortDef.t) list
  }

module Unordered : sig
  (** The same as [t] but definition order is not significant (this is a map rather than a
      list). *)
  type 'info t =
    { externals : 'info Kind.t Lvca_util.String.Map.t
    ; sort_defs : 'info SortDef.t Lvca_util.String.Map.t
    }
end

val mk_unordered : 'info t -> [ `Ok of 'info Unordered.t | `Duplicate_key of string ]
val equal : ('info -> 'info -> bool) -> 'info t -> 'info t -> bool
val map_info : f:('a -> 'b) -> 'a t -> 'b t
val erase_info : _ t -> unit t

(** {1 Misc} *)

val lookup_operator
  :  'info t
  -> string (** sort name *)
  -> string (** operator_name *)
  -> ((string * 'info Kind.t option) list * 'info OperatorDef.t) option

val pp : _ t Fmt.t

(** Check that each sort in the syntax has a consistent arity. *)
val kind_check : _ t -> (kind_map, kind_mismap) Result.t

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : OptRange.t t ParseUtil.t
  val whitespace_t : OptRange.t t ParseUtil.t
end

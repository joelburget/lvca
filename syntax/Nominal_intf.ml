module type TermS = sig
  module Prim : LanguageObject_intf.S
  module Pat : Pattern_intf.S with module Prim = Prim

  type 'info scope

  type 'info t =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Prim.t

  val equal : ('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val info : 'info t -> 'info

  val pp_generic
    :  open_loc:'info Fmt.t
    -> close_loc:'info Fmt.t
    -> pp_pat:'info Pat.t Fmt.t
    -> 'info t Fmt.t

  val pp : _ t Fmt.t
  val pp_range : OptRange.t t Fmt.t
  val pp_ranges : SourceRanges.t t Fmt.t

  (* TODO: remove *)
  val pp_str : _ t -> string
  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : _ t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : _ t -> string

  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val erase : _ t -> unit t

  (** Attempt to convert a non-binding term to a pattern.

      For example, the term [add(lit(1); a)] is convertible to a pattern, but
      [lambda(a. a)] is not. *)
  val to_pattern : 'info t -> ('info Pat.t, unit scope) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

      For example [add(lit(1)); a)] (as a pattern) can be converted to a term. *)
  val of_pattern : 'info Pat.t -> 'info t

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all : 'info t Lvca_util.String.Map.t -> 'info t -> 'info t

  val select_path : path:int list -> 'info t -> ('info t, string) Result.t

  val match_pattern
    :  info_eq:('info -> 'info -> bool)
    -> 'info Pat.t
    -> 'info t
    -> 'info t Lvca_util.String.Map.t option

  val free_vars : _ t -> Lvca_util.String.Set.t

  (** Check that the given term matches the given sort.

      This recursively checks subterms and patterns.

      Checks performed:

      + All used variables must be bound.
      + Variables must have the correct sort at their use site.
      + Primitives must have the correct sort (string / integer).
      + All mentioned operators must appear in the relevant sort.
      + All operators must have the correct number of subterms.
      + Variable-arity terms can have only non-binding terms as children
      + Fixed-valence terms must have the correct number of binders. All must be
        variables.
      + Variable-valence terms must have one binder, a pattern. *)
  val check
    :  ('info Prim.t -> 'info Sort.t -> string option) (** Primitive checker *)
    -> 'info AbstractSyntax.t (** Abstract syntax *)
    -> 'info Sort.t (** Sort to check term against *)
    -> 'info t
    -> ('info, ('info Pat.t, 'info t) Base.Either.t) CheckFailure.t option

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
    val whitespace_t : OptRange.t t ParseUtil.t
  end

  module Properties : Properties_intf.S with type 'info t := 'info t

  (*
  module Primitive : sig
    (** Hardcoded for the Primitive type *)
    val check
      :  'info AbstractSyntax.t (** Abstract syntax *)
      -> 'info Sort.t (** Sort to check term against *)
      -> 'info t
      -> ('info, ('info Pat.t, 'info t) Base.Either.t) CheckFailure.t option
  end
*)
end

module type ScopeS = sig
  module Prim : LanguageObject_intf.S
  module Pat : Pattern_intf.S

  type 'info term
  type 'info t = Scope of 'info Pat.t list * 'info term

  val equal : ('info -> 'info -> bool) -> 'info t -> 'info t -> bool

  val pp_generic
    :  open_loc:'info Fmt.t
    -> close_loc:'info Fmt.t
    -> pp_pat:'info Pat.t Fmt.t
    -> 'info t Fmt.t

  val pp : _ t Fmt.t
  val pp_range : OptRange.t t Fmt.t
  val pp_ranges : SourceRanges.t t Fmt.t
  val pp_str : _ t -> string
  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val erase : _ t -> unit t

  (* TODO?
val to_pattern
  :  ('info, 'prim) t
  -> (('info, 'prim) Pat.t (unit, 'prim) scope) Result.t

val of_pattern : ('info, 'prim) Pat.t -> ('info, 'prim) t
  *)

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all : 'info term Lvca_util.String.Map.t -> 'info t -> 'info t

  (* TODO:
  val free_vars : (_, _) t -> Lvca_util.String.Set.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
    val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  end

  module Properties : Properties_intf.S with type t := (unit, Primitive.t) t
  *)
end

module type S = sig
  module Prim : LanguageObject_intf.S
  module Pat : Pattern_intf.S with module Prim = Prim

  type 'info term =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Prim.t

  and 'info scope = Scope of 'info Pat.t list * 'info term

  module Term :
    TermS
      with module Prim = Prim
       and module Pat = Pat
       and type 'info scope = 'info scope
       and type 'info t = 'info term

  module Scope :
    ScopeS
      with module Prim = Prim
       and module Pat = Pat
       and type 'info term = 'info term
       and type 'info t = 'info scope
end

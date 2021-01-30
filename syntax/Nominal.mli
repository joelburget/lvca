(** Representation of terms that simply uses variable names to represent scope. *)

type ('info, 'prim) term =
  | Operator of 'info * string * ('info, 'prim) scope list
  | Var of 'info * string
  | Primitive of 'info * 'prim

and ('info, 'prim) scope = Scope of ('info, 'prim) Pattern.t list * ('info, 'prim) term

val equal
  :  ('info -> 'info -> bool)
  -> ('prim -> 'prim -> bool)
  -> ('info, 'prim) term
  -> ('info, 'prim) term
  -> bool

val info : ('info, _) term -> 'info

val pp_term_generic
  :  open_loc:'info Fmt.t
  -> close_loc:'info Fmt.t
  -> pp_pat:('prim Fmt.t -> ('info, 'prim) Pattern.t Fmt.t)
  -> pp_prim:'prim Fmt.t
  -> ('info, 'prim) term Fmt.t

val pp_term : 'prim Fmt.t -> (_, 'prim) term Fmt.t
val pp_term_range : 'prim Fmt.t -> (OptRange.t, 'prim) term Fmt.t
val pp_term_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) term Fmt.t
val pp_term_str : 'prim Fmt.t -> (_, 'prim) term -> string
val pp_scope : 'prim Fmt.t -> Format.formatter -> (_, 'prim) scope -> unit
val pp_scope_range : 'prim Fmt.t -> (OptRange.t, 'prim) scope Fmt.t
val pp_scope_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) scope Fmt.t
val pp_scope_str : 'prim Fmt.t -> (_, 'prim) scope -> string
val jsonify : 'prim Lvca_util.Json.serializer -> (_, 'prim) term Lvca_util.Json.serializer

val unjsonify
  :  'prim Lvca_util.Json.deserializer
  -> (unit, 'prim) term Lvca_util.Json.deserializer

(** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
val serialize : 'prim Lvca_util.Json.serializer -> (_, 'prim) term -> Bytes.t

(** Decode from {{:https://cbor.io} CBOR}). *)
val deserialize
  :  'prim Lvca_util.Json.deserializer
  -> Bytes.t
  -> (unit, 'prim) term option

(** The SHA-256 hash of the serialized term. This is useful for content-identifying terms. *)
val hash : 'prim Lvca_util.Json.serializer -> (_, 'prim) term -> string

val map_info : f:('a -> 'b) -> ('a, 'prim) term -> ('b, 'prim) term
val erase : (_, 'prim) term -> (unit, 'prim) term
val erase_scope : (_, 'prim) scope -> (unit, 'prim) scope

(** Indicates that this scope was encountered when attempting to convert to a pattern. *)

(** Attempt to convert a non-binding term to a pattern.

    For example, the term [add(lit(1); a)] is convertible to a pattern, but [lambda(a. a)]
    is not. *)
val to_pattern
  :  ('info, 'prim) term
  -> (('info, 'prim) Pattern.t, (unit, 'prim) scope) Result.t

(** Convert from a pattern to the corresponding term. This always succeeds.

    For example [add(lit(1)); a)] (as a pattern) can be converted to a term. *)
val pattern_to_term : ('info, 'prim) Pattern.t -> ('info, 'prim) term

(** Substitute all the variables in the context.

    Leaves variables not found in the context free. *)
val subst_all
  :  ('info, 'prim) term Lvca_util.String.Map.t
  -> ('info, 'prim) term
  -> ('info, 'prim) term

val select_path
  :  path:int list
  -> ('info, 'prim) term
  -> (('info, 'prim) term, string) Result.t

val match_pattern
  :  prim_eq:('prim -> 'prim -> bool)
  -> ('info, 'prim) Pattern.t
  -> ('info, 'prim) term
  -> ('info, 'prim) term Lvca_util.String.Map.t option

val free_vars : (_, _) term -> Lvca_util.String.Set.t

(** Check that the given term matches the given sort.

    This recursively checks subterms and patterns.

    Checks performed:

    + All used variables must be bound.
    + Variables must have the correct sort at their use site.
    + Primitives must have the correct sort (string / integer).
    + All mentioned operators must appear in the relevant sort.
    + All operators must have the correct number of subterms.
    + Variable-arity terms can have only non-binding terms as children
    + Fixed-valence terms must have the correct number of binders. All must be variables.
    + Variable-valence terms must have one binder, a pattern. *)
val check
  :  'prim Fmt.t
  -> ('info -> 'prim -> 'info Sort.t -> string option) (** Primitive checker *)
  -> 'info AbstractSyntax.t (** Abstract syntax *)
  -> 'info Sort.t (** Sort to check term against *)
  -> ('info, 'prim) term
  -> ('info, (('info, 'prim) Pattern.t, ('info, 'prim) term) Base.Either.t) CheckFailure.t
     option

module Parse (Comment : ParseUtil.Comment_int) : sig
  val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) term ParseUtil.t
  val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) term ParseUtil.t
end

module Properties : sig
  val json_round_trip1 : (unit, Primitive.t) term -> PropertyResult.t
  val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
  val string_round_trip1 : (unit, Primitive.t) term -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end

module Primitive : sig
  (** Hardcoded for the Primitive type *)
  val check
    :  'info AbstractSyntax.t (** Abstract syntax *)
    -> 'info Sort.t (** Sort to check term against *)
    -> ('info, Primitive.t) term
    -> ( 'info
       , (('info, Primitive.t) Pattern.t, ('info, Primitive.t) term) Base.Either.t )
       CheckFailure.t
       option
end

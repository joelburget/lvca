(** Representation of terms that simply uses variable names to represent scope. *)

type ('loc, 'prim) term =
  | Operator of 'loc * string * ('loc, 'prim) scope list
  | Var of 'loc * string
  | Primitive of 'loc * 'prim

and ('loc, 'prim) scope =
  | Scope of ('loc, 'prim) Pattern.t list * ('loc, 'prim) term list

val location : ('loc, _) term -> 'loc
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

val map_loc : f:('a -> 'b) -> ('a, 'prim) term -> ('b, 'prim) term
val erase : (_, 'prim) term -> (unit, 'prim) term
val erase_scope : (_, 'prim) scope -> (unit, 'prim) scope

(** Indicates that this scope was encountered when attempting to convert to a pattern. *)

(** Attempt to convert a non-binding term to a pattern.

    For example, the term [add(lit(1); a)] is convertible to a pattern, but [lambda(a. a)]
    is not. *)
val to_pattern
  :  ('loc, 'prim) term
  -> (('loc, 'prim) Pattern.t, (unit, 'prim) scope) Result.t

(** Convert from a pattern to the corresponding term. This always succeeds.

    For example [add(lit(1)); a)] (as a pattern) can be converted to a term. *)
val pattern_to_term : ('loc, 'prim) Pattern.t -> ('loc, 'prim) term

(** Substitute all the variables in the context.

    Leaves variables not found in the context free. *)
val subst_all
  :  ('loc, 'prim) term Lvca_util.String.Map.t
  -> ('loc, 'prim) term
  -> ('loc, 'prim) term

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

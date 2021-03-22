(** Representation of terms that simply uses variable names to represent scope. *)

module rec Term : sig
  type ('info, 'prim) t =
    | Operator of 'info * string * ('info, 'prim) Scope.t list
    | Var of 'info * string
    | Primitive of 'info * 'prim

  val equal
    :  ('info -> 'info -> bool)
    -> ('prim -> 'prim -> bool)
    -> ('info, 'prim) t
    -> ('info, 'prim) t
    -> bool

  val info : ('info, _) t -> 'info

  val pp_generic
    :  open_loc:'info Fmt.t
    -> close_loc:'info Fmt.t
    -> pp_pat:('prim Fmt.t -> ('info, 'prim) Pattern.t Fmt.t)
    -> pp_prim:'prim Fmt.t
    -> ('info, 'prim) t Fmt.t

  val pp : 'prim Fmt.t -> (_, 'prim) t Fmt.t
  val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
  val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t

  (* TODO: remove *)
  val pp_str : 'prim Fmt.t -> (_, 'prim) t -> string
  val jsonify : 'prim Lvca_util.Json.serializer -> (_, 'prim) t Lvca_util.Json.serializer

  val unjsonify
    :  'prim Lvca_util.Json.deserializer
    -> (unit, 'prim) t Lvca_util.Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : 'prim Lvca_util.Json.serializer -> (_, 'prim) t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : 'prim Lvca_util.Json.deserializer -> Bytes.t -> (unit, 'prim) t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : 'prim Lvca_util.Json.serializer -> (_, 'prim) t -> string

  val map_info : f:('a -> 'b) -> ('a, 'prim) t -> ('b, 'prim) t
  val erase : (_, 'prim) t -> (unit, 'prim) t

  (** Attempt to convert a non-binding term to a pattern.

      For example, the term [add(lit(1); a)] is convertible to a pattern, but
      [lambda(a. a)] is not. *)
  val to_pattern
    :  ('info, 'prim) t
    -> (('info, 'prim) Pattern.t, (unit, 'prim) Scope.t) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

      For example [add(lit(1)); a)] (as a pattern) can be converted to a term. *)
  val of_pattern : ('info, 'prim) Pattern.t -> ('info, 'prim) t

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all
    :  ('info, 'prim) t Lvca_util.String.Map.t
    -> ('info, 'prim) t
    -> ('info, 'prim) t

  val select_path
    :  path:int list
    -> ('info, 'prim) t
    -> (('info, 'prim) t, string) Result.t

  val match_pattern
    :  prim_eq:('prim -> 'prim -> bool)
    -> ('info, 'prim) Pattern.t
    -> ('info, 'prim) t
    -> ('info, 'prim) t Lvca_util.String.Map.t option

  val free_vars : (_, _) t -> Lvca_util.String.Set.t

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
    :  'prim Fmt.t
    -> ('info -> 'prim -> 'info Sort.t -> string option) (** Primitive checker *)
    -> 'info AbstractSyntax.t (** Abstract syntax *)
    -> 'info Sort.t (** Sort to check term against *)
    -> ('info, 'prim) t
    -> ('info, (('info, 'prim) Pattern.t, ('info, 'prim) t) Base.Either.t) CheckFailure.t
       option

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
    val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  end

  module Properties : sig
    val json_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
    val string_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val string_round_trip2 : string -> PropertyResult.t
  end

  module Primitive : sig
    (** Hardcoded for the Primitive type *)
    val check
      :  'info AbstractSyntax.t (** Abstract syntax *)
      -> 'info Sort.t (** Sort to check term against *)
      -> ('info, Primitive.t) t
      -> ( 'info
         , (('info, Primitive.t) Pattern.t, ('info, Primitive.t) t) Base.Either.t )
         CheckFailure.t
         option
  end
end

and Scope : sig
  type ('info, 'prim) t = Scope of ('info, 'prim) Pattern.t list * ('info, 'prim) Term.t

  val equal
    :  ('info -> 'info -> bool)
    -> ('prim -> 'prim -> bool)
    -> ('info, 'prim) t
    -> ('info, 'prim) t
    -> bool

  val pp_generic
    :  open_loc:'info Fmt.t
    -> close_loc:'info Fmt.t
    -> pp_pat:('prim Fmt.t -> ('info, 'prim) Pattern.t Fmt.t)
    -> pp_prim:'prim Fmt.t
    -> ('info, 'prim) t Fmt.t

  val pp : 'prim Fmt.t -> (_, 'prim) t Fmt.t
  val pp_range : 'prim Fmt.t -> (OptRange.t, 'prim) t Fmt.t
  val pp_ranges : 'prim Fmt.t -> (SourceRanges.t, 'prim) t Fmt.t
  val pp_str : 'prim Fmt.t -> (_, 'prim) t -> string
  val jsonify : 'prim Lvca_util.Json.serializer -> (_, 'prim) t Lvca_util.Json.serializer

  val unjsonify
    :  'prim Lvca_util.Json.deserializer
    -> (unit, 'prim) t Lvca_util.Json.deserializer

  val map_info : f:('a -> 'b) -> ('a, 'prim) t -> ('b, 'prim) t
  val erase : (_, 'prim) t -> (unit, 'prim) t

  (* TODO?
val to_pattern
  :  ('info, 'prim) t
  -> (('info, 'prim) Pattern.t, (unit, 'prim) scope) Result.t

val of_pattern : ('info, 'prim) Pattern.t -> ('info, 'prim) t
  *)

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all
    :  ('info, 'prim) Term.t Lvca_util.String.Map.t
    -> ('info, 'prim) t
    -> ('info, 'prim) t

  (* TODO:
  val free_vars : (_, _) t -> Lvca_util.String.Set.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
    val whitespace_t : 'prim ParseUtil.t -> (OptRange.t, 'prim) t ParseUtil.t
  end

  module Properties : sig
    val json_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
    val string_round_trip1 : (unit, Primitive.t) t -> PropertyResult.t
    val string_round_trip2 : string -> PropertyResult.t
  end
  *)
end

(** Representation of terms that simply uses variable names to represent scope. *)
open Lvca_util

module Types : sig
  type 'info term =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Primitive.t

  and 'info scope = Scope of 'info Pattern.t list * 'info term
end

module Plain : sig
  type term =
    | Operator of string * scope list
    | Var of string
    | Primitive of Primitive.Plain.t

  and scope = Scope of Pattern.Plain.t list * term
end

module Term : sig
  type 'info t = 'info Types.term =
    | Operator of 'info * string * 'info Types.scope list
    | Var of 'info * string
    | Primitive of 'info Primitive.t

  module Plain : sig
    type t = Plain.term =
      | Operator of string * Plain.scope list
      | Var of string
      | Primitive of Primitive.Plain.t
  end

  val of_plain : Plain.t -> unit t
  val to_plain : _ t -> Plain.t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val info : 'info t -> 'info
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
  val pp : _ t Fmt.t
  val pp_range : Lvca_provenance.Opt_range.t t Fmt.t
  val pp_ranges : Lvca_provenance.Source_ranges.t t Fmt.t

  (* TODO: remove *)
  val pp_str : _ t -> string
  val jsonify : _ t Json.serializer
  val unjsonify : unit t Json.deserializer

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
  val to_pattern : 'info t -> ('info Pattern.t, unit Types.scope) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

      For example [add(lit(1)); a)] (as a pattern) can be converted to a term. *)
  val of_pattern : 'info Pattern.t -> 'info t

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all : 'info t String.Map.t -> 'info t -> 'info t

  val select_path : path:int list -> 'info t -> ('info t, string) Result.t

  val match_pattern
    :  info_eq:('info -> 'info -> bool)
    -> 'info Pattern.t
    -> 'info t
    -> 'info t String.Map.t option

  val free_vars : _ t -> String.Set.t

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
    :  'info Abstract_syntax.t (** Abstract syntax *)
    -> 'info Sort.t (** Sort to check term against *)
    -> 'info t
    -> ('info, ('info Pattern.t, 'info t) Base.Either.t) Check_failure.t option

  module Parse : sig
    val t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
    val whitespace_t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
  end

  module Properties : sig
    include Properties_intf.Parse_pretty_s with type 'info t := 'info t
    include Properties_intf.Json_s with type 'info t := 'info t
  end
end

module Scope : sig
  type 'info t = 'info Types.scope = Scope of 'info Pattern.t list * 'info Types.term

  module Plain : sig
    type t = Plain.scope = Scope of Pattern.Plain.t list * Plain.term
  end

  val of_plain : Plain.t -> unit t
  val to_plain : _ t -> Plain.t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t
  val pp : _ t Fmt.t
  val pp_range : Lvca_provenance.Opt_range.t t Fmt.t
  val pp_ranges : Lvca_provenance.Source_ranges.t t Fmt.t
  val pp_str : _ t -> string
  val jsonify : _ t Json.serializer
  val unjsonify : unit t Json.deserializer
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val erase : _ t -> unit t

  (* TODO?
val to_pattern
  :  ('info, 'prim) t
  -> (('info, 'prim) Pattern.t (unit, 'prim) scope) Result.t

val of_pattern : ('info, 'prim) Pattern.t -> ('info, 'prim) t
  *)

  (** Substitute all the variables in the context.

      Leaves variables not found in the context free. *)
  val subst_all : 'info Types.term String.Map.t -> 'info t -> 'info t

  (* TODO:
  val free_vars : (_, _) t -> String.Set.t

  module Parse  : sig
    val t : 'prim Lvca_parsing.t -> Lvca_provenance.Opt_range.t t Lvca_parsing.t
    val whitespace_t : 'prim Lvca_parsing.t -> Lvca_provenance.Opt_range.t t Lvca_parsing.t
  end

  module Properties : Properties_intf.S with type 'info t := unit t
  *)
end

module type Convertible_s = sig
  include Language_object_intf.S

  val to_nominal : 'info t -> 'info Term.t
  val of_nominal : 'info Term.t -> ('info t, 'info Term.t) Result.t
end

module type Extended_term_s = sig
  include Language_object_intf.Extended_s

  (* TODO: to_pattern, of_pattern *)

  val select_path
    :  path:int list
    -> 'info t
    -> ('info t, (string, 'info Term.t) Base.Either.t) Result.t

  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : _ t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : _ t -> string
end

module Extend_term (Object : Convertible_s) :
  Extended_term_s with type 'info t = 'info Object.t

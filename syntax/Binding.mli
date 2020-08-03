(** This module contains two equivalent representations for terms: de Bruijn and Nominal.

 The two representations each have their strong points.

 de Bruijn indices are especially convenient for deciding alpha-equivalence and renaming
 variables.

 Nominal terms are simpler both for parsing and pretty printing.
 *)

module rec DeBruijn : sig
  (** Representation of terms that uses de Bruijn indices to represent scope. *)

  type 'loc term =
    | Operator of 'loc * string * 'loc scope list
    | Var of 'loc * int * int
    | Primitive of 'loc * Primitive.t

  and 'loc scope = Scope of 'loc Pattern.t list * 'loc term list

  val to_nominal : 'loc term -> 'loc Nominal.term option
  val from_nominal : 'loc Nominal.term -> ('loc term, string) Result.t

  val from_nominal_with_bindings
    :  (int * int) Lvca_util.String.Map.t
    -> 'loc Nominal.term
    -> ('loc term, string) Result.t

  (** Are the two terms equivalent up to variable renaming? *)
  val alpha_equivalent : 'loc term -> 'b term -> bool

  (** Open a scope, substituting a term for each variable bound by this scope. *)
  (* val open_scope : scope -> term list -> (term, string) Result.t *)
end
and Nominal : sig
  (** Representation of terms that simply uses variable names to represent scope. *)

  type 'loc term =
    | Operator of 'loc * string * 'loc scope list
    | Var of 'loc * string
    | Primitive of 'loc * Primitive.t

  and 'loc scope = Scope of 'loc Pattern.t list * 'loc term list

  val pp_term : Format.formatter -> 'loc Nominal.term -> unit
  val pp_term_range : Format.formatter -> Range.t Nominal.term -> unit
  val pp_term_str : 'loc Nominal.term -> string

  val pp_scope : Format.formatter -> 'loc Nominal.scope -> unit
  val pp_scope_range : Format.formatter -> Range.t Nominal.scope -> unit
  val pp_scope_str : 'loc Nominal.scope -> string

  val jsonify : unit Nominal.term -> Lvca_util.Json.t
  val unjsonify : Lvca_util.Json.t -> unit Nominal.term option

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : unit Nominal.term -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit term option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
   terms. *)
  val hash : unit Nominal.term -> string

  val erase : 'loc term -> unit term
  val erase_scope : 'loc scope -> unit scope

  (** Indicates that this scope was encountered when attempting to convert to a pattern.
   *)
  exception ToPatternFailure of unit scope

  (** Attempt to convert a non-binding term to a pattern.

   For example, the term [add(lit(1); a)] is convertible to a pattern, but [lambda(a. a)]
   is not.

   @raise ToPatternScopeEncountered
  *)
  val to_pattern_exn : 'loc Nominal.term -> 'loc Pattern.t
  val to_pattern : 'loc Nominal.term -> ('loc Pattern.t, unit scope) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

    For example [add(lit(1)); a)] (as a pattern) can be converted to a term.
   *)
  val pattern_to_term : 'loc Pattern.t -> 'loc Nominal.term

  module Parse (Comment : Lvca_util.Angstrom.Comment_int) : sig
    val t : Range.t term Angstrom.t
  end
end

module Properties : sig
  val json_round_trip1 : unit Nominal.term -> bool
  val json_round_trip2 : Lvca_util.Json.t -> bool
  val string_round_trip1 : unit Nominal.term -> bool
  val string_round_trip2 : string -> bool
end

(** This module contains two equivalent representations for terms: de Bruijn and Nominal.

 The two representations each have their strong points.

 de Bruijn indices are especially convenient for deciding alpha-equivalence and renaming
 variables.

 Nominal terms are simpler both for parsing and pretty printing.
 *)

module rec DeBruijn : sig
  (** Representation of terms that uses de Bruijn indices to represent scope. *)

  type 'a term =
    | Operator of 'a * string * 'a scope list
    | Var of 'a * int * int
    | Primitive of 'a * Primitive.t

  and 'a scope = Scope of 'a * 'a Pattern.t list * 'a term list

  val to_nominal : 'a term -> 'a Nominal.term option
  val from_nominal : 'a Nominal.term -> ('a term, string) Result.t

  val from_nominal_with_bindings
    :  (int * int) Lvca_util.String.Map.t
    -> 'a Nominal.term
    -> ('a term, string) Result.t

  (** Are the two terms equivalent up to variable renaming? *)
  val alpha_equivalent : 'a term -> 'b term -> bool

  (** Open a scope, substituting a term for each variable bound by this scope. *)
  (* val open_scope : scope -> term list -> (term, string) Result.t *)
end
and Nominal : sig
  (** Representation of terms that simply uses variable names to represent scope. *)

  type 'a term =
    | Operator of 'a * string * 'a scope list
    | Var of 'a * string
    | Primitive of 'a * Primitive.t

  and 'a scope = Scope of 'a * 'a Pattern.t list * 'a term list

  val pp_term : Format.formatter -> 'a Nominal.term -> unit
  val pp_term_range : Format.formatter -> Range.t Nominal.term -> unit
  val pp_term_str : 'a Nominal.term -> string

  val pp_scope : Format.formatter -> 'a Nominal.scope -> unit
  val pp_scope_range : Format.formatter -> Range.t Nominal.scope -> unit
  val pp_scope_str : 'a Nominal.scope -> string

  val jsonify : unit Nominal.term -> Lvca_util.Json.t
  val unjsonify : Lvca_util.Json.t -> unit Nominal.term option

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : unit Nominal.term -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit term option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
   terms. *)
  val hash : unit Nominal.term -> string

  val erase : 'a term -> unit term
  val erase_scope : 'a scope -> unit scope

  (** Indicates that this scope was encountered when attempting to convert to a pattern.
   *)
  exception ToPatternFailure of unit scope

  (** Attempt to convert a non-binding term to a pattern.

   For example, the term [add(lit(1); a)] is convertible to a pattern, but [lambda(a. a)]
   is not.

   @raise ToPatternScopeEncountered
  *)
  val to_pattern_exn : 'a Nominal.term -> 'a Pattern.t
  val to_pattern : 'a Nominal.term -> ('a Pattern.t, unit scope) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

    For example [add(lit(1)); a)] (as a pattern) can be converted to a term.
   *)
  val pattern_to_term : 'a Pattern.t -> 'a Nominal.term

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

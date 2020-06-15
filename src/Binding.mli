(** This module contains two equivalent representations for terms: de Bruijn and Nominal.

 The two representations each have their strong points.

 de Bruijn indices are especially convenient for deciding alpha-equivalence and renaming
 variables.

 Nominal terms are simpler both for parsing and pretty printing.

 TODO: justify citing a Head-to-Head comparison
 *)

module rec DeBruijn : sig
  (** Representation of terms that uses de Bruijn indices to represent scope. *)

  type term =
    | Operator of string * scope list
    | Var of int * int
    | Primitive of Primitive.t

  and scope = Scope of Pattern.t list * term

  val to_nominal : term -> Nominal.term option
  val from_nominal : Nominal.term -> (term, string) Result.t

  val from_nominal_with_bindings
    :  (int * int) Util.String.Map.t
    -> Nominal.term
    -> (term, string) Result.t

  (** Are the two terms equivalent up to variable renaming? *)
  val alpha_equivalent : term -> term -> bool

  (** Open a scope, substituting a term for each variable bound by this scope. *)
  (* val open_scope : scope -> term list -> (term, string) Result.t *)
end
and Nominal : sig
  (** Representation of terms that simply uses variable names to represent scope. *)

  type term =
    | Operator of string * scope list
    | Var of string
    | Primitive of Primitive.t

  and scope = Scope of Pattern.t list * term

  val pp_term : Format.formatter -> Nominal.term -> unit
  val pp_term_str : Nominal.term -> string

  val pp_scope : Format.formatter -> Nominal.scope -> unit
  val pp_scope_str : Nominal.scope -> string

  val jsonify : Nominal.term -> Util.Json.t
  val unjsonify : Util.Json.t -> Nominal.term option

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : Nominal.term -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> term option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
   terms. *)
  val hash : Nominal.term -> string

  (** Indicates that this scope was encountered when attempting to convert to a pattern.
   *)
  exception ToPatternScopeEncountered of scope

  (** Attempt to convert a non-binding term to a pattern.

   For example, the term [add(lit(1); a)] is convertible to a pattern, but [lambda(a. a)]
   is not.

   @raise ToPatternScopeEncountered
  *)
  val to_pattern_exn : Nominal.term -> Pattern.t
  val to_pattern : Nominal.term -> (Pattern.t, scope) Result.t

  (** Convert from a pattern to the corresponding term. This always succeeds.

    For example [add(lit(1)); a)] (as a pattern) can be converted to a term.
   *)
  val pattern_to_term : Pattern.t -> Nominal.term

  module Parse (Lex : Util.Angstrom.Lexical_int) : sig
    val t : term Angstrom.t
  end
end

module Properties : sig
  val json_round_trip1 : Nominal.term -> bool
  val json_round_trip2 : Util.Json.t -> bool
  val string_round_trip1 : Nominal.term -> bool
  val string_round_trip2 : string -> bool
end

module type All_term_s = Language_object_intf.S

(* TODO: we could generalize Nonbinding_term_s and Binding_term_s by making them a
   functor taking the term type and changing the names to to_term and of_term.
   Or returning an Either.
   *)
module type Nonbinding_term_s = sig
  include Language_object_intf.S

  val of_nonbinding : 'info Nonbinding.term -> ('info t, 'info Nonbinding.term) Result.t
  val to_nonbinding : 'info t -> 'info Nonbinding.term
end

module type Binding_term_s = sig
  include Language_object_intf.S

  val to_nominal : 'info t -> 'info Nominal.Term.t
  val of_nominal : 'info Nominal.Term.t -> ('info t, 'info Nominal.Term.t) Result.t
end

module type Extended_term_s = sig
  include All_term_s

  val erase : _ t -> unit t
  val pp : _ t Fmt.t
  val to_string : _ t -> string

  (* TODO: to_pattern, of_pattern *)

  val select_path
    :  path:int list
    -> 'info t
    -> ('info t, (string, 'info Nominal.Term.t) Base.Either.t) Result.t

  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : _ t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : _ t -> string

  module Parse : sig
    val whitespace_t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
  end
end

module Mk (Object : Binding_term_s) : Extended_term_s with type 'info t = 'info Object.t

module type Properties = Properties_intf.S

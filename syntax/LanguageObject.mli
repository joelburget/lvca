module type AllTermS = LanguageObject_intf.S

(* TODO: we could generalize NonBindingTermS and BindingTermS by making them a
   functor taking the term type and changing the names to to_term and of_term.
   Or returning an Either.
   *)
module type NonBindingTermS = sig
  include LanguageObject_intf.S

  val of_nonbinding : 'info NonBinding.term -> ('info t, 'info NonBinding.term) Result.t
  val to_nonbinding : 'info t -> 'info NonBinding.term
end

module type BindingTermS = sig
  include LanguageObject_intf.S

  val to_nominal : 'info t -> 'info Nominal.Term.t
  val of_nominal : 'info Nominal.Term.t -> ('info t, 'info Nominal.Term.t) Result.t
end

module type ExtendedTermS = sig
  type 'a t

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

  module Parse (Comment : ParseUtil_intf.Comment_s) : sig
    val whitespace_t : Lvca_provenance.OptRange.t t ParseUtil.t
  end
end

module Mk (Object : BindingTermS) : ExtendedTermS with type 'info t = 'info Object.t

module type Properties = Properties_intf.S

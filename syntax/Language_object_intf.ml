open Lvca_util

module type Has_info = sig
  type t

  val info : t -> Provenance.t
end

module type Json_convertible = sig
  type t

  val jsonify : t Json.serializer
  val unjsonify : t Json.deserializer
end

module type Serializable = sig
  type t

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : t -> string
end

(** A signature all language objects satisfy. *)
module type S = sig
  (** {1 Data type with attached info} *)
  type t

  (** {1 Info} *)
  include Has_info with type t := t
end

open Lvca_util

module type Plain_convertible = sig
  type 'info t

  module Plain : sig
    type t
  end

  val to_plain : _ t -> Plain.t
  val of_plain : Plain.t -> unit t
end

module type Has_info = sig
  type 'info t

  val info : 'info t -> 'info
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
end

module type Json_convertible = sig
  type 'info t

  val jsonify : 'info t Json.serializer
  val unjsonify : unit t Json.deserializer
end

module type Serializable = sig
  type 'info t

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : _ t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : _ t -> string
end

(** A signature all language objects satisfy. *)
module type S = sig
  type 'info t

  (** {1 Plain data} *)
  include Plain_convertible with type 'info t := 'info t

  (** {1 Info} *)
  include Has_info with type 'info t := 'info t
end

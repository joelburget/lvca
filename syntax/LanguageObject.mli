module type AllTermS = sig
  type 'info t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val t : OptRange.t t ParseUtil.t
  end
end

(* TODO: we could generalize NonBindingTermS and BindingTermS by making them a
   functor taking the term type and changing the names to to_term and of_term.
   Or returning an Either.
   *)
module type NonBindingTermS = sig
  include AllTermS

  val to_nonbinding : 'info t -> ('info, Lvca_util.Void.t) NonBinding.term

  val of_nonbinding
    :  ('info, 'prim) NonBinding.term
    -> ('info t, ('info, 'prim) NonBinding.term) Result.t
end

module type BindingTermS = sig
  include AllTermS

  val to_nominal : 'info t -> ('info, Lvca_util.Void.t) Nominal.Term.t

  val of_nominal
    :  ('info, 'prim) Nominal.Term.t
    -> ('info t, ('info, 'prim) Nominal.Term.t) Result.t
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
    -> ( 'info t
       , (string, ('info, Lvca_util.Void.t) Nominal.Term.t) Base.Either.t )
       Result.t

  val jsonify : _ t Lvca_util.Json.serializer
  val unjsonify : unit t Lvca_util.Json.deserializer

  (** Encode (using {{:https://cbor.io} CBOR}) as bytes. *)
  val serialize : _ t -> Bytes.t

  (** Decode from {{:https://cbor.io} CBOR}). *)
  val deserialize : Bytes.t -> unit t option

  (** The SHA-256 hash of the serialized term. This is useful for content-identifying
      terms. *)
  val hash : _ t -> string

  module Parse (Comment : ParseUtil.Comment_int) : sig
    val whitespace_t : OptRange.t t ParseUtil.t
  end
end

module Mk (Object : BindingTermS) : ExtendedTermS with type 'info t = 'info Object.t

module type Properties = sig
  type 'info t

  val json_round_trip1 : unit t -> PropertyResult.t
  val json_round_trip2 : Lvca_util.Json.t -> PropertyResult.t
  val string_round_trip1 : unit t -> PropertyResult.t
  val string_round_trip2 : string -> PropertyResult.t
end

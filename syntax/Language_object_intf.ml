open Lvca_util

(** A signature all language objects satisfy. *)
module type S = sig
  type 'info t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool

  (** {1 Plain data} *)
  module Plain : sig
    type t
  end

  val to_plain : _ t -> Plain.t
  val of_plain : Plain.t -> unit t

  (** {1 Info} *)
  val info : 'info t -> 'info

  val map_info : f:('a -> 'b) -> 'a t -> 'b t

  (** {1 Serialization} *)

  val jsonify : 'info t Json.serializer
  val unjsonify : unit t Json.deserializer

  (** {1 Printing / Parsing} *)
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

  module Parse : sig
    val t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
  end
end

(** Helpers derivable from [S] *)
module type Extended_s = sig
  include S

  val erase : _ t -> unit t
  val pp : _ t Fmt.t
  val to_string : _ t -> string

  module Parse : sig
    val t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
    val whitespace_t : Lvca_provenance.Opt_range.t t Lvca_parsing.t
  end
end

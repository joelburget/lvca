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

(** A signature all language objects satisfy. *)
module type S = sig
  type 'info t

  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  (* TODO: should they be comparable as well? *)

  (** {1 Plain data} *)
  include Plain_convertible with type 'info t := 'info t

  (** {1 Info} *)
  include Has_info with type 'info t := 'info t

  (** {1 Serialization} *)
  include Json_convertible with type 'info t := 'info t

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

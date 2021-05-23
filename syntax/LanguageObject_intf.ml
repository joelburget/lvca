module type S = sig
  type 'info t

  module Plain : sig
    type t
  end

  val info : 'info t -> 'info
  val to_plain : _ t -> Plain.t
  val of_plain : Plain.t -> unit t
  val equal : info_eq:('info -> 'info -> bool) -> 'info t -> 'info t -> bool
  val map_info : f:('a -> 'b) -> 'a t -> 'b t
  val pp_generic : open_loc:'info Fmt.t -> close_loc:'info Fmt.t -> 'info t Fmt.t

  module Parse (Comment : ParseUtil_intf.Comment_s) : sig
    val t : Lvca_provenance.OptRange.t t ParseUtil.t
  end
end

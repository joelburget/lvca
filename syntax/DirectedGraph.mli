module Int : sig
  type t = int list list

  val connected_components : t -> (* Lvca_util.Int.Set.t list *) int list
end

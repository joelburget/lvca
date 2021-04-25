module Int : sig
  type t = int list list
  type numbered_components = int list
  type component_sets = Lvca_util.Int.Set.t list

  val numbered_connected_components : t -> numbered_components
  val connected_component_sets : t -> component_sets
end

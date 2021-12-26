include [%lvca.abstract_syntax_module_sig "option a := None() | Some(a);"]

val of_option : 'a option -> 'a Option.t
val to_option : 'a Option.t -> 'a option
val map : f:('a -> 'b) -> 'a Option.t -> 'b Option.t

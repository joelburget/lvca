include [%lvca.abstract_syntax_module_sig "list a := Nil() | Cons(a; list a)"]

val of_list : 'a list -> 'a List.t
val to_list : 'a List.t -> 'a list
val map : f:('a -> 'b) -> 'a List.t -> 'b List.t
val extract_vars_from_empty_pattern : Lvca_syntax.Pattern.t -> string list
val make_empty_pattern : (Lvca_syntax.Provenance.t * string) list -> Lvca_syntax.Pattern.t

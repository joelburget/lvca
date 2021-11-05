include [%lvca.abstract_syntax_module_sig "list a := Nil() | Cons(a; list a)"]

val of_list : 'a list -> 'a List.t
val to_list : 'a List.t -> 'a list
val map : f:('a -> 'b) -> 'a List.t -> 'b List.t

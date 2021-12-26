include [%lvca.abstract_syntax_module_sig "empty := ;"]

type t = Empty.t

val pp : t Fmt.t
val parse : t Lvca_parsing.t

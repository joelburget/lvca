type grammar
type parser

val to_grammar : Types.ConcreteSyntaxDescription.t -> grammar
val to_parser  : grammar -> parser
val parse      : parser -> string -> ConcreteSyntax.tree

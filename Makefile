src/ConcreteSyntax_Parser.messages: src/ConcreteSyntax_Parser.mly
	menhir src/ConcreteSyntax_Parser.mly --update-errors src/ConcreteSyntax_Parser.messages

src/Term_Parser.messages: src/Term_Parser.mly
	menhir src/Term_Parser.mly --update-errors src/Term_Parser.messages

src/AbstractSyntax_Parser.messages: src/AbstractSyntax_Parser.mly
	menhir src/AbstractSyntax_Parser.mly --update-errors src/AbstractSyntax_Parser.messages

src/Statics_Parser.messages: src/Statics_Parser.mly
	menhir src/Statics_Parser.mly src/ParserLib.mly --base Statics_Parser --update-errors src/Statics_Parser.messages

src/Dynamics_Parser.messages: src/Dynamics_Parser.mly
	menhir src/Dynamics_Parser.mly src/ParserLib.mly --base Dynamics_Parser --update-errors src/Dynamics_Parser.messages

src/Regex_Parser.messages: src/Regex_Parser.mly
	menhir src/Regex_Parser.mly --update-errors src/Regex_Parser.messages



src/ConcreteSyntax_ParseErrors.ml: src/ConcreteSyntax_Parser.mly src/ConcreteSyntax_Parser.messages
	menhir src/ConcreteSyntax_Parser.mly --compile-errors src/ConcreteSyntax_Parser.messages > src/ConcreteSyntax_ParseErrors.ml

src/Term_ParseErrors.ml: src/Term_Parser.mly src/Term_Parser.messages
	menhir src/Term_Parser.mly --compile-errors src/Term_Parser.messages > src/Term_ParseErrors.ml

src/AbstractSyntax_ParseErrors.ml: src/AbstractSyntax_Parser.mly src/AbstractSyntax_Parser.messages
	menhir src/AbstractSyntax_Parser.mly --compile-errors src/AbstractSyntax_Parser.messages > src/AbstractSyntax_ParseErrors.ml

src/Statics_ParseErrors.ml: src/Statics_Parser.mly src/Statics_Parser.messages
	menhir src/Statics_Parser.mly src/ParserLib.mly --base Statics_Parser --compile-errors src/Statics_Parser.messages > src/Statics_ParseErrors.ml

src/Dynamics_ParseErrors.ml: src/Dynamics_Parser.mly src/Dynamics_Parser.messages
	menhir src/Dynamics_Parser.mly src/ParserLib.mly --base Dynamics_Parser --compile-errors src/Dynamics_Parser.messages > src/Dynamics_ParseErrors.ml

src/Regex_ParseErrors.ml: src/Regex_Parser.mly src/Regex_Parser.messages
	menhir src/Regex_Parser.mly --compile-errors src/Regex_Parser.messages > src/Regex_ParseErrors.ml

format:
	@fd --extension ml --extension mli | xargs ocamlformat --enable-outside-detected-project --inplace

install-deps:
	opam install base bignum bonsai cbor core_kernel digestif js_of_ocaml ppx_jane re

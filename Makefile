src/ConcreteSyntaxParser.messages: src/ConcreteSyntaxParser.mly
	menhir src/ConcreteSyntaxParser.mly --update-errors src/ConcreteSyntaxParser.messages

src/TermParser.messages: src/TermParser.mly
	menhir src/TermParser.mly --update-errors src/TermParser.messages

src/LanguageParser.messages: src/LanguageParser.mly
	menhir src/LanguageParser.mly --update-errors src/LanguageParser.messages

src/StaticsParser.messages: src/StaticsParser.mly
	menhir src/StaticsParser.mly src/ParserLib.mly --base StaticsParser --update-errors src/StaticsParser.messages

src/DynamicsParser.messages: src/DynamicsParser.mly
	menhir src/DynamicsParser.mly src/ParserLib.mly --base DynamicsParser --update-errors src/DynamicsParser.messages



src/ConcreteSyntaxParseErrors.ml: src/ConcreteSyntaxParser.mly src/ConcreteSyntaxParser.messages
	menhir src/ConcreteSyntaxParser.mly --compile-errors src/ConcreteSyntaxParser.messages > src/ConcreteSyntaxParseErrors.ml

src/TermParseErrors.ml: src/TermParser.mly src/TermParser.messages
	menhir src/TermParser.mly --compile-errors src/TermParser.messages > src/TermParseErrors.ml

src/LanguageParseErrors.ml: src/LanguageParser.mly src/LanguageParser.messages
	menhir src/LanguageParser.mly --compile-errors src/LanguageParser.messages > src/LanguageParseErrors.ml

src/StaticsParseErrors.ml: src/StaticsParser.mly src/StaticsParser.messages
	menhir src/StaticsParser.mly src/ParserLib.mly --base StaticsParser --compile-errors src/StaticsParser.messages > src/StaticsParseErrors.ml

src/DynamicsParseErrors.ml: src/DynamicsParser.mly src/DynamicsParser.messages
	menhir src/DynamicsParser.mly src/ParserLib.mly --base DynamicsParser --compile-errors src/DynamicsParser.messages > src/DynamicsParseErrors.ml

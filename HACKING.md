## How to do Menhir stuff

* Produce a parser: `menhir src/Dynamics_Parser.mly --table`
* Generate .messages file: `menhir src/Dynamics_Parser.mly --list-errors`
* Explain conflicts: `menhir src/Dynamics_Parser.mly --explain`
* Produce _messages.ml file: `menhir src/Dynamics_Parser.mly --compile-errors src/Dynamics_Parser.messages`
* Update errors file: `menhir src/Dynamics_Parser.mly --update-errors src/Dynamics_Parser.messages`

module TermViewer = {
  open Types;
  open Types.Abt;

  let rec show_term(term:term) = switch (term) {
      | Term(name, lst) =>
        ReactDOMRe.createDOMElementVariadic(
          "span",
          ~props=ReactDOMRe.domProps(),
          Array.concat([
            [| React.string(name) |],
            [| React.string("(") |],
            Array.of_list(List.map(show_scope, lst)),
            [| React.string(")") |]
          ])
        )
      | Var(name)     => React.string(name)
      | Sequence(tms) => ReactDOMRe.createDOMElementVariadic(
          "span",
          ~props=ReactDOMRe.domProps(),
          Array.concat([
            [| React.string("[") |],
            Array.of_list(List.map(show_term, tms)),
            [| React.string("]") |]
          ])
        )
      | Primitive(prim) => show_prim(prim)
      }

  and show_scope(scope:scope) = switch (scope) {
    | Scope(names, tm) => ReactDOMRe.createDOMElementVariadic(
      "span",
      ~props=ReactDOMRe.domProps(),
        Array.concat([
          Array.of_list(intersperse_after(List.map(React.string, names), React.string("."))),
          [| show_term(tm) |]
        ])
      )
  }

  and show_prim(prim:primitive) = switch (prim) {
    | PrimInteger(i)  => React.string(Bigint.to_string(i))
    | PrimString(str) => React.string(str)
    | PrimBool(true)  => React.string("true")
    | PrimBool(false) => React.string("false")
  };

  [@react.component]
  let make = (~term:term) => {
    <div>{show_term(term)}</div>
  };
};

module LvcaViewer = {
  type action =
    | Type(string)
    ;

  [@react.component]
  let make = () => {
    open Belt.Result;

    let (termInput,          setTermInput)          = React.useState(() => "");
    let (languageDefinition, setLanguageDefinition) = React.useState(() => "");
    let (staticsDefinition,  setStaticsDefinition)  = React.useState(() => "");
    let (dynamicsDefinition, setDynamicsDefinition) = React.useState(() => "");

    let termResult = switch (TermParser.term(TermLexer.read, Lexing.from_string(termInput))) {
          | term                                 => Ok(term)
          | exception TermLexer.SyntaxError(msg) => Error(msg)
          | exception Parsing.Parse_error        => Error("Parse error")
          };

    let termView = switch (termResult) {
          | Ok(term)   => <TermViewer term=term />
          | Error(msg) => <div className="error"> {React.string(msg)} </div>
          };

    <div>
      <h1>{React.string("LVCA")}</h1>

      <h2>{React.string("Language Definition")}</h2>
      <textarea
        value=languageDefinition
        onChange=(event => setLanguageDefinition(ReactEvent.Form.target(event)##value))
      />

      <h2>{React.string("Statics")}</h2>
      <textarea
        value=staticsDefinition
        onChange=(event => setStaticsDefinition(ReactEvent.Form.target(event)##value))
      />

      <h2>{React.string("Dynamics")}</h2>
      <textarea
        value=dynamicsDefinition
        onChange=(event => setDynamicsDefinition(ReactEvent.Form.target(event)##value))
      />

      <h2>{React.string("Input")}</h2>
      <textarea
        value=termInput
        onChange=(event => setTermInput(ReactEvent.Form.target(event)##value))
      />

      {termView}
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

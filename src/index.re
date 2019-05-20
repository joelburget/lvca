module TermViewer = {
  open Demo;

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
      | Primitive(_prim) => React.string("Primitive")
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
    let (text, setText) = React.useState(() => "");
    let termResult = switch (TermParser.term(TermLexer.read, Lexing.from_string(text))) {
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
      <textarea value=text onChange=(event => setText(ReactEvent.Form.target(event)##value)) />
      {termView}
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

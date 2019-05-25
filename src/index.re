[@bs.module "codemirror/keymap/vim"] external _vimImport : unit = "default";
[@bs.module] external _modeImport : unit = "./lvca-mode";

module CodeMirror = {
  [@bs.deriving abstract]
  type options = {
    [@bs.optional] keyMap: string,
    [@bs.optional] mode: string,
  };

  [@react.component][@bs.module]
  external make:
    (~value: string,
     ~onChange: string => unit,
     ~options: options)
    => React.element
    = "react-codemirror";
};

module Repl = {
  [@react.component]
  let make = (~input:string, ~setInput: string => unit) => {
    let options = CodeMirror.options(/* TODO ~keyMap="vim", */ ~mode="lvca", ());
    <CodeMirror value=input onChange=setInput options=options />
  };
};

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
            Array.of_list(intersperse(List.map(show_scope, lst), React.string(";"))),
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
    | PrimString(str) => React.string("\"" ++ str ++ "\"")
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

    let (termInput,          setTermInput)          = React.useState(() => "foo()");
    let (languageDefinition, setLanguageDefinition) = React.useState(() => LanguageSimple.str);

    let termResult = switch (TermParser.term(TermLexer.read, Lexing.from_string(termInput))) {
          | term                                 => Ok(term)
          | exception LexerUtil.SyntaxError(msg) => Error(msg)
          | exception Parsing.Parse_error        => Error("Parse error")
          | exception _ /* Menhirbasics.Error */ => Error("Parse error")
          };

    let termView = switch (termResult) {
          | Ok(term)   => <TermViewer term=term />
          | Error(msg) => <div className="error"> {React.string(msg)} </div>
          };

    <div className="lvca-viewer">
      <h1 className="header">{React.string("LVCA")}</h1>

      <h2 className="header2 header2-left">{React.string("Language Definition")}</h2>
      <div className="left-pane">
        <CodeMirror
          value=languageDefinition
          onChange=(str => setLanguageDefinition(_ => str))
          options=CodeMirror.options(())
        />
      </div>

      <h2 className="header2 header2-right">{React.string("repl")}</h2>
      <div className="right-pane">
        <Repl input=termInput setInput=(str => setTermInput(_ => str)) />
        {termView}
      </div>
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

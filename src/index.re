[@bs.module "codemirror/keymap/vim"] external _vimImport : unit = "default";
[@bs.module] external _modeImport : unit = "../../../src/lvca-mode";

let show_prim(prim:Types.primitive) = switch (prim) {
  | PrimInteger(i)  => React.string(Bigint.to_string(i))
  | PrimString(str) => React.string("\"" ++ str ++ "\"")
  | PrimBool(true)  => React.string("true")
  | PrimBool(false) => React.string("false")
};

module CodeMirror = {
  [@bs.deriving abstract]
  type options = {
    [@bs.optional] keyMap: string,
    [@bs.optional] mode: string,
  };

  // A CodeMirror editor
  type editor = {.};

  // A React event
  [@bs.deriving abstract]
  type event = {
    key: string,
  };

  [@react.component][@bs.module "react-codemirror2"]
  external make:
    (~value: string,
     ~onChange: (editor, {.}, string) => unit,
     ~options: options,
     ~onKeyDown: (editor, event) => unit =?)
    => React.element
    = "UnControlled";
};

module Repl = {
  [@react.component]
  let make = (~input: string,
              ~history: array(string),
              ~future: array(string),
              ~setInput: string => unit
              ) => {
    let options = CodeMirror.options(~mode="lvca", ());
    let handleKey = (_editor, evt) => {
      if (CodeMirror.keyGet(evt) == "Enter") {
        Js.log("Enter");
      }
    };

    <CodeMirror
      value=input
      onChange=((editor, data, value) => setInput(value))
      onKeyDown=handleKey
      options=options
    />
  };
};

let make_span = children => {
  ReactDOMRe.createDOMElementVariadic(
    "span",
    ~props=ReactDOMRe.domProps(),
    Array.concat(children)
  )
};

module TermViewer = {
  open Util;
  open Types.Abt;

  let rec show_term(term:term) = switch (term) {
      | Term(name, lst) =>
        make_span([
          [| React.string(name) |],
          [| React.string("(") |],
          Array.of_list(intersperse(List.map(show_scope, lst), React.string(";"))),
          [| React.string(")") |]
        ])
      | Var(name)     => React.string(string_of_int(name)) // TODO: convert to ast
      | Sequence(tms) => make_span([
          [| React.string("[") |],
          Array.of_list(List.map(show_term, tms)),
          [| React.string("]") |]
        ])
      | Primitive(prim) => show_prim(prim)
      }

  and show_scope(scope:scope) = switch (scope) {
    | Scope(names, tm) => make_span([
      Array.of_list(intersperse_after(List.map(React.string, names), React.string("."))),
      [| show_term(tm) |]
    ])
  };

  [@react.component]
  let make = (~term:term) => {
    <div>{show_term(term)}</div>
  };
};

module CoreValView = {
  open Util;
  open Types.Core;

  let rec view_core_pat = pat => switch (pat) {
  | PatternTerm(name, pats) =>
    <span>
      {React.string(name ++ "(") /* XXX children */}
      {React.string(")")}
    </span>
  | PatternVar(None) => React.string("_")
  | PatternVar(Some(name)) => React.string(name)
  | PatternLit(lit)     => show_prim(lit)
  | PatternDefault      => React.string("default")
  }

  and view_core = core => switch (core) {
  | CoreVar(name) => React.string(name)
  | CoreVal(core_val) => view_core_val(core_val)
  | CoreApp(f, args) =>
    make_span([
      [| React.string("app(") |],
      [| view_core(f) |],
      [| React.string("; ") |],
      Array.of_list(intersperse(List.map(view_core, args), React.string("; "))),
      [| React.string(")") |],
    ])
  | Lam(args, body) => make_span([
    [| React.string("lam(") |],
    Array.of_list(intersperse(List.map(React.string, args), React.string(". "))),
    [| view_core(body) |],
    [| React.string(")") |],
  ])
  | Case(arg, _ty, _cases) => make_span([
    [| React.string("case(") |],
    [| view_core(arg) |],
    [| React.string("; ...)") |],
  ])
  | Meaning(name) => make_span([
    [| React.string("[[") |],
    [| React.string(name) |],
    [| React.string("]]") |],
  ])
  }

  and view_core_val = coreVal => switch (coreVal) {
  | ValTm(name, children) => make_span([
    [| React.string(name) |],
    Array.of_list(intersperse(List.map(view_core_val, children), React.string("; "))),
  ])
  | ValLit(lit) => show_prim(lit)
  | ValPrimop(name) => React.string(name) /* TODO: take a look at this */
  | ValLam(vars, core) => make_span([
    Array.of_list(intersperse(List.map(React.string, vars), React.string(". "))),
    [| view_core(core) |],
  ])
  };

  [@react.component]
  let make = (~coreVal:core_val) => {
    <div> {view_core_val(coreVal)} </div>
  };
}

module ParseStatus = {
  open Belt.Result;

  module Component = {
    [@react.component]
    let make = (~result) => {
      switch (result) {
      | Ok(_parsed) => <span className="result-good"> {React.string("(good)")} </span>
      | Error(msg)  => <span className="result-bad"> {React.string(msg)} </span>
      }
    };
  };

  let parse = fun(runParse, read, lexbuf) => {
    let result =
      switch (runParse(read, lexbuf)) {
      | parsed                               => Ok(parsed)
      | exception LexerUtil.SyntaxError(msg) => Error(msg)
      /* Parsing.Parse_error / ***Parser.Error */
      | exception _ =>
        let pos = lexbuf.Lexing.lex_curr_p;
        let tok = Lexing.lexeme(lexbuf);
        Error(Printf.sprintf("Parse error (%s) %s:%d:%d", tok,
          pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1))
      };

    (<Component result=result />, result)
  }
}

module LvcaViewer = {
  type action =
    | Type(string)
    ;

  [@react.component]
  let make = () => {
    open Belt.Result;

    let (replHistory, setHistory)
      = React.useState(() => [||]);
    let (replFuture, setFuture)
      = React.useState(() => [||]);

    let (termInput,     setTermInput)
      = React.useState(() => "ite(val(false()); val(false()); val(true()))");

    let (asInput,       setAsInput)
      = React.useState(() => LanguageSimple.abstractSyntax);
    let (staticsInput,  setStaticsInput)
      = React.useState(() => LanguageSimple.statics);
    let (dynamicsInput, setDynamicsInput)
      = React.useState(() => LanguageSimple.dynamics);

    let (languageView, language) = ParseStatus.parse(
      LanguageParser.languageDef,
      LanguageLexer.read,
      Lexing.from_string(asInput));

    let (staticsView, statics) = ParseStatus.parse(
      StaticsParser.rules,
      StaticsLexer.read,
      Lexing.from_string(staticsInput));

    let (dynamicsView, dynamics) = ParseStatus.parse(
      DynamicsParser.dynamics,
      DynamicsLexer.read,
      Lexing.from_string(dynamicsInput)
    );

    let show_term_pane = isOk(language) && isOk(statics) && isOk(dynamics);

    let termResult = switch
      (TermParser.term(TermLexer.read, Lexing.from_string(termInput))) {
    | term                                 =>
      Types.Abt.from_ast(getExn(language), "tm", term) // XXX getExn
    | exception LexerUtil.SyntaxError(msg) => Error(msg)
    | exception Parsing.Parse_error        => Error("Parse error")
    | exception TermParser.Error           => Error("Parse error")
    };

/*     let termView = switch (termResult) { */
/*     | Ok(term)   => <TermViewer term=term /> */
/*     | Error(msg) => <div className="error"> {React.string(msg)} </div> */
/*     }; */

    let evalResult : Types.Core.translation_result(Types.Core.core_val) = {
      open Types.Core;
      switch (dynamics, termResult) {
        | (Ok(dynamics_), Ok(termResult_)) => switch (term_to_core(dynamics_, termResult_)) {
          | Ok(core) => switch (eval(core)) {
            | Ok(core_val) => Ok(core_val)
            | Error(msg)   => Error((msg, Some(termResult_)))
          }
          | Error(msg) => Error(msg)
        }
        | (Error(msg), _)
        | (_, Error(msg)) => Error((msg, None))
      }
    };

    let evalView = switch (evalResult) {
    | Ok(coreVal)
      => <CoreValView coreVal=coreVal />
    | Error((msg, None))
      => <div className="error"> {React.string(msg)} </div>
    | Error((msg, Some(tm)))
      => <div className="error">
           {React.string(msg)}
           <TermViewer term=tm />
         </div>
    };

    let replPane = if (show_term_pane) {
        <div className="repl-pane">
          <div className="term-input">
            <Repl
              input=termInput
              history=replHistory
              future=replFuture
              setInput=(str => setTermInput(_ => str))
            />
          </div>
          <div className="term-view">{evalView}</div>
        </div>
      } else {
        <div className="repl-pane disabled" />
      };

    <div className="lvca-viewer">
      <h1 className="header">{React.string("LVCA")}</h1>

      <h2 className="header2 header2-abstract-syntax">
        {React.string("Abstract Syntax ")}
        {languageView}
      </h2>
      <div className="abstract-syntax-pane">
        <CodeMirror
          value=asInput
          onChange=((_, _, str) => setAsInput(_ => str))
          options=CodeMirror.options(~mode="default", ())
        />
      </div>

      <h2 className="header2 header2-statics">
        {React.string("Statics ")}
        {staticsView}
      </h2>
      <div className="statics-pane">
        <CodeMirror
          value=staticsInput
          onChange=((_, _, str) => setStaticsInput(_ => str))
          options=CodeMirror.options(~mode="default", ())
        />
      </div>

      <h2 className="header2 header2-dynamics">
        {React.string("Dynamics ")}
        {dynamicsView}
      </h2>
      <div className="dynamics-pane">
        <CodeMirror
          value=dynamicsInput
          onChange=((_, _, str) => setDynamicsInput(_ => str))
          options=CodeMirror.options(~mode="default", ())
        />
      </div>

      <h2 className="header2 header2-repl">{React.string("repl")}</h2>
      {replPane}
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

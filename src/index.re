open Util

type item_result = Types.Core.translation_result(Types.Core.core_val);

type history_item = {
  input: string,
  result: item_result,
};

type history = {
  before: list(history_item),
  after: list(history_item),
  input: string
};

let resultForInput = (language, dynamics, input) => {
    open Types.Core;

    let termResult = switch
      (TermParser.term(TermLexer.read, Lexing.from_string(input))) {
    | term
      => Types.Abt.from_ast(language, "tm", term)
    | exception LexerUtil.SyntaxError(msg) => Error(msg)
    | exception Parsing.Parse_error        => Error("Parse error")
    | exception TermParser.Error           => Error("Parse error")
    };

    switch (termResult) {
      | Ok(termResult') => Belt.Result.flatMap(
          term_to_core(dynamics, termResult'),
          core => map_error(eval(core), msg => (msg, Some(termResult')))
        )
      | Error(msg) => Error((msg, None))
    }
};

let shift_from_to (
  shift_from: list(history_item),
  shift_to: list(history_item),
  elem: history_item
  ) : (list(history_item), list(history_item), history_item)
  = switch (shift_from) {
    | [] => (shift_from, shift_to, elem)
    | [elem', ...shift_from'] =>
      let shift_to' = [ elem, ...shift_to ];
      (shift_from', shift_to', elem')
  };


let step_forward (
  language : Types.language,
  dynamics : Types.Core.denotation_chart,
  {input, before, after} : history,
  ) : history
  = {
    let (after', before', elem) =
      shift_from_to(after, before,
                    { input, result: resultForInput(language, dynamics, input) });
    {before: before', after: after', input: elem.input}
  };

let step_back (
  language : Types.language,
  dynamics : Types.Core.denotation_chart,
  {input, before, after} : history,
  ) : history
  = {
    let (before', after', elem) =
      shift_from_to(before, after,
                    { input, result: resultForInput(language, dynamics, input) });
    {before: before', after: after', input: elem.input}
  };

let rec go_back    = (lang, dyn, hist, i) => switch(i) {
  | 0 => hist
  | _ => step_back(lang, dyn, go_back(lang, dyn, hist, i - 1))
};

let rec go_forward = (lang, dyn, hist, i) => switch(i) {
  | 0 => hist
  | _ => step_forward(lang, dyn, go_forward(lang, dyn, hist, i - 1))
}

module Repl = {
  open Types;

  // TODO: move event type somewhere better
  let preventDefault : CodeMirror.event => unit
    = [%bs.raw "evt => evt.preventDefault()"];

  [@react.component]
  let make = (
    ~history: history,
    ~language: language,
    ~statics: list(Statics.rule),
    ~dynamics: Core.denotation_chart,
    ~setInput: string => unit,
    ~handleEnter: unit => unit,
    ~handleUp: int => unit,
    ~handleDown: int => unit,
    ) => {
    let make_div = EvalView.make_div;
    let {before, after, input} = history;
    let options = CodeMirror.options(~mode="lvca", ());

/*     let termView = switch (termResult) { */
/*     | Ok(term)   => <TermViewer term=term /> */
/*     | Error(msg) => <div className="error"> {React.string(msg)} </div> */
/*     }; */

    let evalResult = resultForInput(language, dynamics, input);

    let handleKey = (_editor, evt) => {
      let key = CodeMirror.keyGet(evt);
      let shift = CodeMirror.shiftKeyGet(evt);
      if (shift) {
        if (key == "Enter") {
          preventDefault(evt);
          handleEnter();
        } else if (key == "ArrowUp") {
          preventDefault(evt);
          handleUp(1);
        } else if (key == "ArrowDown") {
          preventDefault(evt);
          handleDown(1);
        }
      }
    };

    let beforeElems = List.rev(List.mapi(
      (i, {input, result}) =>
        <div className="history-item">
          <div
            className="history-input"
            onClick={_evt => handleUp(i + 1)}
          >
            {React.string(input)}
          </div>
          <div className="term-view"><EvalView evalResult=result /></div>
        </div>,
      before
    ));
    let afterElems = List.mapi(
      (i, {input, result}) =>
        <div className="history-item">
          <div
            className="history-input"
            onClick={_evt => handleDown(i + 1)}
          >
            {React.string(input)}
          </div>
          <div className="term-view"><EvalView evalResult=result /></div>
        </div>,
      after
    );

    <>
      {make_div(Array.of_list(beforeElems))}
      <div>
        <div className="term-input">
          <CodeMirror
            value=input
            onBeforeChange=((_editor, _data, value) => setInput(value))
            onKeyDown=handleKey
            options=options
            editorDidMount=CodeMirror.focusEditor
          />
        </div>
        <div className="term-view"><EvalView evalResult=evalResult /></div>
      </div>
      {make_div(Array.of_list(afterElems))}
    </>
  };
};

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

    let initialHistory: history = {
        input: "ite(val(false()); val(false()); val(true()))",
        before: [],
        after: [],
      }
    let (replHistory, setHistory) = React.useState(() => initialHistory);

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

    let replPane = switch (language, statics, dynamics) {
      | (Ok(language), Ok(statics), Ok(dynamics))
      =>
        <div className="repl-pane">
          <Repl
            history=replHistory
            language=language
            statics=statics
            dynamics=dynamics
            setInput=(input => setHistory(hist => {...hist, input}))
            handleEnter=(() => setHistory(({input, before, after} as hist) => {
              switch (after) {
                | [] =>
                  let before' = [
                    { input, result: resultForInput(language, dynamics, input) },
                    ...before
                  ];
                  {before: before', after, input: ""}
                | _ => step_forward(language, dynamics, hist)
              }
            }))
            handleUp=(n =>
              setHistory(hist => go_back(language, dynamics, hist, n)))
            handleDown=(n =>
              setHistory(hist => go_forward(language, dynamics, hist, n)))
          />
        </div>
      | _
      =>
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
          onBeforeChange=((_, _, str) => setAsInput(_ => str))
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
          onBeforeChange=((_, _, str) => setStaticsInput(_ => str))
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
          onBeforeChange=((_, _, str) => setDynamicsInput(_ => str))
          options=CodeMirror.options(~mode="default", ())
        />
      </div>

      <h2 className="header2 header2-repl">{React.string("repl")}</h2>
      {replPane}
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

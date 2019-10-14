open Util
module Result = Belt.Result
module LrTables = LrParsingView.Tables

type parse_result = Core.translation_result(Binding.Nominal.term);
type eval_result  = Core.translation_result(Core.core);

type history_item = {
  input: string,
  parsed: parse_result,
  result: eval_result,
};

type history = {
  before: list(history_item),
  after: list(history_item),
  input: string,
};

/* module Parse_term = Parsing.Incremental(Parsing.Parseable_term) */

/* Read and evaluate the user's input */
let read_eval_input = (language, concrete, statics, dynamics, input)
  : (parse_result, eval_result) => {
    open Core;

    let (astResult, abtResult) = switch (ConcreteSyntax.parse(concrete, input)) {
    | Ok(tree)
    => switch (ConcreteSyntax.to_ast(language, tree)) {
      | Ok(ast)
      => (Result.Ok(ast), Binding.DeBruijn.from_nominal(ast))
      | Error(msg)
      => (Error((msg, None)), Error(msg))
    }
    | Error(msg)
    /* TODO: this is not really an ast error message */
    => (Error((msg, None)), Error(msg))
    | exception Util.InvariantViolation(msg)
    => (Error((msg, None)), Error(msg))
    };

    let eval' = tm => map_error(eval(tm), fun(msg) => (msg, None));

    switch (abtResult) {
      | Ok(abtResult') => switch
        (Belt.Result.flatMap(term_denotation(dynamics, [], abtResult'), eval')) {
        | Ok(core_val) => (astResult, Ok(core_val))
        | Error(msg)   => (astResult, Error(msg)) // Error((msg, Some(abtResult'))))
        }
      | Error(msg) => (Error((msg, None)), Error((msg, None)))
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
  concrete : Types.ConcreteSyntaxDescription.t,
  statics  : list(Statics.rule),
  dynamics : Core.denotation_chart,
  {input, before, after} : history,
  ) : history
  = {
    let (parsed, result) = read_eval_input(language, concrete, statics, dynamics, input);
    let (after, before, elem) =
      shift_from_to(after, before, { input, result, parsed });
    {before, after, input: elem.input}
  };

let step_back (
  language : Types.language,
  concrete : Types.ConcreteSyntaxDescription.t,
  statics  : list(Statics.rule),
  dynamics : Core.denotation_chart,
  {input, before, after} : history,
  ) : history
  = {
    let (parsed, result) = read_eval_input(language, concrete, statics, dynamics, input);
    let (before, after, elem) =
      shift_from_to(before, after, { input, result, parsed });
    {before, after, input: elem.input}
  };

let rec go_back    = (lang, concrete, statics, dyn, hist, i) => switch(i) {
  | 0 => hist
  | _ => step_back(lang, concrete, statics, dyn, go_back(lang, concrete, statics, dyn, hist, i - 1))
};

let rec go_forward = (lang, concrete, statics, dyn, hist, i) => switch(i) {
  | 0 => hist
  | _ => step_forward(lang, concrete, statics, dyn, go_forward(lang, concrete, statics, dyn, hist, i - 1))
}

let make_div = children => {
  ReactDOMRe.createDOMElementVariadic(
    "div",
    ~props=ReactDOMRe.domProps(),
    children
  )
};


module Repl = {
  open Types;

  // TODO: move event type somewhere better
  let preventDefault : CodeMirror.event => unit
    = [%bs.raw "evt => evt.preventDefault()"];

  [@react.component]
  let make = (
    ~history: history,
    ~language: language,
    ~concrete: ConcreteSyntaxDescription.t,
    ~statics: list(Statics.rule),
    ~dynamics: Core.denotation_chart,
    ~setInput: string => unit,
    ~handleEnter: unit => unit,
    ~handleUp: int => unit,
    ~handleDown: int => unit,
    ) => {
    let {before, after, input} = history;
    let options = CodeMirror.options(~mode="lvca", ());

/*     let termView = switch (termResult) { */
/*     | Ok(term)   => <TermViewer term=term /> */
/*     | Error(msg) => <div className="error"> {React.string(msg)} </div> */
/*     }; */

    let (parsed, evalResult) = read_eval_input(language, concrete, statics, dynamics, input);

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
      (i, {input, parsed, result}) =>
        <div className="history-item">
          <div
            className="history-input"
            onClick={_evt => handleUp(i + 1)}
          >
            {React.string(input)}
          </div>
          <div className="term-view">
            <EvalView input=parsed evalResult=result />
          </div>
        </div>,
      before
    ));
    let afterElems = List.mapi(
      (i, {input, parsed, result}) =>
        <div className="history-item">
          <div
            className="history-input"
            onClick={_evt => handleDown(i + 1)}
          >
            {React.string(input)}
          </div>
          <div className="term-view">
            <EvalView input=parsed evalResult=result />
          </div>
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
        <div className="term-view">
          <EvalView input=parsed evalResult=evalResult />
        </div>
      </div>
      {make_div(Array.of_list(afterElems))}
    </>
  };
};

module LvcaViewer = {
  /* type action = */
  /*   | Type(string) */
  /*   ; */

  [@react.component]
  let make = () => {
    open Belt.Result;

    let initialHistory: history = {
        input: "if false then false else true",
        before: [],
        after: [],
      }
    let (replHistory, setHistory) = React.useState(() => initialHistory);

    let (asInput,       setAsInput)
      = React.useState(() => LanguageSimple.abstractSyntax);
    let (concreteInput,  setConcreteInput)
      = React.useState(() => LanguageSimple.concrete);
    let (staticsInput,  setStaticsInput)
      = React.useState(() => LanguageSimple.statics);
    let (dynamicsInput, setDynamicsInput)
      = React.useState(() => LanguageSimple.dynamics);

    let (showGrammarPane, setShowGrammarPane)
      = React.useState(() => false);

    module Parseable_language' = ParseStatus.Make(Parsing.Parseable_language);
    let (languageView, language) = Parseable_language'.parse(asInput);

    module Parseable_concrete = ParseStatus.Make(Parsing.Parseable_concrete_syntax);
    let (concreteView, concrete) = Parseable_concrete.parse(concreteInput);

    module Parseable_statics' = ParseStatus.Make(Parsing.Parseable_statics);
    let (staticsView, statics) = Parseable_statics'.parse(staticsInput);

    module Parseable_dynamics' = ParseStatus.Make(Parsing.Parseable_dynamics);
    let (dynamicsView, dynamics) = Parseable_dynamics'.parse(dynamicsInput);

    let replPane = switch (language, concrete, statics, dynamics) {
      | (Ok(language), Ok(concrete), Ok(statics), Ok(dynamics))
      =>
        <div className="repl-pane">
          <Repl
            history=replHistory
            language=language
            statics=statics
            concrete=concrete
            dynamics=dynamics
            setInput=(input => setHistory(hist => {...hist, input}))
            handleEnter=(() => setHistory(({input, before, after} as hist) => {
              switch (after) {
                | [] =>
                  let (parsed, result) = read_eval_input(language, concrete, statics, dynamics, input);
                  let before' = [ { input, parsed, result }, ...before ];
                  {before: before', after, input: ""}
                | _ => step_forward(language, concrete, statics, dynamics, hist)
              }
            }))
            handleUp=(n =>
              setHistory(hist => go_back(language, concrete, statics, dynamics, hist, n)))
            handleDown=(n =>
              setHistory(hist => go_forward(language, concrete, statics, dynamics, hist, n)))
          />
        </div>
      | _
      =>
        <div className="repl-pane disabled" />
    };

    let getGrammarPane = concrete => {
      let grammar = ConcreteSyntax.to_grammar(concrete);
      let module Lr0' = LrParsing.Lr0({ let grammar = grammar });
      let action_table = Lr0'.full_action_table(());
      let goto_table = Lr0'.full_goto_table(());
      <LrTables
        grammar=grammar
        action_table=action_table
        goto_table=goto_table
      />
    };

    let grammarPane = switch (concrete) {
        | Ok(concrete) => {
          try (getGrammarPane(concrete)) {
            | InvariantViolation(msg) => <div>{React.string(msg)}</div>
          }
        }
        | _ => <div>{React.string("grammar not available")}</div>
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

      <h2 className="header2 header2-concrete">
        {React.string("Concrete Syntax ")}
        {concreteView}
        <button onClick=(_ => setShowGrammarPane(_ => !showGrammarPane))>
          {React.string(showGrammarPane ?
                        "hide grammar tables" :
                        "show grammar tables")}
        </button>
      </h2>
      <div className="concrete-pane">
        {showGrammarPane ? grammarPane : ReasonReact.null}
        <CodeMirror
          value=concreteInput
          onBeforeChange=((_, _, str) => setConcreteInput(_ => str))
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

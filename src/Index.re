open ReactUtil
open Util
module Result = Belt.Result
module Grammar = LrParsingView.Grammar
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

/* Read and evaluate the user's input */
let read_and_eval = (abstract_syntax, concrete, statics, dynamics, input)
  : (parse_result, eval_result) => {
    open Core;

    let { Types.imports, language } = abstract_syntax;

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
      | Ok(abtResult') =>

        let env = {
          Bidirectional.rules: statics,
          var_types: Belt.Map.String.empty,
        };
        let tm = Statics.of_de_bruijn(abtResult');
        /* TODO: actually show this to the user */
        switch (Bidirectional.infer(env, tm)) {
          | _           => Js.log("check success")
          | exception _ => Js.log("check failure")
        };

        switch (Result.flatMap(term_denotation(dynamics, [], abtResult'), eval')) {
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
  abstract_syntax : Types.abstract_syntax,
  concrete        : ConcreteSyntaxDescription.t,
  statics         : list(Statics.rule),
  dynamics        : Core.denotation_chart,
  {input, before, after} : history,
  ) : history
  = {
    let (parsed, result) =
      read_and_eval(abstract_syntax, concrete, statics, dynamics, input);
    let (after, before, elem) =
      shift_from_to(after, before, { input, result, parsed });
    {before, after, input: elem.input}
  };

let step_back (
  abstract_syntax : Types.abstract_syntax,
  concrete        : ConcreteSyntaxDescription.t,
  statics         : list(Statics.rule),
  dynamics        : Core.denotation_chart,
  {input, before, after} : history,
  ) : history
  = {
    let (parsed, result) =
      read_and_eval(abstract_syntax, concrete, statics, dynamics, input);
    let (before, after, elem) =
      shift_from_to(before, after, { input, result, parsed });
    {before, after, input: elem.input}
  };

let rec go_back    = (lang, concrete, statics, dyn, hist, i) => switch(i) {
  | 0 => hist
  | _ => step_back(
    lang,
    concrete,
    statics,
    dyn,
    go_back(lang, concrete, statics, dyn, hist, i - 1)
  )
};

let rec go_forward = (lang, concrete, statics, dyn, hist, i) => switch(i) {
  | 0 => hist
  | _ => step_forward(
    lang,
    concrete,
    statics,
    dyn,
    go_forward(lang, concrete, statics, dyn, hist, i - 1)
  )
}

module Repl = {
  open Types;

  // TODO: move event type somewhere better
  let preventDefault : CodeMirror.event => unit
    = [%bs.raw "evt => evt.preventDefault()"];

  [@react.component]
  let make = (
    ~history: history,
    ~abstract_syntax: abstract_syntax,
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

    let (parsed, evalResult) =
      read_and_eval(abstract_syntax, concrete, statics, dynamics, input);

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

module SyntaxDebugger = {
  module MQueue = Belt.MutableQueue

  [@react.component]
  let make = (~grammar : LrParsing.grammar, ~lexer : Lex.lexer) => {
    let (input, setInput) = React.useState(() => "");
    let (hoverSpan, setHoverSpan) = React.useState(() => None);
    let inputRef = React.useRef(Js.Nullable.null);

    React.useEffect1(() => switch (hoverSpan) {
      | None => None
      | Some((start_span, end_span)) => {
        let node = inputRef
          -> React.Ref.current
          -> Js.Nullable.toOption;
        switch (node) {
          | Some(node')
          => ReactDOMRe.domElementToObj(node')
               ##setSelectionRange(start_span, end_span)
          | None
          => ()
        };
        None
      }
      },
      [|hoverSpan|]
    );

    switch (Lex.lex(lexer, input)) {
      /* TODO: highlight affected area */
      | Error({ message }) => React.string(message)
      | Ok(tokens) => {
        let tokenElems = tokens |. Belt.Array.map
          (({ name, start, finish }) =>
             <span
               className="token"
               onMouseOver=(_ => { setHoverSpan(_ => Some((start, finish))) })
               onMouseOut=(_ => { setHoverSpan(_ => None) })
             >
               {React.string(name)}
             </span>
          );

        let tokens' = MQueue.fromArray(tokens);
        let len = String.length(input);
        /* TODO: name might not always be "$" */
        MQueue.add(tokens', { name: "$", start: len, finish: len });

        /* TODO: avoid building this module twice */
        let module Lr0' = LrParsing.Lr0({ let grammar = grammar });
        let (_parse_result, trace) =
          Lr0'.parse_trace(LrParsing.DoTrace, tokens');

        let traceElems = trace
          |. Belt.Array.map(((action, stack, results, trace_tokens)) => {
            let cls = switch(action) {
              | Accept => "result-good"
              | Error  => "result-bad"
              | _      => ""
            };
            <tr className=cls>
              <td>{React.string(LrParsing.string_of_stack(stack))}</td>
              <td>{React.string(Lr0'.string_of_symbols(results))}</td>
              <td>{React.string(LrParsing.string_of_tokens(trace_tokens))}</td>
              <td>{React.string(Lr0'.string_of_action(action))}</td>
            </tr>
          });

        <div className="syntax-debugger">
          <div>
            <label htmlFor="example-input">
              {React.string("Example string: ")}
            </label>
            <input
              autoFocus=true
              id="example-input"
              type_="text"
              size=50
              value=input
              ref={ReactDOMRe.Ref.domRef(inputRef)}
              onChange=(event => setInput(ReactEvent.Form.target(event)##value))
            />
          </div>
          <div className="debugger-tokens">
            <span>{React.string("Tokens: ")}</span>
            {make_elem("span", tokenElems)}
          </div>
          <table>
            <thead>
              <tr>
                <th>{React.string("stack")}</th>
                <th>{React.string("symbols")}</th>
                <th>{React.string("input")}</th>
                <th>{React.string("action")}</th>
              </tr>
            </thead>
            {make_elem("tbody", traceElems)}
          </table>
        </div>
      }
    }
  };
};

module ConcreteSyntaxEditor = {
  type state =
    { concreteInput : string,
      showGrammarPane : bool,
      debuggerContents : string,
      showDebugger : bool,
      syntaxDesc : option(ConcreteSyntaxDescription.t),
    };

  type action =
    | DefinitionUpdate(string)
    | ToggleGrammarPane
    | ToggleDebugger
    ;

  [@react.component]
  let make = (~onComplete : ConcreteSyntaxDescription.t => unit) => {
    let ({ concreteInput, showGrammarPane, showDebugger }, dispatch) = React.useReducer(
      ({concreteInput, showGrammarPane, debuggerContents, showDebugger, syntaxDesc}, action) => switch (action) {
        | ToggleGrammarPane
        => {concreteInput, showGrammarPane: !showGrammarPane, debuggerContents, showDebugger, syntaxDesc}
        | ToggleDebugger
        => {concreteInput, showGrammarPane, debuggerContents, showDebugger: !showDebugger, syntaxDesc}
        | DefinitionUpdate(concreteInput')
        => {concreteInput: concreteInput', showGrammarPane, debuggerContents, showDebugger, syntaxDesc}
      },
      { concreteInput: LanguageSimple.concrete,
        showGrammarPane: false,
        debuggerContents: "",
        showDebugger: false,
        syntaxDesc: None,
      }
    );

    module Parseable_concrete =
      ParseStatus.Make(Parsing.Parseable_concrete_syntax);
    let (concreteDidParseView, pre_concrete) =
      Parseable_concrete.parse(concreteInput);

    let concrete = pre_concrete
      |. Belt.Result.map(((pre_terminal_rules, sort_rules)) =>
        ConcreteSyntax.make_concrete_description(pre_terminal_rules, sort_rules)
      );

    React.useEffect1(() => switch (concrete) {
      | Error(_) => None
      | Ok(concrete') => {
          onComplete(concrete');
          None
        }
      },
      [|concreteInput|]
    );

    let getGrammarPaneAndDebugger = (concrete, showGrammarPane, showDebugger) => {
      let grammar = ConcreteSyntax.to_grammar(concrete);
      let module Lr0' = LrParsing.Lr0({ let grammar = grammar });

      let states = Lr0'.states
        |. Belt.Array.map(state => {
          let kernel_items = Lr0'.state_to_item_set(state);
          let { LrParsing.nonkernel_items } : LrParsing.configuration_set
            = Lr0'.lr0_closure'(kernel_items);
          let kernel_repr
            = Lr0'.string_of_item_set(kernel_items, ~sep="\n");
          let nonkernel_repr
            = Lr0'.string_of_item_set(nonkernel_items, ~sep="\n");
          (state, kernel_repr, nonkernel_repr)
        })
        ;

      let grammarPane = if (showGrammarPane) {
        let action_table = Lr0'.full_lr0_action_table(());
        let goto_table = Lr0'.full_lr0_goto_table(());
        <div>
          <Grammar
            grammar=grammar
            states=states
          />
          <LrTables
            grammar=grammar
            action_table=action_table
            goto_table=goto_table
          />
        </div>
      } else {
        ReasonReact.null;
      };

      /* The debugger first lexes and shows the sequence of tokens. You can
       * hover over a token to highlight the text in the original string.
       *
       * It then parses. We show a table of parser transitions where you can
       * hover over any reduction to show the string included in the
       * production.
       */
      let debugger = if (showDebugger) {
        let lexer = ConcreteSyntax.lexer_of_desc(concrete);
        <SyntaxDebugger grammar=grammar lexer=lexer />;
      } else {
        ReasonReact.null
      };

      (grammarPane, debugger)
    };

    let (grammarPane, debugger) = switch (showGrammarPane, showDebugger, concrete) {
      | (false, false, _) => (ReasonReact.null, ReasonReact.null)
      | (_, _, Ok(concrete)) => {
        try (getGrammarPaneAndDebugger(concrete, showGrammarPane, showDebugger)) {
          | InvariantViolation(msg) =>
            (<div>{React.string(msg)}</div>, <div>{React.string(msg)}</div>)
        };
      }

      /* Just show one error message, even if both are supposed to be shown */
      | (_, _, Error(err)) => (React.string(err), ReasonReact.null)
    };

    <div>
      <h2 className="header2 header2-concrete">
        {React.string("Concrete Syntax ")}
        {concreteDidParseView}
      </h2>
      <div className="concrete-pane">
        <CodeMirror
          value=concreteInput
          onBeforeChange=((_, _, str) => dispatch(DefinitionUpdate(str)))
          options=CodeMirror.options(~mode="default", ())
        />
        <button onClick=(_ => dispatch(ToggleGrammarPane))>
          {React.string(showGrammarPane ?
                        "hide grammar tables" :
                        "show grammar tables")}
        </button>
        <button onClick=(_ => dispatch(ToggleDebugger))>
          {React.string(showDebugger ?  "hide debugger" : "show debugger")}
        </button>
      </div>
      {debugger}
      {grammarPane}
    </div>

  };
};

module DynamicsEditor = {
  [@react.component]
  let make = (~onComplete : Core.denotation_chart => unit) => {
    let (dynamicsInput, setDynamicsInput) =
      React.useState(() => LanguageSimple.dynamics);

    module Parseable_dynamics' = ParseStatus.Make(Parsing.Parseable_dynamics);
    let (dynamicsView, dynamics) = Parseable_dynamics'.parse(dynamicsInput);

    React.useEffect1(() => switch (dynamics) {
      | Error(_) => None
      | Ok(dynamics') => {
          onComplete(Core.produce_denotation_chart(dynamics'));
          None
        }
      },
      [|dynamicsInput|]
    );

    <div>
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
    </div>
  };
};

module ReplPane = {
  open Types;

  [@react.component]
  let make = (
    ~abstract_syntax: abstract_syntax,
    ~concrete: ConcreteSyntaxDescription.t,
    ~statics: list(Statics.rule),
    ~dynamics: Core.denotation_chart,
    ) => {

    let initialHistory: history = {
        input: "if false then false else true",
        before: [],
        after: [],
      }
    let (replHistory, setHistory) = React.useState(() => initialHistory);

    <div>
      <h2 className="header2 header2-repl">{React.string("repl")}</h2>
      <div className="repl-pane">
        <Repl
          history=replHistory
          abstract_syntax=abstract_syntax
          statics=statics
          concrete=concrete
          dynamics=dynamics
          setInput=(input => setHistory(hist => {...hist, input}))
          handleEnter=(() => setHistory(({input, before, after} as hist) => {
            switch (after) {
              | [] =>
                let (parsed, result) =
                  read_and_eval(abstract_syntax, concrete, statics, dynamics, input);
                let before' = [ { input, parsed, result }, ...before ];
                {before: before', after, input: ""}
              | _ => step_forward(abstract_syntax, concrete, statics, dynamics, hist)
            }
          }))
          handleUp=(n =>
            setHistory(hist =>
              go_back(abstract_syntax, concrete, statics, dynamics, hist, n)
            )
          )
          handleDown=(n =>
            setHistory(hist =>
              go_forward(abstract_syntax, concrete, statics, dynamics, hist, n)
            )
          )
        />
      </div>
    </div>
  };
};

module LvcaViewer = {

  type editing_details =
    { abstract_syntax: Types.abstract_syntax,
      concrete_syntax: option(ConcreteSyntaxDescription.t),
      statics: option(list(Statics.rule)),
      dynamics: option(Core.denotation_chart),
    };

  type details =
    { abstract_syntax: Types.abstract_syntax,
      concrete_syntax: ConcreteSyntaxDescription.t,
      statics: list(Statics.rule),
      dynamics: Core.denotation_chart,
    };

  type details_tab = ConcreteTab | StaticsTab | DynamicsTab;

  type stage =
    | AbstractSyntaxStage
    | DetailsStage(editing_details, details_tab)
    | ReplStage(details)
    ;

  type action =
    | ChangeTab(details_tab)
    | ASContinue(Types.abstract_syntax)
    | CompleteConcreteSyntax(ConcreteSyntaxDescription.t)
    | CompleteStatics(list(Statics.rule))
    | CompleteDynamics(Core.denotation_chart)
    | DetailsContinue(details)
    | ReplBack
    | Evaluate
    ;

  let mk_details = abstract_syntax => DetailsStage(
    { abstract_syntax,
      concrete_syntax: None,
      statics: None,
      dynamics: None,
    },
    ConcreteTab
  );

  let change_tab = details => tab => DetailsStage(details, tab);

  [@react.component]
  let make = () => {
    let (state, dispatch) = React.useReducer(
      (state, action) => switch (state, action) {
        | (AbstractSyntaxStage, ASContinue(lang))
        => mk_details(lang)
        | (DetailsStage(details, _), ChangeTab(tab))
        => DetailsStage(details, tab)
        | (DetailsStage(details, tab), CompleteConcreteSyntax(concrete_syntax))
        => DetailsStage({...details, concrete_syntax: Some(concrete_syntax)}, tab)
        | (DetailsStage(details, tab), CompleteStatics(statics))
        => DetailsStage({...details, statics: Some(statics)}, tab)
        | (DetailsStage(details, tab), CompleteDynamics(dynamics))
        => DetailsStage({...details, dynamics: Some(dynamics)}, tab)
        | (DetailsStage(_, _), DetailsContinue(details))
        => ReplStage(details)

        | (ReplStage({ abstract_syntax, concrete_syntax, statics, dynamics }),
          ReplBack)
        => DetailsStage({
          abstract_syntax,
          concrete_syntax: Some(concrete_syntax),
          statics: Some(statics),
          dynamics: Some(dynamics),
        }, ConcreteTab)
        | (ReplStage(_), Evaluate) => failwith("TODO: evaluation")

        | (AbstractSyntaxStage, _)
        => failwith(
          "invariant violation: unexpected action in AbstractSyntaxStage")
        | (DetailsStage(_, _), _)
        => failwith("invariant violation: unexpected action in DetailsStage")
        | (ReplStage(_), _)
        => failwith("invariant violation: unexpected action in ReplStage")
        },
      AbstractSyntaxStage
    );

    let view = switch (state) {
      | AbstractSyntaxStage =>
        <ContainedAbstractSyntaxEditor
          onContinue=(lang => dispatch(ASContinue(lang)))
        />
      | DetailsStage(details, tab) => {
        let tab_contents = switch (tab) {
        | ConcreteTab =>
        <ConcreteSyntaxEditor
          onComplete=(syntax_desc =>
                      dispatch(CompleteConcreteSyntax(syntax_desc)))
        />
        | StaticsTab  => <StaticsEditor
          onComplete=(statics => dispatch(CompleteStatics(statics)))
        />
        | DynamicsTab => <DynamicsEditor
          onComplete=(dynamics => dispatch(CompleteDynamics(dynamics)))
        />
        };
        let details' =
          (details.concrete_syntax, details.statics, details.dynamics);
        let continue_button = switch (details') {
          | (Some(concrete_syntax), Some(statics), Some(dynamics)) => {
            let details =
              { abstract_syntax: details.abstract_syntax,
                concrete_syntax, statics, dynamics
              };
            <button onClick=(_ => dispatch(DetailsContinue(details)))>
              {React.string("Continue to REPL")}
            </button>
          }
          | (_, _, _) => ReasonReact.null
        };
        <div>
          <button onClick=(_ => dispatch(ChangeTab(ConcreteTab)))>
            {React.string("Concrete syntax")}
          </button>
          <button onClick=(_ => dispatch(ChangeTab(StaticsTab)))>
            {React.string("Statics")}
          </button>
          <button onClick=(_ => dispatch(ChangeTab(DynamicsTab)))>
            {React.string("Dynamics")}
          </button>
          {continue_button}
          {tab_contents}
        </div>
      }
      | ReplStage({ abstract_syntax, concrete_syntax, statics, dynamics })
      =>
        <div>
          <button onClick=(_ => dispatch(ReplBack))>
            {React.string("back")}
          </button>
          <ReplPane
            abstract_syntax=abstract_syntax
            concrete=concrete_syntax
            statics=statics
            dynamics=dynamics
          />
        </div>
    };

    <div className="lvca-viewer">
      <h1 className="header">{React.string("LVCA")}</h1>
      {view}
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

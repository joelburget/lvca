open ReactUtil
open Util
module Result = Belt.Result
module Grammar = LrParsingView.Grammar
module LrTables = LrParsingView.Tables

// TODO: duplicated in EvalView
type located_err = (string, option(Binding.DeBruijn.term))
type translation_result('a) = Belt.Result.t('a, located_err)
type parse_result = translation_result(Binding.Nominal.term);
type eval_result  = translation_result(Core.core);

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

    let { Types.imports, sort_defs } = abstract_syntax;

    let (astResult, abtResult) = switch (ConcreteSyntax.parse(concrete, "tm", input)) {
    | Ok(tree)
    => switch (ConcreteSyntax.to_ast(concrete, tree)) {
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

        failwith("TODO");
        /*
        switch (Result.flatMap(term_denotation(dynamics, [], abtResult'), eval')) {
        | Ok(core_val) => (astResult, Ok(core_val))
        | Error(msg)   => (astResult, Error(msg)) // Error((msg, Some(abtResult'))))
        }
        */
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
      let shift = CodeMirror.metaKeyGet(evt);
      if (shift) {
        if (key == "Enter") {
          CodeMirror.preventDefault(evt);
          handleEnter();
        } else if (key == "ArrowUp") {
          CodeMirror.preventDefault(evt);
          handleUp(1);
        } else if (key == "ArrowDown") {
          CodeMirror.preventDefault(evt);
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

    let (tokens, message) = switch (Lex.lex(lexer, input)) {
      /* TODO: highlight affected area */
      | Error({ message }) => ([||], React.string(message))
      | Ok(tokens) => (tokens, ReasonReact.null)
    };

    let tokens' = tokens
      |. Belt.Array.keep (token => token.name != "SPACE");
    let tokenElems = tokens'
      |. Belt.Array.map
      (({ name, start, finish }) =>
         <span
           className="token"
           onMouseOver=(_ => { setHoverSpan(_ => Some((start, finish))) })
           onMouseOut=(_ => { setHoverSpan(_ => None) })
         >
           {React.string(name)}
         </span>
      );

    let tokens'' = MQueue.fromArray(tokens');
    let len = String.length(input);
    /* TODO: name might not always be "$" */
    MQueue.add(tokens'', { name: "$", start: len, finish: len });

    /* TODO: avoid building this module twice */
    let module Lalr = LalrParsing.Lalr1({ let grammar = grammar });
    let (_parse_result, trace) =
      Lalr.parse_trace(LrParsing.DoTrace, tokens'');

    let traceElems = trace
      |. Belt.Array.map(({action, stack, results, input}) => {
        let cls = switch(action) {
          | Accept   => "result-good"
          | Error(_) => "result-bad"
          | _        => ""
        };
        <tr className=cls>
          <td>{React.string(LrParsing.string_of_stack(stack))}</td>
          <td>{React.string(Lalr.string_of_symbols(results))}</td>
          <td>{React.string(Lex.string_of_tokens(input))}</td>
          <td>{React.string(Lalr.string_of_action(action))}</td>
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
      {message}
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
  };
};

module ConcreteSyntaxEditor = {
  type state =
    { concreteInput : string,
      debuggerContents : string,
      showGrammarPane : bool,
      showDebugger : bool,
      showDerivedGrammar : bool,
      syntaxDesc : option(ConcreteSyntaxDescription.t),
      startSort : string,
    };

  type action =
    | DefinitionUpdate(string)
    | SelectStartSort(string)
    | ToggleDerivedGrammar
    | ToggleGrammarPane
    | ToggleDebugger
    ;

  [@react.component]
  let make = (~onUpdate : Belt.Result.t(ConcreteSyntaxDescription.t, string) => unit,
              ~abstractSyntax : Types.abstract_syntax) => {
    let sortNames = Types.sort_names(abstractSyntax) |. Belt.Set.String.toArray;

    let (state, dispatch) = React.useReducer(
      (startState, action) => switch (action) {
        | ToggleDerivedGrammar
        => {...startState, showDerivedGrammar: !startState.showDerivedGrammar}
        | ToggleGrammarPane
        => {...startState, showGrammarPane: !startState.showGrammarPane}
        | ToggleDebugger
        => {...startState, showDebugger: !startState.showDebugger}
        | DefinitionUpdate(concreteInput)
        => {...startState, concreteInput}
        | SelectStartSort(startSort)
        => {...startState, startSort}
      },
      { concreteInput: LanguageJson.concreteSyntax,
        showDerivedGrammar: false,
        showGrammarPane: false,
        showDebugger: false,
        debuggerContents: "",
        startSort: sortNames[0],
        syntaxDesc: None,
      }
    );
    let { concreteInput, showDerivedGrammar, showGrammarPane, showDebugger,
      startSort } = state;

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
          onUpdate(concrete);
          None
        }
      },
      [|concreteInput|]
    );

    let getGrammarPaneAndDebugger = (concrete, showDerivedGrammar, showGrammarPane, showDebugger) => {
      let (grammar, _, _) = ConcreteSyntax.to_grammar(concrete, startSort);
      let module Lalr = LalrParsing.Lalr1({ let grammar = grammar });
      let module LrTables' = LrTables(LrParsing.Lr0({ let grammar = grammar }));

      let states = Lalr.states |. Belt.Array.map(state => {
          let kernel_items = Lalr.state_to_lookahead_item_set(state);
          let { LalrParsing.nonkernel_items }
            = Lalr.lr1_closure'(kernel_items);
          let kernel_repr
            = Lalr.string_of_lookahead_item_set(kernel_items);
          let nonkernel_repr
            = Lalr.string_of_lookahead_item_set(nonkernel_items);
          (state, kernel_repr, nonkernel_repr)
        });

      let grammarPane = if (showGrammarPane) {
        let action_table = Lalr.full_lalr1_action_table(());
        let goto_table = Lalr.full_lalr1_goto_table(());
        <div>
          <Grammar
            grammar=grammar
            states=states
          />
          <LrTables'
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

    let (derivedGrammar, grammarPane, debugger) = switch (showDerivedGrammar, showGrammarPane, showDebugger, concrete) {
      | (false, false, false, _) => (ReasonReact.null, ReasonReact.null, ReasonReact.null)
      | (_, _, _, Ok(concrete)) => {

        let derivedGrammar = if (showDerivedGrammar) {
          let derivedRules = ConcreteSyntax.derived_nonterminal_rules(concrete.nonterminal_rules);
          let str = ConcreteSyntax.string_of_derived_rules(derivedRules);
          <pre> (React.string(str)) </pre>
        } else {
          ReasonReact.null;
        }

        let (x, y) = try (getGrammarPaneAndDebugger(concrete, showDerivedGrammar, showGrammarPane, showDebugger)) {
          | InvariantViolation(msg) =>
            (<div>{React.string(msg)}</div>, <div>{React.string(msg)}</div>)
        };

        (derivedGrammar, x, y)
      }

      /* Just show one error message, even if multiple are supposed to be shown */
      | (_, _, _, Error(err)) => (React.string(err), ReasonReact.null, ReasonReact.null)
    };

    let sortOptions = ReactDOMRe.createDOMElementVariadic(
      "select",
      ~props=ReactDOMRe.domProps(
        ~onChange=(evt =>
          dispatch(SelectStartSort(ReactEvent.Form.target(evt)##value))),
        ()
      ),
      sortNames |. Belt.Array.map (name =>
        <option value=(name)> (React.string(name)) </option>
      )
    );

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
        (sortOptions)
        <button onClick=(_ => dispatch(ToggleDerivedGrammar))>
          {React.string(showDerivedGrammar ?
                        "hide derived grammar" :
                        "show derived grammar")}
        </button>
        <button onClick=(_ => dispatch(ToggleGrammarPane))>
          {React.string(showGrammarPane ?
                        "hide grammar tables" :
                        "show grammar tables")}
        </button>
        <button onClick=(_ => dispatch(ToggleDebugger))>
          {React.string(showDebugger ?  "hide debugger" : "show debugger")}
        </button>
      </div>
      {derivedGrammar}
      {debugger}
      {grammarPane}
    </div>

  };
};

module DynamicsEditor = {
  [@react.component]
  let make = (~onUpdate : Belt.Result.t(Core.denotation_chart, string) => unit) => {
    let (dynamicsInput, setDynamicsInput) =
      React.useState(() => LanguageJson.abstractSyntax);

    module Parseable_dynamics' = ParseStatus.Make(Parsing.Parseable_dynamics);
    let (dynamicsView, dynamics) = Parseable_dynamics'.parse(dynamicsInput);

    let handleKey = (_editor, evt) => {
      if (CodeMirror.metaKeyGet(evt) && CodeMirror.keyGet(evt) == "Enter") {
        CodeMirror.preventDefault(evt);
        onUpdate(dynamics);
      }
    };

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
          onKeyDown=handleKey
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

  type state = {
    abstractExpanded: bool,
    abstract: option(Belt.Result.t(Types.abstract_syntax, string)),
    concreteExpanded: bool,
    concrete: option(Belt.Result.t(ConcreteSyntaxDescription.t, string)),
    staticsExpanded: bool,
    statics: option(Belt.Result.t(list(Statics.rule), string)),
    dynamicsExpanded: bool,
    dynamics: option(Belt.Result.t(Core.denotation_chart, string)),
    replExpanded: bool,
  };

  type action =
    | AbstractUpdate(Belt.Result.t(Types.abstract_syntax, string))
    | ConcreteUpdate(Belt.Result.t(ConcreteSyntaxDescription.t, string))
    | StaticsUpdate(Belt.Result.t(list(Statics.rule), string))
    | DynamicsUpdate(Belt.Result.t(Core.denotation_chart, string))

    | ToggleAbstract
    | ToggleConcrete
    | ToggleStatics
    | ToggleDynamics
    | ToggleRepl

  let initialState = {
    abstractExpanded: true,
    abstract: None,
    concreteExpanded: false,
    concrete: None,
    staticsExpanded: false,
    statics: None,
    dynamicsExpanded: false,
    dynamics: None,
    replExpanded: false,
  };

  [@react.component]
  let make = () => {
    let (state, dispatch) = React.useReducer(
      (state, action) => switch (action) {
        | ToggleAbstract
        => { ...state, abstractExpanded: !state.abstractExpanded }
        | ToggleConcrete
        => { ...state, concreteExpanded: !state.concreteExpanded }
        | ToggleStatics
        => { ...state, staticsExpanded: !state.staticsExpanded }
        | ToggleDynamics
        => { ...state, dynamicsExpanded: !state.dynamicsExpanded }
        | ToggleRepl
        => { ...state, replExpanded: !state.replExpanded }

        | AbstractUpdate(update)
        => { ...state, abstract: Some(update) }
        | ConcreteUpdate(update)
        => { ...state, concrete: Some(update) }
        | StaticsUpdate(update)
        => { ...state, statics: Some(update) }
        | DynamicsUpdate(update)
        => { ...state, dynamics: Some(update) }
      },
      initialState
    );

    let mkSection = (name, enabled, expanded, toggleEvt, contents) =>
      <div className=("section-box " ++ (enabled ? "" : "disabled"))>
        <h3 onClick=(_ => dispatch(toggleEvt))>
          <a href="#">
            (React.string(expanded ?  "- " ++ name : "+ " ++ name))
          </a>
        </h3>
        (expanded ? contents() : React.null)
      </div>;

    let abstractOkay = switch (state.abstract) {
      | Some(Ok(_)) => true
      | _ => false
    };

    let abstract = mkSection("Abstract Syntax", true, state.abstractExpanded,
                             ToggleAbstract,
      _ =>
        <ContainedAbstractSyntaxEditor
          onUpdate=(lang => dispatch(AbstractUpdate(lang)))
          initialInput=LanguageJson.abstractSyntax
        />
    );

    let concrete = switch (state.concreteExpanded, state.abstract) {
      | (true, Some(Ok(abstractSyntax))) =>
        <ConcreteSyntaxEditor
          onUpdate=(update => dispatch(ConcreteUpdate(update)))
          abstractSyntax=abstractSyntax
        />
      | _ => React.null
    };

    let statics = mkSection("Statics", abstractOkay, state.staticsExpanded,
                            ToggleStatics,
      _ =>
        <StaticsEditor
          onUpdate=(update => dispatch(StaticsUpdate(update)))
          initialInput=""
        />
    );

    let dynamics = mkSection("Dynamics", abstractOkay, state.dynamicsExpanded,
                             ToggleDynamics,
      _ =>
        <DynamicsEditor
          onUpdate=(update => dispatch(DynamicsUpdate(update)))
        />
    );

    let replEnabled = switch(
                       state.abstract,
                       state.concrete,
                       state.statics,
                       state.dynamics) {
      | (Some(Ok(_)),
         Some(Ok(_)),
         Some(Ok(_)),
         Some(Ok(_))) =>
        true
      | _ => false
    };

    let repl = switch (state.replExpanded,
                       state.abstract,
                       state.concrete,
                       state.statics,
                       state.dynamics) {
      | (true,
         Some(Ok(abstractSyntax)),
         Some(Ok(concreteSyntax)),
         Some(Ok(statics)),
         Some(Ok(dynamics))) =>
        <div>
          <ReplPane
            abstract_syntax=abstractSyntax
            concrete=concreteSyntax
            statics=statics
            dynamics=dynamics
          />
        </div>
      | _ => React.null
    };

    <div className="lvca-viewer">
      <h1 className="header">{React.string("LVCA")}</h1>
      (abstract)
      (mkSection("Concrete", abstractOkay, state.concreteExpanded,
                 ToggleConcrete, _ => concrete))
      (statics)
      (dynamics)
      (mkSection("Repl", replEnabled, state.replExpanded, ToggleRepl, _ => repl))
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

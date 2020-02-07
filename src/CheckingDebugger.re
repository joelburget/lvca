open Bidirectional;
open Bidirectional_TestUtil;
open Statics;

let elem_of_env = ({ var_types }) => {
  let vars = var_types
    |. Belt.Map.String.toArray
    |. Belt.Array.map (((name, ty)) => {
      <li>
        {React.string(name)}
        {React.string(string_of_term(ty))}
      </li>
    });

  switch (vars) {
    | [||] => <h4>{React.string("(empty environment)")}</h4>
    | _ =>
      <div>
        <h4>{React.string("environment")}</h4>
        {ReactUtil.make_elem("ul", vars)}
      </div>
  }
};

let string_of_typing = (Typing(tm, ty)) =>
  string_of_term(tm) ++ ": " ++ string_of_term(ty);

let elem_of_trace_entry = trace_entry => switch (trace_entry) {
  | CheckTrace(env, typing) =>
    <div>
      <h3>{React.string("check")}</h3>
      {elem_of_env(env)}
      {React.string(string_of_typing(typing))}
    </div>
  | InferTrace(env, term) =>
    <div>
      <h3>{React.string("infer")}</h3>
      {elem_of_env(env)}
      {React.string(string_of_term(term))}
    </div>
  | CheckSuccess =>
    <div>
      <h3>{React.string("check success")}</h3>
    </div>
  | CheckFailure(_) =>
    <div>
      <h3>{React.string("check failure")}</h3>
    </div>
  | Inferred(ty) =>
    <div>
      <h3>{React.string("inferred")}</h3>
      {React.string(string_of_term(ty))}
    </div>
};

let elem_of_current_stack = current_stack => {
  let (last_row, stack) = switch (current_stack) {
    | [Inferred(ty), entry, ...current_stack']
    => {
      let row =
        <tr>
          <td>{elem_of_trace_entry(entry)}</td>
          <td className="result-good">
            {React.string("inferred " ++ string_of_term(ty))}
          </td>
        </tr>;
      ([|row|], current_stack')
    }
    | [CheckSuccess, entry, ...current_stack']
    => {
      let row =
        <tr>
          <td>{elem_of_trace_entry(entry)}</td>
          <td className="result-good">{React.string("success")}</td>
        </tr>;
      ([|row|], current_stack')
    }
    | [CheckFailure(msg), entry, ...current_stack']
    => {
      let row =
        <tr>
          <td>{elem_of_trace_entry(entry)}</td>
          <td className="result-bad">{React.string("failure: " ++ msg)}</td>
        </tr>;
      ([|row|], current_stack')
    }
    | _ => ([||], current_stack)
  };

  let other_rows = stack
    |. Belt.List.toArray
    |. Belt.Array.map(trace_entry =>
      <tr>
        <td>{elem_of_trace_entry(trace_entry)}</td>
        <td></td>
      </tr>
    )
    |. Belt.Array.reverse;

  ReactUtil.make_elem("tbody", Belt.Array.concat(other_rows, last_row))
};

module CheckingDebugger = {

  type action =
    | AbstractUpdate(Belt.Result.t(Types.abstract_syntax, string))
    | StaticsUpdate(Belt.Result.t(list(Statics.rule), string))
    | Evaluate(string)
    | StepForward
    | StepBackward
    | ToggleAbstract
    | ToggleStatics
    | ToggleDebugger
    ;

  type debuggerState = {
    input: string,
    steps: array(trace_step),
    currentStep: int,
  };

  type state = {
    abstractExpanded: bool,
    /* XXX never used */
    abstract: option(Belt.Result.t(Types.abstract_syntax, string)),
    staticsExpanded: bool,
    statics: option(Belt.Result.t(list(Statics.rule), string)),
    debuggerExpanded: bool,
    debugger: option(debuggerState),
  };

  [@react.component]
  let make = () => {
    let (state, dispatch) = React.useReducer(
      (state, action) => switch (action) {
        | AbstractUpdate(abstractResult)
        => { ...state, abstract: Some(abstractResult) }
        | StaticsUpdate(staticsResult)
        => { ...state, statics: Some(staticsResult) }
        | StepForward | StepBackward
        => {
          let debugger = switch (state.debugger) {
            | None => None
            | Some(state) => switch (action) {
              | StepForward
              => Some({ ...state, currentStep: state.currentStep + 1 })
              | StepBackward
              => Some({ ...state, currentStep: state.currentStep - 1 })
              | _ => failwith("invariant violation: can only handle Steps")
            }
          };
          { ...state, debugger }
        }
        | ToggleAbstract
        => { ...state, abstractExpanded: !state.abstractExpanded }
        | ToggleStatics
        => { ...state, staticsExpanded: !state.staticsExpanded }
        | ToggleDebugger
        => { ...state, debuggerExpanded: !state.debuggerExpanded }
        | Evaluate(input)
        => {
          let steps : array(trace_step) = [||];
          let handle_trace = trace_step => {
            let _ = Js.Array2.push(steps, trace_step);
            ()
          };

          let env = { rules: statics, var_types: Belt.Map.String.empty }

          let debugger = switch (parse_cvt(input)) {
            | exception _ => None
            | tm => switch (infer_trace(handle_trace, env, tm)) {
              | _ty => Some({ input, steps, currentStep: 0 })
              | exception _ => None
            }
          };
          { ...state, debugger }
        }
      },
      {
        abstractExpanded: true,
        abstract: None,
        staticsExpanded: false,
        statics: None,
        debuggerExpanded: false,
        debugger: None
      }
    );

    let abstract = switch (state.abstractExpanded) {
      | true
      =>
        <ContainedAbstractSyntaxEditor
          onUpdate=(update => dispatch(AbstractUpdate(update)))
          // XXX update initialInput
          initialInput=LanguageSimple.abstractSyntax
        />
      | false
      => React.null
    };

    let statics = switch (state.staticsExpanded) {
      | true
      => {
        <StaticsEditor
          onUpdate=(statics => dispatch(StaticsUpdate(statics)))
          initialInput=LanguageSimple.statics
        />
      }
      | false
      => React.null
    };

    let debuggerInput = switch (state.debuggerExpanded) {
      | true
      =>
        <input
          type_="text"
          onKeyUp=(event => if (ReactEvent.Keyboard.key(event) == "Enter") {
            dispatch(Evaluate(ReactEvent.Keyboard.target(event)##value));
          })
        />
      | false
      => React.null
    };

    let debugger = switch (state.debuggerExpanded) {
      | true
      => switch (state.debugger) {
        | None => React.null
        | Some({ input, steps, currentStep })
        => {
          let step_count = Belt.Array.length(steps);
          let current_step = Belt.Array.getExn(steps, currentStep);
          let elem = elem_of_current_stack(current_step);

          let backButton = switch(currentStep == 0) {
            | true =>
              <button className="disabled">
                {React.string("previous step")}
              </button>
            | false =>
              <button onClick=(_evt => dispatch(StepBackward))>
                {React.string("previous step")}
              </button>
          };

          let forwardButton = switch(currentStep == step_count - 1) {
            | true =>
              <button className="disabled">
                {React.string("next step")}
              </button>
            | false =>
              <button onClick=(_evt => dispatch(StepForward))>
                {React.string("next step")}
              </button>
          };

          <div>
            <div>{React.string("inferring type of " ++ input)}</div>
            <div>
              {React.string(Printf.sprintf("%n / %n", currentStep + 1, step_count))}
            </div>
            (backButton)
            (forwardButton)
            <table>
              <thead>
                <tr>
                  <th>{React.string("action")}</th>
                  <th>{React.string("result")}</th>
                </tr>
              </thead>
              {elem}
            </table>
          </div>
        }
      }
      | false
      => React.null
    };

    <div>
      <div className="section-box">
        <h3>
          <a href="#" onClick=(_ => dispatch(ToggleAbstract))>
            (React.string(state.abstractExpanded ?
              "- Abstract Syntax" : "+ Abstract Syntax"))
          </a>
        </h3>
        (abstract)
      </div>
      <div className="section-box">
        <h3>
          <a href="#" onClick=(_ => dispatch(ToggleStatics))>
            (React.string(state.staticsExpanded ?  "- Statics" : "+ Statics"))
          </a>
        </h3>
        (statics)
      </div>
      <div className="section-box">
        <h3>
          <a href="#" onClick=(_ => dispatch(ToggleDebugger))>
            (React.string(state.debuggerExpanded ? "- Debugger" : "+ Debugger"))
          </a>
        </h3>
        (debuggerInput)
        (debugger)
      </div>
    </div>
  }
};

ReactDOMRe.renderToElementWithId(<CheckingDebugger />, "index");

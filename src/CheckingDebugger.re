module CheckingDebugger = {
  open Bidirectional
  open Bidirectional_TestUtil
  open Statics

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

  type action =
    | CompleteAbstractSyntax(Types.abstract_syntax)
    | CompleteStatics(list(Statics.rule))
    | Evaluate(string)
    | StepForward
    | StepBackward
    | PreviousStage
    ;

  type state =
    | EditingAbstractSyntax
    | EditingStatics(Types.abstract_syntax)
    | EditingInfer(Types.abstract_syntax, list(Statics.rule))
    | EditingCheck(Types.abstract_syntax, list(Statics.rule))
    | Tracing(Types.abstract_syntax, list(Statics.rule), string, array(trace_step), int)
    ;

  [@react.component]
  let make = () => {
    let (state, dispatch) = React.useReducer(
      (state, action) => switch (state, action) {
        | (EditingAbstractSyntax, CompleteAbstractSyntax(lang))
        => EditingStatics(lang)
        | (EditingStatics(lang), CompleteStatics(statics))
        => EditingInfer(lang, statics)
        | (EditingStatics(lang), PreviousStage)
        => EditingAbstractSyntax
        | (EditingInfer(lang, statics), Evaluate(tmStr))
        => {
          let trace : array(trace_step) = [||];
          let handle_trace = trace_step => {
            let _ = Js.Array2.push(trace, trace_step);
            ()
          };

          let env = { rules: statics, var_types: Belt.Map.String.empty }

          switch (parse_cvt(tmStr)) {
            | exception _ => EditingInfer(lang, statics)
            | tm => switch (infer_trace(handle_trace, env, tm)) {
              | ty => Tracing(lang, statics, tmStr, trace, 0)
              | exception _ => EditingInfer(lang, statics)
            }
          }
        }

        | (EditingInfer(lang, statics), PreviousStage)
        => EditingStatics(lang)

        | (Tracing(lang, statics, tmStr, trace, stepNo), StepForward)
        => Tracing(lang, statics, tmStr, trace, min(stepNo + 1, Belt.Array.length(trace) - 1))
        | (Tracing(lang, statics, tmStr, trace, stepNo), StepBackward)
        => Tracing(lang, statics, tmStr, trace, max(stepNo - 1, 0))
        | (Tracing(lang, statics, _, _, _), PreviousStage)
        => EditingInfer(lang, statics)

        | _ => EditingAbstractSyntax
      },
      EditingAbstractSyntax
    );

    switch (state) {
      | EditingAbstractSyntax =>
        <ContainedAbstractSyntaxEditor
          onContinue=(lang => dispatch(CompleteAbstractSyntax(lang)))
        />

      | EditingStatics(statics) =>
        <div>
          <button onClick=(_ => dispatch(PreviousStage))>
            {React.string("Back to Abstract Syntax")}
          </button>
          <StaticsEditor
            onComplete=(statics => dispatch(CompleteStatics(statics)))
          />
        </div>

      | EditingInfer(_lang, _statics) => {
        <div>
          <h2>{React.string("Input")}</h2>
          <button onClick=(_ => dispatch(PreviousStage))>
            {React.string("Back to Statics")}
          </button>
          <input
            type_="text"
            onKeyUp=(event => if (ReactEvent.Keyboard.key(event) == "Enter") {
              dispatch(Evaluate(ReactEvent.Keyboard.target(event)##value));
            })
          />
        </div>
      }

      | Tracing(_lang, _statics, tmStr, trace, stepNo) =>
        let step_count = Belt.Array.length(trace);
        let current_step = Belt.Array.getExn(trace, stepNo);
        let elem = elem_of_current_stack(current_step);

        <div>
          <h2>{React.string("Typechecking Debugger")}</h2>
          <button onClick=(_ => dispatch(PreviousStage))>
            {React.string("Back to Input")}
          </button>
          <div>{React.string("inferring type of " ++ tmStr)}</div>
          <div>
            {React.string(Printf.sprintf("%n / %n", stepNo + 1, step_count))}
          </div>
          <button onClick=(_evt => dispatch(StepBackward))>
            {React.string("previous step")}
          </button>
          <button onClick=(_evt => dispatch(StepForward))>
            {React.string("next step")}
          </button>
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
};

ReactDOMRe.renderToElementWithId(<CheckingDebugger />, "index");

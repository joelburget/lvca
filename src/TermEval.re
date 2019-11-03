open Belt.Result;

[@react.component]
let make = () => {
  let (termInput, setInput) = React.useState(() => "");
  let (continuousMode, setContinuousMode) = React.useState(() => false);
  let (termValue, setTermValue) = React.useState(() => None);
  module Parseable_term' = ParseStatus.Make(Parsing.Parseable_term);

  let resultView = switch (termValue) {
    | None => React.string("");
    | Some(tm_result) => {
      let resultView = switch (tm_result) {
        | Error(err) => <div>{React.string("error: " ++ err)}</div>
        | Ok(tm') => switch (TermEval_Eval.check_eval(tm')) {
          | Error(msg)=> <div>{React.string(msg)}</div>
          | Ok((ty_str, tm_str)) =>
          <div>
            <div>{React.string(tm_str)}</div>
            <div>{React.string("inferred type: " ++ ty_str)}</div>
          </div>
        }
      };

      resultView
    }
  };

  let handleChange = str => {
    setInput(str);
  };

  let handleKeyUp = key => if (continuousMode || key == "Enter") {
    let (_, tm) = Parseable_term'.parse(termInput);
    setTermValue(_ => Some(tm));
  };

  let handleCMToggle = _ => {
    if (continuousMode) {
      setTermValue(_ => None);
    }
    setContinuousMode(cm => !cm);
  };

  <div>
    <div className="term-pane">
      <div>
        <label>
          {React.string("update continuously ")}
          <input
            type_="checkbox"
            checked=continuousMode
            onChange=handleCMToggle
          />
        </label>
      </div>
      <input
        type_="text"
        value=termInput
        onChange=(event => handleChange(ReactEvent.Form.target(event)##value))
        // handle keyup so the input has already changed
        onKeyUp=(event => handleKeyUp(ReactEvent.Keyboard.key(event)))
      />
      {resultView}
    </div>
  </div>
};

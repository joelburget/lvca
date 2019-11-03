[@react.component]
let make = () => {
  let (termInput, setInput) = React.useState(() => "");

  module Parseable_term' =
    ParseStatus.Make(Parsing.Parseable_term);
  let (termView, _tm) = Parseable_term'.parse(termInput);

  <div>
    {termView}
    <div className="term-pane">
      <input
        value=termInput
        onChange=(event => setInput(ReactEvent.Form.target(event)##value))
      />
    </div>
  </div>
};

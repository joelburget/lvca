type state = (string, option(list(Statics.rule)));
type action = string;

[@react.component]
let make = (
  ~onUpdate : Belt.Result.t(list(Statics.rule), string) => unit,
  ~initialInput : string
  ) => {
  let (staticsInput, setStaticsInput) = React.useState(() => initialInput);

  module Parseable_statics' = ParseStatus.Make(Parsing.Parseable_statics);
  let (staticsView, statics) = Parseable_statics'.parse(staticsInput);

  let continueView = switch (statics) {
    | Error(_) => staticsView
    | Ok(_) =>
    <button onClick=(_ => onUpdate(statics)) >
      {React.string("continue")}
    </button>
  };

  <div>
    {continueView}
    <div className="statics-pane">
      <CodeMirror
        value=staticsInput
        onBeforeChange=((_, _, str) => setStaticsInput(_ => str))
        options=CodeMirror.options(~mode="default", ())
      />
    </div>
  </div>
};

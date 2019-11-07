type state = (string, option(list(Statics.rule)));
type action = string;

[@react.component]
let make = (~onComplete : list(Statics.rule) => unit) => {
  let (staticsInput, setStaticsInput) =
    React.useState(() => LanguageSimple.statics);

  module Parseable_statics' = ParseStatus.Make(Parsing.Parseable_statics);
  let (staticsView, statics) = Parseable_statics'.parse(staticsInput);

  let continueView = switch (statics) {
    | Error(_) => ReasonReact.null
    | Ok(statics') =>
    <button onClick=(_ => onComplete(statics')) >
      {React.string("continue")}
    </button>
  };

  <div>
    {continueView}
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
  </div>
};

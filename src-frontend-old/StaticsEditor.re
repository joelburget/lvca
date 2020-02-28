type state = (string, option(list(Statics.rule)));
type action = string;

[@react.component]
let make = (
  ~onUpdate : Belt.Result.t(list(Statics.rule), string) => unit,
  ~initialInput : string
  ) => {
  let (staticsInput, setStaticsInput) = React.useState(() => initialInput);
  let (hasSubmitted, setSubmitted) = React.useState(() => false);

  module Parseable_statics' = ParseStatus.Make(Parsing.Parseable_statics);
  let (staticsView, statics) = Parseable_statics'.parse(staticsInput);

  let handleKey = (_editor, evt) => {
    if (CodeMirror.metaKeyGet(evt) && CodeMirror.keyGet(evt) == "Enter") {
      CodeMirror.preventDefault(evt);
      onUpdate(statics);
      setSubmitted(_ => true);
    }
  };

  <div>
    (hasSubmitted ? staticsView : React.null)
    <div className="statics-pane">
      <CodeMirror
        value=staticsInput
        onBeforeChange=((_, _, str) => setStaticsInput(_ => str))
        options=CodeMirror.options(~mode="default", ())
        onKeyDown=handleKey
      />
    </div>
  </div>
};

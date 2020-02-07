[@react.component]
let make = (
  ~onUpdate : Belt.Result.t(Types.abstract_syntax, string) => unit,
  ~initialInput: string
  ) => {

  let (asInput, setAsInput) = React.useState(() => initialInput);

  module Parseable_abstract_syntax' =
    ParseStatus.Make(Parsing.Parseable_abstract_syntax);
  let (languageView, language) = Parseable_abstract_syntax'.parse(asInput);

  let continueView = switch (language) {
    | Error(_) => languageView
    | Ok(_) =>
    <button onClick=(_ => onUpdate(language)) >
      {React.string("continue")}
    </button>
  };

  <div className="abstract-syntax-pane">
    (continueView)
    <CodeMirror
      value=asInput
      onBeforeChange=((_, _, str) => setAsInput(_ => str))
      options=CodeMirror.options(~mode="default", ())
    />
  </div>
};

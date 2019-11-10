[@react.component]
let make = (~onContinue : Types.abstract_syntax => unit) => {
  let (asInput, setAsInput) =
    React.useState(() => LanguageSimple.abstractSyntax);

  module Parseable_abstract_syntax' =
    ParseStatus.Make(Parsing.Parseable_abstract_syntax);
  let (languageView, language) = Parseable_abstract_syntax'.parse(asInput);

  let continueView = switch (language) {
    | Error(_) => ReasonReact.null
    | Ok(language') =>
    <button onClick=(_ => onContinue(language')) >
      {React.string("continue")}
    </button>
  };

  <div>
    {continueView}
    <h2 className="header2 header2-abstract-syntax">
      {React.string("Abstract Syntax ")}
      {languageView}
    </h2>
    <div className="abstract-syntax-pane">
      <CodeMirror
        value=asInput
        onBeforeChange=((_, _, str) => setAsInput(_ => str))
        options=CodeMirror.options(~mode="default", ())
      />
    </div>
  </div>
};

[@react.component]
let make = () => {
  let (asInput, setAsInput) =
    React.useState(() => LanguageJson.abstractSyntax);

  module Parseable_abstract_syntax' =
    ParseStatus.Make(Parsing.Parseable_abstract_syntax);
  let (languageView, language) = Parseable_abstract_syntax'.parse(asInput);

  let termInput = switch (language) {
    | Ok(language') => React.string("term parser to go here")
    | Error(_) => languageView
  };

  <div>
    <div className="abstract-syntax-pane">
      <CodeMirror
        value=asInput
        onBeforeChange=((_, _, str) => setAsInput(_ => str))
        options=CodeMirror.options(~mode="default", ())
      />
    </div>
    {termInput}
  </div>
};

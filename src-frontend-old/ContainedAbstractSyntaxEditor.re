[@react.component]
let make = (
  ~onUpdate : Belt.Result.t(Types.abstract_syntax, string) => unit,
  ~initialInput: string
  ) => {

  let (asInput, setAsInput) = React.useState(() => initialInput);
  let (hasSubmitted, setSubmitted) = React.useState(() => false);

  module Parseable_abstract_syntax' =
    ParseStatus.Make(Parsing.Parseable_abstract_syntax);
  let (languageView, language) = Parseable_abstract_syntax'.parse(asInput);

  let handleKey = (_editor, evt) => {
    if (CodeMirror.metaKeyGet(evt) && CodeMirror.keyGet(evt) == "Enter") {
      CodeMirror.preventDefault(evt);
      onUpdate(language);
      setSubmitted(_ => true);
    }
  };

  <div className="abstract-syntax-pane">
    (hasSubmitted ? languageView : React.null)
    <CodeMirror
      value=asInput
      onBeforeChange=((_, _, str) => setAsInput(_ => str))
      options=CodeMirror.options(~mode="default", ())
      onKeyDown=handleKey
    />
  </div>
};

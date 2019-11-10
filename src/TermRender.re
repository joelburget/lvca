module P_term = Parsing.Incremental(Parsing.Parseable_term)

let startStr = {|document([
  header(h2(); "foo");
  paragraph(inline([inlineAtom([]; "paragraph text")]))
])|};


module TermRender = {
  [@react.component]
  let make = () => {
    let (str, setStr) = React.useState(() => startStr);

    let elem = switch (P_term.parse(str)) {
      | Ok(tm) => switch (LanguageDocumentRender.render_tm(tm)) {
        | Ok(elem) => elem
        | Error(msg) => React.string(msg)
      }
      | Error(msg) => React.string(msg)
    };

    <div>
      <textarea
        onKeyUp=(event => if (ReactEvent.Keyboard.key(event) == "Enter" &&
                              (ReactEvent.Keyboard.ctrlKey(event) ||
                               ReactEvent.Keyboard.metaKey(event))) {
          setStr(ReactEvent.Keyboard.target(event)##value);
        })
        defaultValue=str
        rows=20
        cols=80
      />
      <div>{elem}</div>
    </div>
  }
};

ReactDOMRe.renderToElementWithId(<TermRender />, "index");

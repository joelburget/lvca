[@react.component]
let make = (
  ~eval: (Binding.Nominal.term => Tablecloth.Result.t(string, React.element)),
  ~startStr: string
  ) => {
  module P_term = Parsing.Incremental(Parsing.Parseable_term);
  let (str, setStr) = React.useState(() => startStr);

  let elem = P_term.parse(str)
    |. Belt.Result.flatMap(eval)
    |. Util.get_result(React.string)
  ;

  let handleKeyDown = event => {
    if (ReactEvent.Keyboard.key(event) == "Enter" &&
        (ReactEvent.Keyboard.ctrlKey(event) ||
         ReactEvent.Keyboard.metaKey(event))) {
      setStr(ReactEvent.Keyboard.target(event)##value);
    }
  };

  <div>
    <textarea
      onKeyDown=handleKeyDown
      defaultValue=str
      rows=20
      cols=80
    />
    <div>{elem}</div>
  </div>
}

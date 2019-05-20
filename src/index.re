module JsonViewer = {
  let rec make_inner(json:Json.value) = switch (json) {
        | `Assoc(assocs) => ReactDOMRe.createDOMElementVariadic(
            "span",
            ~props=ReactDOMRe.domProps(),
            Array.concat([
              [| React.string("{") |],
              Array.of_list(List.map(
                ((k, v)) => <span> {React.string(k)} {make_inner(v)} </span>,
                assocs
              )),
              [| React.string("}") |]
            ])
          )
        | `Bool(true)  => React.string("true")
        | `Bool(false) => React.string("false")
        | `Float(num)  => React.string(Js.Float.toString(num))
        | `Int(i)      => React.string(string_of_int(i))
        | `List(vals)  => ReactDOMRe.createDOMElementVariadic(
            "span",
            ~props=ReactDOMRe.domProps(),
            Array.concat([
              [| React.string("[") |],
              Array.of_list(List.map(make_inner, vals)),
              [| React.string("]") |]
            ])
          )
        | `Null        => React.string("null")
        | `String(str) => React.string(str)
      };

  [@react.component]
  let make = (~json:Json.value) => {
    <div>{make_inner(json)}</div>
  };
};

module LvcaViewer = {
  /* let style = ReactDOMRe.Style.make(~height="500px", ()); */

  type action =
    | Type(string)
    ;

  [@react.component]
  let make = () => {
    open Belt.Result;
    let (text, setText) = React.useState(() => "");
    let jsonResult = switch (JsonParser.prog(JsonLexer.read, Lexing.from_string(text))) {
          | Some(json)                           => Ok(json)
          | None                                 => Error("empty")
          | exception JsonLexer.SyntaxError(msg) => Error(msg)
          | exception Parsing.Parse_error        => Error("Parse error")
          };

    let jsonView = switch (jsonResult) {
          | Ok(json)   => <JsonViewer json=json />
          | Error(msg) => React.string(msg)
          };

    <div>
      <h1>{React.string("LVCA")}</h1>
      <textarea value=text onChange=(event => setText(ReactEvent.Form.target(event)##value)) />
      {jsonView}
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<LvcaViewer />, "index");

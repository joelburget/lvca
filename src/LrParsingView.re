open LrParsing;

module Grammar = {
  [@react.component]
  let make = (~grammar : grammar, ~states : array((int, string, string))) => {
    /* TODO: make table, sort */
    let terminalElems = grammar.terminal_nums
      |. Belt.SortArray.stableSortBy
        (((_, num), (_', num')) => Pervasives.compare(num, num'))
      |. Belt.Array.map (((name, num)) =>
        <tr>
          <td>{React.string(string_of_int(num))}</td>
          <td>{React.string(name)}</td>
        </tr>
      );

    let terminalsView = ReactDOMRe.createDOMElementVariadic(
      "tbody",
      ~props=ReactDOMRe.domProps(),
      terminalElems
    );

    let stateElems = states
      |. Belt.Array.map(((num, kernel_items, nonkernel_items)) =>
        <tr>
          <td>{React.string(string_of_int(num))}</td>
          <td>
            <pre className="kernel-items"><code>
              {React.string(kernel_items)}
            </code></pre>
            <pre className="nonkernel-items"><code>
              {React.string(nonkernel_items)}
            </code></pre>
          </td>
        </tr>
      );

    let stateElemsView = ReactDOMRe.createDOMElementVariadic(
      "tbody",
      ~props=ReactDOMRe.domProps(),
      stateElems
    );

    <div>
      <h2>{React.string("terminals")}</h2>
      <table>
        <thead>
          <tr>
            <th>{React.string("number")}</th>
            <th>{React.string("symbol")}</th>
          </tr>
        </thead>
        terminalsView
      </table>

      <h2>{React.string("states")}</h2>
      <table>
        <thead>
          <tr>
            <th>{React.string("state number")}</th>
            <th>{React.string("elements")}</th>
          </tr>
        </thead>
        stateElemsView
      </table>
    </div>
  }
}

module Tables = (Lr0 : LR0) => {
  [@react.component]
  let make = (~action_table : array(array(action)),
              ~goto_table : array(array((symbol, option(state)))))
    => {
    let (concat, length, map, mapWithIndex, zip) =
      Belt.Array.(concat, length, map, mapWithIndex, zip);

    assert(length(action_table) == length(goto_table));

    let shared_table = zip(action_table, goto_table);
    let action_span = length(action_table[0]);
    let goto_span = length(goto_table[0]);

    let action_headers = action_table[0]
      |. mapWithIndex((i, _) => {
        <th>{React.string(Lr0.string_of_terminal(i))}</th>
      });
    let goto_headers = goto_table[0]
      |. map(((symbol, _)) =>
        <th>{React.string(Lr0.string_of_symbol(symbol))}</th>
      );

    let headers_row = ReactDOMRe.createDOMElementVariadic(
      "tr",
      ~props=ReactDOMRe.domProps(),
      concat(action_headers, goto_headers)
    );

    let data = shared_table
      |. mapWithIndex((i, (action_row, goto_row)) => {
        let action_row_elems = action_row
          |. map(action =>
            <td>{React.string(LrParsing.action_abbrev(action))}</td>
          );

        let goto_row_elems = goto_row
          |. map(((_symbol, m_state)) => switch (m_state) {
            | Some(state) => string_of_int(state)
            | None => ""
          })
          |. map(x => <td>{React.string(x)}</td>);

        ReactDOMRe.createDOMElementVariadic(
          "tr",
          ~props=ReactDOMRe.domProps(),
          concat (
            [| <td>{React.string(string_of_int(i))}</td> |],
            concat(action_row_elems, goto_row_elems)
          )
        )
      });

    let tbody = ReactDOMRe.createDOMElementVariadic(
      "tbody",
      ~props=ReactDOMRe.domProps(),
      data
    );

    <div>
      <h2>{React.string("action / goto tables")}</h2>
      <table>
        <thead>
          <tr>
            <th rowSpan=2>{React.string("state")}</th>
            <th colSpan=action_span>{React.string("action")}</th>
            <th colSpan=goto_span>{React.string("goto")}</th>
          </tr>
          headers_row
        </thead>
        tbody
      </table>
    </div>
  }
}

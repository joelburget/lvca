open LrParsing;

module Grammar = (G : GRAMMAR, Lalr : LalrParsing.LALR) => {
  [@react.component]
  let make = () => {
    let grammar = G.grammar;

    let (showNonkernel, setShowNonkernel) =
      React.useState(() => Lalr.states |. Belt.Array.map(_ => false));

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

    let stateElems = Lalr.states |. Belt.Array.mapWithIndex((ix, state) => {
        let kernel_items = Lalr.state_to_lookahead_item_set(state);
        let { LalrParsing.nonkernel_items }
          = Lalr.lr1_closure'(kernel_items);

        let button = if (!Belt.Set.isEmpty(nonkernel_items)) {
          <button onClick=(_ => setShowNonkernel(_ => {
            let showNonkernel' = Belt.Array.copy(showNonkernel);
            Belt.Array.setExn(showNonkernel', ix, !showNonkernel[ix]);
            showNonkernel'
          }))>
            (React.string(showNonkernel[ix] ? "hide nonkernel items" : "show nonkernel items"))
          </button>
        } else {
          React.null
        };

        <tr>
          <td>{React.string(string_of_int(state))}</td>
          <td>
            <pre className="kernel-items"><code>
              (React.string(Lalr.string_of_lookahead_item_set(kernel_items)))
              (button)
            </code></pre>
            (if (showNonkernel[ix]) {
              <pre className="nonkernel-items"><code>
                {React.string(Lalr.string_of_lookahead_item_set(nonkernel_items))}
              </code></pre>
             } else {
               React.null
             }
            )
          </td>
        </tr>
      });

    <div>
      <h2>{React.string("terminals")}</h2>
      <table>
        <thead>
          <tr>
            <th>{React.string("number")}</th>
            <th>{React.string("symbol")}</th>
          </tr>
        </thead>
        (ReactUtil.make_elem("tbody", terminalElems))
      </table>

      <h2>{React.string("states")}</h2>
      <table>
        <thead>
          <tr>
            <th>{React.string("state number")}</th>
            <th>{React.string("elements")}</th>
          </tr>
        </thead>
        (ReactUtil.make_elem("tbody", stateElems))
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

        ReactUtil.make_elem(
          "tr",
          concat (
            [| <td>{React.string(string_of_int(i))}</td> |],
            concat(action_row_elems, goto_row_elems)
          )
        )
      });

    <div>
      <h2>{React.string("action / goto tables")}</h2>
      <table>
        <thead>
          <tr>
            <th rowSpan=2>{React.string("state")}</th>
            <th colSpan=action_span>{React.string("action")}</th>
            <th colSpan=goto_span>{React.string("goto")}</th>
          </tr>
          (ReactUtil.make_elem("tr", concat(action_headers, goto_headers)))
        </thead>
        (ReactUtil.make_elem("tbody", data))
      </table>
    </div>
  }
}

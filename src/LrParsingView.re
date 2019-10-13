open LrParsing;
module BA = Belt.Array;
module MS = Belt.Map.String;

/*
let string_of_grammar : grammar -> string
  = failwith "TODO"
  */

module Grammar = {
  [@react.component]
  let make = (~grammar : grammar) => {
    <div>
      <h2>{React.string("terminals")}</h2>
      /* TODO */
      <h2>{React.string("states")}</h2>
      /* TODO */
    </div>
  }
}

module Tables = {
  [@react.component]
  let make = (~grammar : grammar,
              ~action_table : array(array(action)),
              ~goto_table : array(array((symbol, option(state)))))
    => {
    let (concat, length, map, mapWithIndex, zip) =
      BA.(concat, length, map, mapWithIndex, zip);

    assert(length(action_table) == length(goto_table));

    let lookup_terminal_name = i =>
      switch (MS.findFirstBy(grammar.terminal_names,
                         ((_, num) => num == i))) {
        | Some((name, _)) => name
        | None => "T" ++ string_of_int(i)
      };

    let lookup_nonterminal_name = i =>
      switch (MS.findFirstBy(grammar.nonterminal_names,
                         ((_, num) => num == i))) {
        | Some((name, _)) => name
        | None => "NT" ++ string_of_int(i)
      };

    let shared_table = zip(action_table, goto_table);
    let action_span = length(action_table[0]);
    let goto_span = length(goto_table[0]);

    let action_headers = action_table[0]
      |. mapWithIndex((i, _) => {
        <th>{React.string(lookup_terminal_name(i))}</th>
      });
    let goto_headers = goto_table[0]
      |. map(((symbol, _)) => switch (symbol) {
        | Terminal(num) => lookup_terminal_name(num)
        | Nonterminal(num) => lookup_nonterminal_name(num)
      })
      |. map(x => <th>{React.string(x)}</th>);

    let headers_row = ReactDOMRe.createDOMElementVariadic(
      "tr",
      ~props=ReactDOMRe.domProps(),
      concat(action_headers, goto_headers)
    );

    let data = shared_table
      |. map(((action_row, goto_row)) => {
        let action_row_elems = action_row
          |. map(action => switch (action) {
            | Shift(n) => "s" ++ string_of_int(n)
            | Reduce(n) => "r" ++ string_of_int(n)
            | Accept => "acc"
            | Error => "err"
          })
          |. map(x => <td>{React.string(x)}</td>);

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
            [| <td></td> |],
            concat(action_row_elems, goto_row_elems)
          )
        )
      });

    let tbody = ReactDOMRe.createDOMElementVariadic(
      "tbody",
      ~props=ReactDOMRe.domProps(),
      data
    );

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
  }
}

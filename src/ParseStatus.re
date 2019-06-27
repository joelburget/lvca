/* */
open Belt.Result;

module Component = {
  [@react.component]
  let make = (~result) => {
    switch (result) {
    | Ok(_parsed) =>
      <span className="result-good">
        {React.string("(parsed)")}
      </span>
    | Error(msg)  =>
      <span className="result-bad">
        {React.string(msg)}
      </span>
    }
  };
};

module Make = (Parse: Parsing.Parseable) => {

  module Parse' = Parsing.Incremental(Parse);

  let parse = fun(input) => {
    let result = Parse'.parse(input);
    (<Component result=result />, result)
  }

};

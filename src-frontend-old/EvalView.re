// TODO: duplicated in index
type located_err = (string, option(Binding.DeBruijn.term))
type translation_result('a) = Belt.Result.t('a, located_err)
type input = translation_result(Binding.Nominal.term);
type eval_result  = translation_result(Core.core);

[@react.component]
let make = (~input: input, ~evalResult: eval_result) => {
  switch (evalResult) {
  | Ok(coreVal)
    => let ast = Core.to_ast(coreVal);
       let hash = switch (input) {
         | Error((msg, _)) => msg
         | Ok(tm)          => "#" ++ String.sub(Binding.Nominal.hash(tm), 0, 8)
       };

    <div className="eval-result-row">
      <div className="eval-result">
        {React.string(Binding.Nominal.pp_term'(ast))}
      </div>
      <div className="eval-result-hash">
        {React.string(hash)}
      </div>
    </div>

  | Error((msg, None))
    => <div className="error"> {React.string(msg)} </div>
  | Error((msg, Some(tm)))
    => let ast_view = switch (Binding.DeBruijn.to_nominal(tm)) {
         | None => <div />
         | Some (ast_tm)
         => <div>{React.string(Binding.Nominal.pp_term'(ast_tm))}</div>
       };

    <div className="error">
      <div>{React.string(msg)}</div>
      {ast_view}
    </div>
  };
}

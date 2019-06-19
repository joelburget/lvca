// TODO: duplicated in index
type item_result = Types.Core.translation_result(Types.Core.core_val);

[@react.component]
let make = (~evalResult: item_result) => {
  switch (evalResult) {
  | Ok(coreVal)
    => let ast = Types.Core.val_to_ast(coreVal);
       <div>{React.string(Types.Ast.pp_term'(ast))}</div>
  | Error((msg, None))
    => <div className="error"> {React.string(msg)} </div>
  | Error((msg, Some(tm)))
    => let ast_view = switch (Types.Abt.to_ast(tm)) {
         | None => <div />
         | Some (ast_tm)
         => <div>{React.string(Types.Ast.pp_term'(ast_tm))}</div>
       };
    <div className="error">
      <div>{React.string(msg)}</div>
      {ast_view}
    </div>
  };
}

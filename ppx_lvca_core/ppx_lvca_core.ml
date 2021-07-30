open Ppxlib

let expand_core ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = Syntax_quoter.extract_string ~loc expr in
  match
    Lvca_parsing.(
      parse_string (whitespace *> Lvca_core.Parse.term ~comment:c_comment) str)
  with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Core_syntax_quoter.Core.term ~loc tm
;;

let core_extension =
  Extension.declare
    "lvca.core"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_core
;;

let () =
  Ppxlib.Driver.register_transformation
    "lvca_core"
    ~rules:[ Context_free.Rule.extension core_extension ]
;;

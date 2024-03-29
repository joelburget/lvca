open Ppxlib

let expand_core ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = Syntax_quoter.extract_string ~loc expr in
  match Lvca_parsing.(parse_string (whitespace *> Lvca_del.Core.Parse.term) str) with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Core_syntax_quoter.Core.term ~loc tm
;;

let core_extension =
  Extension.declare
    "lvca.del.core"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_core
;;

let () =
  Ppxlib.Driver.register_transformation
    "lvca_del_core"
    ~rules:[ Context_free.Rule.extension core_extension ]
;;

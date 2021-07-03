open Lvca_syntax
open Ppxlib

let expand_core ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match parse Lvca_core.Term.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Syntax_quoter.Exp.Core.term ~loc tm
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
    "lvca"
    ~rules:[ Context_free.Rule.extension core_extension ]
;;

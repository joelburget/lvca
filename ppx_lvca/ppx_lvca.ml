open Lvca_syntax
open Ppxlib
open Syntax_quoter

let parse p = Lvca_parsing.(parse_string (whitespace *> p))

let expand_nominal ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string ~loc expr in
  match parse Nominal.Term.parse' str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Exp.nominal ~loc tm
;;

(*
let expand_nonbinding ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string ~loc expr in
  match parse Nonbinding.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Exp.nonbinding ~loc tm
;;
   *)

let expand_pattern ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string ~loc expr in
  match parse Pattern.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Exp.pattern ~loc tm
;;

let expand_abstract_syntax ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string ~loc expr in
  match parse Abstract_syntax.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> Exp.language ~loc syntax
;;

let expand_module ~(loc : Location.t) ~path:_ (lang_str_expr : expression) : module_expr =
  let str, loc = extract_string ~loc lang_str_expr in
  match parse Abstract_syntax.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax ->
    let module Builder_context = struct
      let buf = str

      module Ast = Ast_builder.Make (struct
        let loc = loc
      end)
    end
    in
    let module Container_module = Module_builder.Container_module (Builder_context) in
    Container_module.mk syntax
;;

let expand_module_sig ~(loc : Location.t) ~path:_ (expr : expression) : module_type =
  let str, loc = extract_string ~loc expr in
  match parse Abstract_syntax.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax ->
    let module Builder_context = struct
      let buf = str

      module Ast = Ast_builder.Make (struct
        let loc = loc
      end)
    end
    in
    let module Sig = Module_builder.Sig (Builder_context) in
    Sig.mk syntax
;;

let term_extension =
  Extension.declare
    "lvca.nominal"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_nominal
;;

(*
let nonbinding_extension =
  Extension.declare
    "lvca.nonbinding"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_nonbinding
;;
*)

let pattern_extension =
  Extension.declare
    "lvca.pattern"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_pattern
;;

let abstract_syntax_extension =
  Extension.declare
    "lvca.abstract_syntax"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_abstract_syntax
;;

let abstract_syntax_module_extension =
  Extension.declare
    "lvca.abstract_syntax_module"
    Extension.Context.Module_expr
    Ast_pattern.(single_expr_payload __)
    expand_module
;;

let abstract_syntax_module_sig_extension =
  Extension.declare
    "lvca.abstract_syntax_module_sig"
    Extension.Context.Module_type
    Ast_pattern.(single_expr_payload __)
    expand_module_sig
;;

let () =
  Ppxlib.Driver.register_transformation
    "lvca"
    ~rules:
      [ Context_free.Rule.extension term_extension
        (* ; Context_free.Rule.extension nonbinding_extension *)
      ; Context_free.Rule.extension pattern_extension
      ; Context_free.Rule.extension abstract_syntax_extension
      ; Context_free.Rule.extension abstract_syntax_module_extension
      ; Context_free.Rule.extension abstract_syntax_module_sig_extension
      ]
;;

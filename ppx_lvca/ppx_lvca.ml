open Lvca_syntax
open Ppxlib

(* TODO: parser, core, nonbinding / OCaml data mapping *)

let extract_string loc expr =
  (* payload and location of the string contents, inside "" or {||} *)
  let adjust shift loc =
    let adjust shift p = { p with Lexing.pos_cnum = p.pos_cnum + shift } in
    { loc with
      Location.loc_start = adjust shift loc.loc_start
    ; Location.loc_end = adjust (-shift) loc.loc_end
    }
  in
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (str, _loc, None)) -> str, adjust 1 expr.pexp_loc
  | Pexp_constant (Pconst_string (str, _loc, Some x)) ->
    str, adjust (String.length x + 2) expr.pexp_loc
  | _ -> Location.raise_errorf ~loc "Expecting string payload"
;;

let expand_nominal ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match Lvca_parsing.parse_string Nominal.Term.Parse.whitespace_t str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Syntax_quoter.mk_nominal ~loc tm
;;

let expand_nonbinding ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match Lvca_parsing.parse_string Nonbinding.Parse.whitespace_term str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Syntax_quoter.mk_nonbinding ~loc tm
;;

let expand_pattern ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match Lvca_parsing.parse_string Lvca_syntax.Pattern.Parse.whitespace_t str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Syntax_quoter.mk_pattern ~loc tm
;;

let expand_abstract_syntax ~(loc : Location.t) ~path:_ (expr : expression) : expression =
  let str, loc = extract_string loc expr in
  match Lvca_parsing.parse_string Abstract_syntax.Parse.whitespace_t str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> Syntax_quoter.mk_language ~loc syntax
;;

let expand_module ~(loc : Location.t) ~path:_ (expr : expression) : module_expr =
  let str, loc = extract_string loc expr in
  (*
  Fmt.pr "contextual loc: ";
  Location.print Fmt.stdout loc;
  Fmt.pr "\n";
  *)
  match Lvca_parsing.parse_string Abstract_syntax.Parse.whitespace_t str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax ->
    let module Container_module =
      Module_builder.Container_module (struct
        let buf = str

        module Ast = Ast_builder.Make (struct
          let loc = loc
        end)
      end)
    in
    Container_module.mk syntax
;;

let term_extension =
  Extension.declare
    "lvca_nominal"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_nominal
;;

let nonbinding_extension =
  Extension.declare
    "lvca_nonbinding"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_nonbinding
;;

let pattern_extension =
  Extension.declare
    "lvca_pattern"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_pattern
;;

let abstract_syntax_extension =
  Extension.declare
    "lvca_abstract_syntax"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_abstract_syntax
;;

let abstract_syntax_module_extension =
  Extension.declare
    "abstract_syntax_module" (* TODO: better naming *)
    Extension.Context.Module_expr
    Ast_pattern.(single_expr_payload __)
    expand_module
;;

let () =
  Ppxlib.Driver.register_transformation
    "lvca"
    ~rules:
      [ Context_free.Rule.extension term_extension
      ; Context_free.Rule.extension nonbinding_extension
      ; Context_free.Rule.extension pattern_extension
      ; Context_free.Rule.extension abstract_syntax_extension
      ; Context_free.Rule.extension abstract_syntax_module_extension
      ]
;;

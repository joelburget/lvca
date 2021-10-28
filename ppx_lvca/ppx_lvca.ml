open Base
open Lvca_syntax
open Ppxlib
open Syntax_quoter

let parse p = Lvca_parsing.(parse_string (whitespace *> p))

let parse_module_name =
  let open Lvca_parsing in
  sep_by1
    (No_junk.char '.')
    (No_junk.satisfy Char.is_uppercase
    >>= fun c0 ->
    many (C_comment_parser.satisfy Char.(fun c -> is_alphanum c || c = '_' || c = '\''))
    >>| fun cs -> String.of_char_list (c0 :: cs))
;;

let expand_nominal ~(loc : Location.t) ~path:_ (str : string) : expression =
  match parse Nominal.Term.parse' str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Exp.nominal ~loc tm
;;

let expand_nonbinding ~(loc : Location.t) ~path:_ str : expression =
  match parse Nonbinding.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Exp.nonbinding ~loc tm
;;

let expand_pattern ~(loc : Location.t) ~path:_ str : expression =
  match parse Pattern.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok tm -> Exp.pattern ~loc tm
;;

let mk_module_mapping ~loc exprs =
  exprs
  |> List.map ~f:(fun (sort_ident, expr) ->
         match sort_ident.txt with
         | Lident sort_name ->
           (match expr.pexp_desc with
           | Pexp_constant (Pconst_string (module_name, _, _)) ->
             (match parse parse_module_name module_name with
             | Error msg ->
               Location.raise_errorf ~loc "failed to parse module name: %s" msg
             | Ok modules -> sort_name, modules)
           | _ -> Location.raise_errorf ~loc "expected a constant string")
         | _ -> Location.raise_errorf ~loc "expected an identifier")
  |> Lvca_util.String.Map.of_alist_exn
;;

let check_expr_opt ~loc expr_opt =
  match expr_opt with
  | None -> ()
  | Some _ ->
    Location.raise_errorf
      ~loc
      "This record is an abuse for specifying modules only -- it can't be an extension"
;;

let expand_abstract_syntax ~(loc : Location.t) ~path:_ str : expression =
  match parse Abstract_syntax.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> Exp.language ~loc syntax
;;

let expand_module ~(loc : Location.t) ~path:_ str exprs expr_opt : module_expr =
  let module Builder_context = struct
    let buf = str

    module Ast = Ast_builder.Make (struct
      let loc = loc
    end)
  end
  in
  let module Container_module = Module_builder.Container_module (Builder_context) in
  check_expr_opt ~loc expr_opt;
  match parse Abstract_syntax.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> Container_module.mk ~module_mapping:(mk_module_mapping ~loc exprs) syntax
;;

let expand_module_sig ~(loc : Location.t) ~path:_ str exprs expr_opt : module_type =
  let module Builder_context = struct
    let buf = str

    module Ast = Ast_builder.Make (struct
      let loc = loc
    end)
  end
  in
  let module Sig = Module_builder.Sig (Builder_context) in
  check_expr_opt ~loc expr_opt;
  match parse Abstract_syntax.parse str with
  | Error msg -> Location.raise_errorf ~loc "%s" msg
  | Ok syntax -> Sig.mk ~module_mapping:(mk_module_mapping ~loc exprs) syntax
;;

let term_extension =
  Extension.declare
    "lvca.nominal"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_nominal
;;

let nonbinding_extension =
  Extension.declare
    "lvca.nonbinding"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_nonbinding
;;

let pattern_extension =
  Extension.declare
    "lvca.pattern"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_pattern
;;

let payload () =
  let f g str = g str [] None in
  Ast_pattern.(
    single_expr_payload
      (pexp_tuple (estring __ ^:: pexp_record __ __ ^:: nil) ||| map (estring __) ~f))
;;

let abstract_syntax_extension =
  Extension.declare
    "lvca.abstract_syntax"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_abstract_syntax
;;

let abstract_syntax_module_extension =
  Extension.declare
    "lvca.abstract_syntax_module"
    Extension.Context.Module_expr
    (payload ())
    expand_module
;;

let abstract_syntax_module_sig_extension =
  Extension.declare
    "lvca.abstract_syntax_module_sig"
    Extension.Context.Module_type
    (payload ())
    expand_module_sig
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
      ; Context_free.Rule.extension abstract_syntax_module_sig_extension
      ]
;;

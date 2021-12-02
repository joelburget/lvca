(* Decisions:
 * - always allow parens
 * - common whitespace definition
 *   - how to define required / optional?
 * - how to define boxes?
 *)
open Base

let reserved_words = Lvca_util.String.Set.empty
let lower_ident = Lvca_parsing.C_comment_parser.lower_identifier reserved_words

module Sequence_item = struct
  type t =
    | Var of Provenance.t * string
    | Literal of Provenance.t * string

  let info = function Var (i, _) | Literal (i, _) -> i

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    choice
      ~failure_msg:"looking for a variable or literal"
      [ (lower_ident >>~ fun range str -> Var (Provenance.of_range range, str))
      ; (string_lit >>~ fun range str -> Literal (Provenance.of_range range, str))
      ]
    <?> "sequence item"
  ;;

  let pp ppf t =
    let info = info t in
    Provenance.open_stag ppf info;
    (match t with
    | Var (_, name) -> Fmt.string ppf name
    | Literal (_, str) -> Fmt.pf ppf "%S" str);
    Provenance.close_stag ppf info
  ;;
end

module Syntax = struct
  type t =
    | Regex of Provenance.t * Regex.t
    | Sequence of Provenance.t * Sequence_item.t list

  let info = function Regex (i, _) | Sequence (i, _) -> i

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    choice
      ~failure_msg:"looking for a regex or sequence"
      [ (many1 Sequence_item.parse
        >>~ fun range items -> Sequence (Provenance.of_range range, items))
      ; (char '/' *> of_angstrom Regex.parse
        <* char '/'
        >>~ fun range re -> Regex (Provenance.of_range range, re))
      ]
    <?> "syntax"
  ;;

  let pp ppf t =
    let info = info t in
    Provenance.open_stag ppf info;
    (match t with
    | Regex (_, re) -> Fmt.pf ppf "/%a/" Regex.pp re
    | Sequence (_, sequence_items) ->
      Fmt.(pf ppf "@[%a@]" (list Sequence_item.pp ~sep:sp) sequence_items));
    Provenance.close_stag ppf info
  ;;
end

module Operator_syntax = struct
  type t =
    { info : Provenance.t
    ; pattern : Pattern.t
    ; syntax : Syntax.t
    }

  let info t = t.info

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    lift2
      (fun (pat_range, pattern) (syntax_range, syntax) ->
        let range = Lvca_provenance.Opt_range.union pat_range syntax_range in
        let info = Provenance.of_range range in
        { info; pattern; syntax })
      (attach_pos' (Pattern.parse reserved_words) <* char '~')
      (attach_pos' Syntax.parse)
    <?> "operator syntax"
  ;;

  let pp ppf { info; pattern; syntax } =
    Provenance.open_stag ppf info;
    Fmt.pf ppf "%a ~ %a" Pattern.pp pattern Syntax.pp syntax;
    Provenance.close_stag ppf info
  ;;
end

module Sort_syntax = struct
  type t =
    { info : Provenance.t
    ; name : Provenance.t * string
    ; variables : (Provenance.t * string) list
    ; operators : Operator_syntax.t list
    }

  let parse =
    let open Lvca_parsing in
    let open C_comment_parser in
    lift3
      (fun (name_info, name) (vars_info, variables) (ops_info, operators) ->
        let info =
          [ name_info; vars_info; ops_info ]
          |> Lvca_provenance.Opt_range.list_range
          |> Provenance.of_range
        in
        let name = Provenance.of_range name_info, name in
        let variables =
          List.map variables ~f:(fun (pos, ident) -> Provenance.of_range pos, ident)
        in
        { info; name; variables; operators })
      (attach_pos' lower_ident)
      (attach_pos' (parens (many (attach_pos' lower_ident)) <* char ':'))
      (attach_pos' (many Operator_syntax.parse))
    <?> "sort syntax"
  ;;

  let pp_name ppf (info, name) =
    Provenance.open_stag ppf info;
    Fmt.string ppf name;
    Provenance.close_stag ppf info
  ;;

  let pp ppf { info; name; variables; operators } =
    let open Fmt in
    Provenance.open_stag ppf info;
    pf
      ppf
      "%a(%a):%a"
      pp_name
      name
      (list pp_name)
      variables
      (list Operator_syntax.pp)
      operators;
    Provenance.close_stag ppf info
  ;;
end

type t = Sort_syntax.t list

let parse =
  let open Lvca_parsing in
  many Sort_syntax.parse <?> "parse / pretty definition"
;;

let pp = Fmt.(list Sort_syntax.pp ~sep:cut)

let%test_module "parsing / pretty-printing" =
  (module struct
    let () = Stdlib.Format.set_tags false

    let%test_module "Sequence_item" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Sequence_item.parse
        let parse_print str = Fmt.pr "%a" Sequence_item.pp (parse str)

        let%expect_test _ =
          parse_print "a";
          [%expect {|a|}]
        ;;

        let%expect_test _ =
          parse_print {|"foo"|};
          [%expect {|"foo"|}]
        ;;
      end)
    ;;

    let%test_module "Syntax" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Syntax.parse
        let parse_print str = Fmt.pr "%a" Syntax.pp (parse str)

        let%expect_test _ =
          parse_print {|x "+" y|};
          [%expect {|x "+" y|}]
        ;;

        let%expect_test _ =
          parse_print {|/[a-z][a-zA-Z0-9_]*/|};
          [%expect {|/[a-z][a-zA-Z0-9_]*/|}]
        ;;
      end)
    ;;

    let%test_module "Operator_syntax" =
      (module struct
        let parse = Lvca_parsing.parse_string_or_failwith Operator_syntax.parse
        let parse_print str = Fmt.pr "%a" Operator_syntax.pp (parse str)

        let%expect_test _ =
          parse_print {|Add(x; y) ~ x "+" y|};
          [%expect {|Add(x; y) ~ x "+" y|}]
        ;;

        let%expect_test _ =
          parse_print {|x ~ /[a-z][a-zA-Z0-9_]*/|};
          [%expect {|x ~ /[a-z][a-zA-Z0-9_]*/|}]
        ;;
      end)
    ;;

    (*
    let parse = Lvca_parsing.parse_string_or_failwith Operator_syntax.parse
    let parse_print str = Fmt.pr "%a" Operator_syntax.pp (parse str)

    let defn =
      {|
expr(x, y):
  | Add(x; y) ~ x "+" y
  | Mul(x; y) ~ x "*" y
  | x         ~ /[a-z][a-zA-Z0-9_]*/
    |}
    ;;

    let%expect_test _ =
      parse_print defn;
      [%expect]
    ;;
       *)
  end)
;;

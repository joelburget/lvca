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
    | Regex of Provenance.t * Re.t
    | Sequence of Provenance.t * Sequence_item.t list

  let info = function Regex (i, _) | Sequence (i, _) -> i

  let parse =
    let open Lvca_parsing in
    choice
      ~failure_msg:"looking for a regex or sequence"
      [ (many1 Sequence_item.parse
        >>~ fun range items -> Sequence (Provenance.of_range range, items))
        (* TODO: parse regexes *)
      ]
  ;;

  let pp ppf t =
    let info = info t in
    Provenance.open_stag ppf info;
    (match t with
    | Regex (_, re) -> Fmt.pf ppf "/%a/" Re.pp re
    | Sequence (_, sequence_items) -> Fmt.list Sequence_item.pp ppf sequence_items);
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

let%test_module _ =
  (module struct
    let defn =
      {|
expr(x, y):
  | Add(x; y) ~ x "+" y
  | Mul(x; y) ~ x "*" y
  | x         ~ /[a-z][a-zA-Z0-9_]*/
    |}
    ;;
  end)
;;

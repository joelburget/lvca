open Base
open Lvca_syntax

let abstract_syntax_str =
  {|
{ list }

maybe(a) :=
  | nothing()
  | some(a)

// An edit in some language is either:
edit(lang) :=
  // A simple, atomic edit
  | atomic(core(lang; maybe(lang)))
  // Or an edit with a message attached
  | labeled(edit(); string())
  // Or a list of edits.
  | list(list(edit))
|}
;;

module ParseAbstract = AbstractSyntax.Parse (ParseUtil.CComment)

let abstract_syntax : AbstractSyntax.t =
  abstract_syntax_str
  |> ParseUtil.parse_string ParseAbstract.whitespace_t
  |> Result.ok_or_failwith
;;

type core = Core.term

type t =
  | Atomic of core
  | Labeled of t * string (* TODO? string -> core *)
  | List of t list

let rec pp : t Fmt.t =
 fun ppf ->
  let pf = Fmt.pf in
  function
  | Atomic core -> pf ppf "%a" (Fmt.braces Core.pp) core
  | Labeled (edit, name) -> pf ppf "%s:%a" name pp edit
  | List edits -> pf ppf "%a" Fmt.(brackets (list ~sep:comma pp)) edits
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module ParseCore = Core.Parse (ParseUtil.CComment)
  open Parsers

  let t : t Parsers.t =
    fix (fun t ->
        choice
          [ braces ParseCore.term >>| (fun core -> Atomic core) <?> "core term"
          ; brackets (sep_by (char ',') t) >>| (fun ts -> List ts) <?> "list"
          ; lift3 (fun name _colon edit -> Labeled (edit, name)) identifier (char ':') t
            <?> "labeled"
          ])
    <?> "edit"
  ;;

  let whitespace_t = junk *> t
end

type term = Binding.Nominal.term

let run_atom : term -> core -> (term, Core.eval_error) Result.t =
 fun tm core -> Core.(eval (CoreApp (core, Term tm)))
;;

(* TODO: don't throw away this information, switch from strings *)
let run_atom' : term -> core -> (term, string) Result.t =
 fun tm core -> Result.map_error (run_atom tm core) ~f:(fun (msg, _tm) -> msg)
;;

let rec run : term -> t -> (term, string) Result.t =
 fun tm edit ->
  match edit with
  | Atomic core -> run_atom' tm core
  | Labeled (edit', _) -> run tm edit'
  | List edits -> List.fold_result edits ~init:tm ~f:run
;;

let%test_module "Parsing" =
  (module struct
    module ParseEdit = Parse (ParseUtil.CComment)
    module ParseTerm = Binding.Nominal.Parse (ParseUtil.CComment)

    let parse : string -> (t, string) Result.t =
      parse_string ~consume:All ParseEdit.whitespace_t
    ;;

    let parse_and_print : string -> unit =
     fun str ->
      match parse str with
      | Ok edit -> pp Caml.Format.std_formatter edit
      | Error msg -> Caml.print_string msg
   ;;

    let eval_and_print : string -> string -> unit =
     fun tm edit ->
      let open Result.Let_syntax in
      match
        let%bind edit = parse edit in
        let%bind tm = ParseUtil.parse_string ParseTerm.t tm in
        let%map tm = run tm edit in
        Binding.Nominal.pp_term Caml.Format.std_formatter tm
      with
      | Error msg -> Caml.print_string msg
      | Ok () -> ()
   ;;

    let%expect_test _ =
      parse_and_print "[]";
      [%expect {| [] |}]
    ;;

    let%expect_test _ =
      eval_and_print "foo()" "[]";
      [%expect {| foo() |}]
    ;;

    let%expect_test _ =
      parse_and_print "foo:[]";
      [%expect {| foo:[] |}]
    ;;

    let%expect_test _ =
      eval_and_print "foo()" "edit:[]";
      [%expect {| foo() |}]
    ;;

    let nested_nop =
      {|
      a:[
        b1:[
          c1:[
            d:[]
          ],
          c2:[]
        ],
        b2:[]
      ]
    |}
    ;;

    let%expect_test _ =
      parse_and_print nested_nop;
      [%expect {| a:[b1:[c1:[d:[]], c2:[]], b2:[]] |}]
    ;;

    let%expect_test _ =
      eval_and_print "foo()" nested_nop;
      [%expect {| foo() |}]
    ;;

    let replace_bar =
      {|{\(tm : lang()) -> match tm with {
      | bar() -> {baz()}
      | _ -> {foo()}
    }}|}
    ;;

    let%expect_test _ =
      parse_and_print replace_bar;
      [%expect
        {| {\(tm : lang()) -> match tm with { bar() -> {baz()} | _ -> {foo()} }} |}]
    ;;

    let%expect_test _ =
      eval_and_print "foo()" replace_bar;
      [%expect {| foo() |}]
    ;;

    let%expect_test _ =
      eval_and_print "bar()" replace_bar;
      [%expect {| baz() |}]
    ;;

    let%expect_test _ =
      parse_and_print
        {|
      sequence:
        [ replace_bar:
          {\(tm : lang()) -> match tm with {
            | bar() -> {baz()}
            | _ -> {foo()}
          }}
        , named_edit: {f}
        ]
    |};
      [%expect
        {|
      sequence:[replace_bar:{\(tm : lang()) ->
                             match tm with { bar() -> {baz()} | _ -> {foo()} }},
                named_edit:{f}] |}]
    ;;
  end)
;;

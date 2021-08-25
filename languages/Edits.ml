open Base
open Lvca_syntax
open Lvca_provenance
open Lvca_core
open Stdio

module Lang =
[%lvca.abstract_syntax_module
{|
core : *  // module Lvca_core.Lang.Term
string : *  // module Primitive.String
list : * -> *  // module Lvca_core.List_model.List

edit :=
  | Atomic(core)
  | Labeled(edit; string)
  | List(list edit)
|}]

type core = Opt_range.t Term.t

type 'lang t =
  | Atomic of 'lang
  | Labeled of 'lang t * string (* TODO? string -> core *)
  | List of 'lang t list

let rec map_t ~f = function
  | Atomic x -> Atomic (f x)
  | Labeled (x, label) -> Labeled (map_t ~f x, label)
  | List ts -> List (List.map ~f:(map_t ~f) ts)
;;

let rec pp : 'lang Fmt.t -> 'lang t Fmt.t =
 fun lang_fmt ppf ->
  let pp' = pp lang_fmt in
  function
  | Atomic core -> lang_fmt ppf (* (Fmt.braces Core.pp) *) core
  | Labeled (edit, name) -> Fmt.pf ppf "%s:%a" name pp' edit
  | List edits -> Fmt.(brackets (list ~sep:comma pp')) ppf edits
;;

let parse lang_p =
  let open Lvca_parsing in
  fix (fun t ->
      choice
        ~failure_msg:
          "looking for a core term, list (in brackets), or an identifier (label)"
        [ lang_p >>| (fun core -> Atomic core) <?> "core term"
        ; Ws.brackets (sep_by (Ws.char ',') t) >>| (fun ts -> List ts) <?> "list"
        ; lift3
            (fun name _colon edit -> Labeled (edit, name))
            Ws.identifier
            (Ws.char ':')
            t
          <?> "labeled"
        ])
  <?> "edit"
;;

type term = Opt_range.t Nominal.Term.t

let%test_module "Parsing" =
  (module struct
    let comment = Lvca_parsing.no_comment

    let parse (str : string) : (core t, string) Result.t =
      Lvca_parsing.(
        parse_string (whitespace *> parse (Ws.braces (Lvca_core.Parse.term ~comment))) str)
      |> Result.map ~f:(map_t ~f:(Term.map_info ~f:Commented.get_range))
    ;;

    let parse_and_print : string -> unit =
     fun str ->
      match parse str with
      | Ok edit -> pp (Fmt.braces Term.pp_concrete) Caml.Format.std_formatter edit
      | Error msg -> print_string msg
   ;;

    let eval_atom : term -> core -> (term, Opt_range.t eval_error) Result.t =
     fun tm core ->
      let open Lvca_core.Lang.Term in
      eval
        ~no_info:None
        (Ap (None, core, List_model.of_list ~empty_info:None [ Nominal (None, tm) ]))
   ;;

    (* TODO: don't throw away this information, switch from strings *)
    let eval_atom' : term -> core -> (term, string) Result.t =
     fun tm core -> Result.map_error (eval_atom tm core) ~f:(fun (msg, _tm) -> msg)
   ;;

    let rec eval : term -> core t -> (term, string) Result.t =
     fun tm edit ->
      match edit with
      | Atomic core -> eval_atom' tm core
      | Labeled (edit', _) -> eval tm edit'
      | List edits -> List.fold_result edits ~init:tm ~f:eval
   ;;

    let eval_and_print : string -> string -> unit =
     fun tm edit ->
      let open Result.Let_syntax in
      match
        let%bind edit = parse edit in
        let%bind tm = Lvca_parsing.parse_string (Nominal.Term.parse' ~comment) tm in
        let tm = Nominal.Term.map_info ~f:Commented.get_range tm in
        let%map tm = eval tm edit in
        Nominal.Term.pp Caml.Format.std_formatter tm
      with
      | Error msg -> print_string msg
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
      {|{\(tm : lang) -> match tm with {
      | bar() -> {baz()}
      | _ -> {foo()}
    }}|}
    ;;

    let%expect_test _ =
      parse_and_print replace_bar;
      [%expect {| {\(tm : lang) -> match tm with { bar() -> {baz()} | _ -> {foo()} }} |}]
    ;;

    let%expect_test _ =
      eval_and_print "foo()" replace_bar;
      [%expect {| foo() |}]
    ;;

    let%expect_test _ =
      eval_and_print "bar()" replace_bar;
      [%expect {| baz() |}]
    ;;

    (* let rename x y = Printf.sprintf {|{\(tm : lam) -> tm[%s := {Term_var(%S)}]}|} x y *)
    (* let rename x y = Printf.sprintf {|{\(tm : lam) -> let %s = {Term_var(%S)} in tm}|} x y *)
    let _rename x y =
      Printf.sprintf
        {|
      {\(tm : lang) -> match tm with {
        | var(old_name) -> match string.equal old_name %s with {
          | true() -> {var(%s)}
          | false() -> tm
        }
        | _ -> tm
      }}
      |}
        x
        y
    ;;

    let _rename x y =
      Printf.sprintf
        {|
      {
        let rec rename_tm = \(tm : nominal) -> match tm with {
          | Operator(name; scopes) ->
            let scopes = list.map rename_scope scopes in
            Operator(name; scopes)
          | Var(name) -> match string.equal name %s with {
            | True() -> {Var(%s)}
            | False() -> tm
          }
          | Primitive(_) -> tm
        }
        and rename_scope = \(scope : scope) -> match scope with {
          | Scope(subtms) -> {Scope(list.map rename_tm subtms)}
        }
        in
        rename_tm
      }
    |}
        x
        y
    ;;

    let rename_x_y = {|{\(tm : lang) -> rename {"x"} {"y"} tm}|}

    let%expect_test _ =
      eval_and_print "lam(x. x)" rename_x_y;
      [%expect {| lam(x. x) |}]
    ;;

    let%expect_test _ =
      eval_and_print "x" rename_x_y;
      [%expect {| y |}]
    ;;

    let%expect_test _ =
      eval_and_print "lam(z. z)" rename_x_y;
      [%expect {| lam(z. z) |}]
    ;;

    let%expect_test _ =
      eval_and_print "ap(lam(z. z); x)" rename_x_y;
      [%expect {| ap(lam(z. z); y) |}]
    ;;

    let%expect_test _ =
      eval_and_print "ap(lam(z. x); y)" rename_x_y;
      [%expect {| ap(lam(z. y); y) |}]
    ;;

    let%expect_test _ =
      parse_and_print
        {|
      sequence:
        [ replace_bar:
          {\(tm : lang) -> match tm with {
            | bar() -> {baz()}
            | _ -> {foo()}
          }}
        , named_edit: {f}
        ]
    |};
      (* TODO: This should break *)
      [%expect
        {|
          sequence:[replace_bar:{\(tm : lang) ->
                                 match tm with { bar() -> {baz()} | _ -> {foo()} }},
                    named_edit:{f}] |}]
    ;;
  end)
;;

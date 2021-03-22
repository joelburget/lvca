open Base
open Lvca_syntax
open Lvca_core
open Stdio

let abstract_syntax =
  [%lvca_abstract_syntax
    {|
maybe a :=
  | nothing()
  | some(a)

// An edit in some language is either:
edit lang :=
  // A simple, atomic edit
  | atomic(core lang (maybe lang))
  // Or an edit with a message attached
  | labeled(edit; string)
  // Or a list of edits.
  | list(list edit)
|}]
;;

(* module ParseAbstract = AbstractSyntax.Parse(ParseUtil.CComment)

   let abstract_syntax : AbstractSyntax.t = abstract_syntax_str |> ParseUtil.parse_string
   ParseAbstract.whitespace_t |> Result.ok_or_failwith *)

type core = OptRange.t Core.term

type 'lang t =
  | Atomic of 'lang
  | Labeled of 'lang t * string (* TODO? string -> core *)
  | List of 'lang t list

let rec pp : 'lang Fmt.t -> 'lang t Fmt.t =
 fun lang_fmt ppf ->
  let pp' = pp lang_fmt in
  function
  | Atomic core -> Fmt.pf ppf "%a" lang_fmt (* (Fmt.braces Core.pp) *) core
  | Labeled (edit, name) -> Fmt.pf ppf "%s:%a" name pp' edit
  | List edits -> Fmt.pf ppf "%a" Fmt.(brackets (list ~sep:comma pp')) edits
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)

  let ( junk
      , sep_by
      , identifier
      , brackets
      , choice
      , fix
      , lift3
      , ( >>| )
      , ( <?> )
      , ( *> )
      , char )
    =
    Parsers.(
      ( junk
      , sep_by
      , identifier
      , brackets
      , choice
      , fix
      , lift3
      , ( >>| )
      , ( <?> )
      , ( *> )
      , char ))
  ;;

  let t : 'lang Parsers.t -> 'lang t Parsers.t =
   fun lang_p ->
    fix (fun t ->
        choice
          [ lang_p >>| (fun core -> Atomic core) <?> "core term"
          ; brackets (sep_by (char ',') t) >>| (fun ts -> List ts) <?> "list"
          ; lift3 (fun name _colon edit -> Labeled (edit, name)) identifier (char ':') t
            <?> "labeled"
          ])
    <?> "edit"
 ;;

  let whitespace_t lang_p = junk *> t lang_p
end

type term = (OptRange.t, Primitive.t) Nominal.Term.t

let%test_module "Parsing" =
  (module struct
    module Parsers = ParseUtil.Mk (ParseUtil.CComment)
    module ParseEdit = Parse (ParseUtil.CComment)
    module ParseTerm = Nominal.Term.Parse (ParseUtil.CComment)
    module ParseCore = Core.Parse (ParseUtil.CComment)
    module ParsePrimitive = Primitive.Parse (ParseUtil.CComment)

    let parse : string -> (core t, string) Result.t =
      ParseUtil.parse_string (ParseEdit.whitespace_t (Parsers.braces ParseCore.term))
    ;;

    let parse_and_print : string -> unit =
     fun str ->
      match parse str with
      | Ok edit -> pp (Fmt.braces Core.pp) Caml.Format.std_formatter edit
      | Error msg -> print_string msg
   ;;

    let run_atom : term -> core -> (term, OptRange.t Core.eval_error) Result.t =
      let eval_primitive _eval_ctx _eval_ctx' _ctx _tm _name _args =
        Error
          ( "no primitive evaluation"
          , Core.Term
              (Nominal.Term.Primitive
                 (None, Primitive.PrimString "TODO: make this unnecessary")) )
      in
      fun tm core -> Core.(eval eval_primitive (CoreApp (None, core, [ Term tm ])))
    ;;

    (* TODO: don't throw away this information, switch from strings *)
    let run_atom' : term -> core -> (term, string) Result.t =
     fun tm core -> Result.map_error (run_atom tm core) ~f:(fun (msg, _tm) -> msg)
   ;;

    let rec run : term -> core t -> (term, string) Result.t =
     fun tm edit ->
      match edit with
      | Atomic core -> run_atom' tm core
      | Labeled (edit', _) -> run tm edit'
      | List edits -> List.fold_result edits ~init:tm ~f:run
   ;;

    let eval_and_print : string -> string -> unit =
     fun tm edit ->
      let open Result.Let_syntax in
      match
        let%bind edit = parse edit in
        let%bind tm = ParseUtil.parse_string (ParseTerm.t ParsePrimitive.t) tm in
        let%map tm = run tm edit in
        Nominal.Term.pp Primitive.pp Caml.Format.std_formatter tm
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

    (* rename a lambda calculus term. needs: cond, eq *)

    (* let rename x y = Printf.sprintf {|{ let rec rename = \(target : string()) ->
       \(new_name : string()) -> \(tm : lam()) -> match tm with { | var(v) -> cond(eq(v;
       target); var(new_name); tm) | lam(var(v). body) -> cond(eq(v; target); tm;
       lam(var(v). rename(body; target; new_name)) ) | app(tm1; tm2) -> app(rename(tm1;
       target; new_name); rename(tm1; target; new_name)) } in rename "%s" "%s" }|} x y *)

    (* let rename x y = Printf.sprintf {|{ let rec rename target new_name tm = match tm
       with { | var(v) -> if v == target then new_name else tm | lam(var(v). body) -> if v
       == target then tm else lam(var(v). rename body target new_name) ) | app(tm1; tm2)
       -> app(rename tm1 target new_name; rename tm1 target new_name) } in rename "%s"
       "%s" }|} x y *)

    (* XXX / lol -- rename should be built in. *is* built in *)

    (* let%expect_test _ = eval_and_print "lam(x. x)" (rename "x" "y"); [%expect{| lam(x.
       x) |}]

       let%expect_test _ = eval_and_print "x" (rename "x" "y"); [%expect{| y |}]

       let%expect_test _ = eval_and_print "lam(z. z)" (rename "x" "y"); [%expect{| lam(z.
       z) |}]

       let%expect_test _ = eval_and_print "lam(z. z) x" (rename "x" "y"); [%expect{|
       lam(z. z) x |}]

       let%expect_test _ = eval_and_print "lam(z. x) y" (rename "x" "y"); [%expect{|
       lam(z. y) y |}] *)

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
        {| sequence:[replace_bar:{\(tm : lang) -> match tm with { bar() -> {baz()} | _ -> {foo()} }}, named_edit:{f}] |}]
    ;;
  end)
;;

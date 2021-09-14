open Base
open Lvca_provenance
open Lvca_syntax
open Lvca_util
module List_model = Lvca_core.List_model
module Option_model = Lvca_core.Option_model
module Format = Stdlib.Format

module Lang =
[%lvca.abstract_syntax_module
{|
char : *  // module Primitive.Char
string : *  // module Primitive.String
list : * -> *  // module List_model.List
core_term : *  // module Lvca_core.Term
option : * -> *  // module Option_model.Option

term :=
  | AnyChar()
  | Char(char)
  | String(string)
  | Satisfy(string; core_term)
  | Fail(core_term)
  | Option(term)
  | Count(term; core_term)
  | Many(term)
  | Many1(term)
  | Choice(list term)
  | Sequence(sequence)
  | Let(term; term. term)
  | Fix(term. term)

sequence :=
  | Empty_sequence(core_term)
  | Binding(term; term. sequence)
  | Non_binding(term; sequence)
|}]

module Term = struct
  open Lang.Term

  module Prec = struct
    let atom = 6
    let quantifier = 5
    let app = 4
    let eq = 3
    let arr = 2
    let alt = 1
  end

  let rec sequence_to_list = function
    | Lang.Sequence.Empty_sequence (_, body) -> [], body
    | Binding (_, tm, ({ name; _ }, seq)) ->
      let rest, body = sequence_to_list seq in
      (Some name, tm) :: rest, body
    | Non_binding (_, tm, seq) ->
      let rest, body = sequence_to_list seq in
      (None, tm) :: rest, body
  ;;

  let pp_generic ~open_loc ~close_loc ppf p =
    let core = Lvca_core.Term.pp_concrete in
    let fmt, pf = Fmt.(fmt, pf) in
    let with_parens ~ambient_prec ~prec pp =
      if ambient_prec > prec then Fmt.parens pp else pp
    in
    let rec go ambient_prec ppf p =
      let loc = info p in
      open_loc ppf loc;
      let formatter, prec =
        match p with
        | AnyChar _ -> fmt ".", Prec.atom
        | Char (_, (_, char)) ->
          (fun ppf -> Fmt.(quote ~mark:"'" char) ppf char), Prec.atom
        | String (_, (_, str)) -> (fun ppf -> Fmt.(quote string) ppf str), Prec.atom
        | Satisfy (_, (_, name), tm) ->
          (fun ppf -> pf ppf "@[<2>satisfy (@[%s -> {%a}@])@]" name core tm), Prec.app
        | Let (_, named, ({ name; _ }, body)) ->
          ( (fun ppf ->
              pf ppf "@[<v>@[<2>let %s =@ @[%a@] in@]@ %a@]" name (go 0) named (go 0) body)
          , Prec.atom )
        | Fail (_, tm) ->
          let f ppf =
            match tm with
            | Embedded (_, Primitive (_, String msg)) -> pf ppf {|@[<2>fail "%s"@]|} msg
            | _ -> pf ppf "@[<2>fail {%a}@]" core tm
          in
          f, Prec.app
        | Count (_, p, tm) ->
          let f ppf =
            match tm with
            | Embedded (_, Primitive (_, Integer n)) ->
              pf ppf "@[<hv>%a%s@]" (go Prec.quantifier) p (Z.to_string n)
            | _ -> pf ppf "@[<hv>%a{%a}@]" (go Prec.quantifier) p core tm
          in
          f, Prec.quantifier
        | Option (_, p) ->
          (fun ppf -> pf ppf "%a?" (go Prec.quantifier) p), Prec.quantifier
        | Many (_, p) -> (fun ppf -> pf ppf "%a*" (go Prec.quantifier) p), Prec.quantifier
        | Many1 (_, p) ->
          (fun ppf -> pf ppf "%a+" (go Prec.quantifier) p), Prec.quantifier
        | Choice (_, branches) ->
          let initial_bar =
            Format.pp_print_custom_break ~fits:("", 0, "") ~breaks:("", 2, "| ")
          in
          let branches = List_model.to_list branches in
          let formatter ppf =
            pf
              ppf
              "@[<hv 0>%t%a@,@]"
              initial_bar
              Fmt.(list ~sep:(any "@;<1 2>| ") (go Prec.alt))
              branches
          in
          formatter, Prec.alt
        | Fix (_, ({ name; _ }, p)) ->
          (fun ppf -> pf ppf "@[<2>fix@ (@[%s -> %a@])@]" name (go 0) p), Prec.app
        | Sequence (_, seq) ->
          let binders, body = sequence_to_list seq in
          let formatter ppf =
            pf
              ppf
              "@[<hv 2>@[<hv>%a@] ->@ {%a}@]"
              Fmt.(list ~sep:(any "@ ") binder)
              binders
              core
              body
          in
          formatter, Prec.arr
        | Term_var (_, name) -> (fun ppf -> Fmt.string ppf name), Prec.atom
      in
      with_parens ~ambient_prec ~prec (fun ppf () -> formatter ppf) ppf ();
      close_loc ppf loc
    and binder ppf (opt_name, p) =
      match opt_name with
      | None -> (go 0) ppf p
      | Some name -> pf ppf "%s=%a" name (go Prec.eq) p
    in
    go 0 ppf p
  ;;
end

let pp_plain ppf p =
  Term.pp_generic ~open_loc:(fun _ _ -> ()) ~close_loc:(fun _ _ -> ()) ppf p
;;

module Parse = struct
  open Lvca_parsing
  open Lang.Term

  type quantifier =
    | Q_count of Opt_range.t Lvca_core.Term.t
    | Q_option
    | Q_many
    | Q_many1

  let rec apply_quantifiers tm = function
    | [] -> tm
    | (q, q_range) :: qs ->
      let range = Opt_range.union (Lang.Term.info tm) q_range in
      let tm =
        match q with
        | Q_count c_tm -> Count (range, tm, c_tm)
        | Q_option -> Option (range, tm)
        | Q_many -> Many (range, tm)
        | Q_many1 -> Many1 (range, tm)
      in
      apply_quantifiers tm qs
  ;;

  let rec mk_sequence binders rhs =
    match binders with
    | [] ->
      let range = Lvca_core.Term.info rhs in
      Lang.Sequence.Empty_sequence (range, rhs)
    | (Some (name, info), tm) :: binders ->
      let rhs = mk_sequence binders rhs in
      let range = Opt_range.union info (Lang.Term.info tm) in
      Lang.Sequence.Binding (range, tm, (Single_var.{ name; info }, rhs))
    | (None, tm) :: binders ->
      let rhs = mk_sequence binders rhs in
      let range = Lang.Term.info tm in
      Lang.Sequence.Non_binding (range, tm, rhs)
  ;;

  let arrow = Ws.string "->"

  let t c_term =
    fix (fun parser ->
        (* prec 6 *)
        let parse_atom =
          choice
            [ (Ws.char_lit >>~ fun range c -> Char (range, (range, c)))
            ; (Ws.string_lit >>~ fun range str -> String (range, (range, str)))
            ; (Ws.char '.' >>~ fun range _ -> AnyChar range)
            ; (Ws.identifier >>~ fun range ident -> Term_var (range, ident))
            ; Ws.parens parser
            ]
        in
        (* prec 5 *)
        let parse_quantified =
          parse_atom
          >>= fun tm ->
          let quantifier =
            choice
              [ (Ws.braces c_term >>| fun tm -> Q_count tm)
              ; (Ws.integer_lit
                >>~ fun range i ->
                let i = Z.of_string i in
                let c_term =
                  Lvca_core.Lang.Term.Embedded (range, Primitive (range, Integer i))
                in
                Q_count c_term)
              ; (Ws.char '?' >>| fun _ -> Q_option)
              ; (Ws.char '*' >>| fun _ -> Q_many)
              ; (Ws.char '+' >>| fun _ -> Q_many1)
              ]
          in
          many (attach_pos quantifier) >>| apply_quantifiers tm
        in
        (* prec 4 *)
        let parse_app =
          choice
            [ (Ws.string "fix"
              >>== fun { range = range1; _ } ->
              Ws.parens
                (lift3
                   (fun (name, info) _arr (body, range2) ->
                     let range = Opt_range.union range1 range2 in
                     Fix (range, (Single_var.{ name; info }, body)))
                   (attach_pos Ws.identifier)
                   arrow
                   (attach_pos parser)))
            ; (Ws.string "satisfy"
              >>== fun { range = range1; _ } ->
              Ws.parens
                (lift3
                   (fun (name, info) _arr (tm, range2) ->
                     let range = Opt_range.union range1 range2 in
                     Satisfy (range, (info, name), tm))
                   (attach_pos Ws.identifier)
                   arrow
                   (attach_pos (Ws.braces c_term))))
            ; (Ws.string "fail"
              >>== fun { range = range1; _ } ->
              choice
                [ (Ws.braces c_term
                  >>~ fun range2 tm ->
                  let range = Opt_range.union range1 range2 in
                  Fail (range, tm))
                ; (Ws.string_lit
                  >>~ fun range2 str ->
                  let range = Opt_range.union range1 range2 in
                  Fail
                    ( range
                    , Lvca_core.Lang.Term.Embedded (range, Primitive (range2, String str))
                    ))
                ])
            ]
          <|> parse_quantified
        in
        (* prec 3 *)
        let parse_eq =
          choice
            [ lift3
                (fun ident _eq tm -> Some ident, tm)
                (attach_pos Ws.identifier)
                (Ws.char '=')
                parse_app
            ; (parse_app >>| fun tm -> None, tm)
            ]
        in
        (* prec 2 *)
        let parse_arr =
          choice
            [ lift3
                (fun (bindings, range1) _arr (rhs, range2) ->
                  let range = Opt_range.union range1 range2 in
                  Sequence (range, mk_sequence bindings rhs))
                (attach_pos (many parse_eq))
                arrow
                (attach_pos (Ws.braces c_term))
            ; parse_app
            ]
        in
        (* prec 1 *)
        let parse_alt =
          parse_arr
          >>== fun { range = range1; value = tm } ->
          many (Ws.char '|' *> parse_arr)
          >>~ fun range2 tms ->
          let range = Opt_range.union range1 range2 in
          match tms with
          | [] -> tm
          | _ -> Choice (range, List_model.of_list ~empty_info:None (tm :: tms))
        in
        (* prec 0 *)
        let parse_let =
          choice
            [ lift4
                (fun (_let, range1) (name, info) _eq tm _in rhs ->
                  let range = Opt_range.union range1 (Lang.Term.info rhs) in
                  Let (range, tm, (Single_var.{ name; info }, rhs)))
                (attach_pos (Ws.string "let"))
                (attach_pos Ws.identifier)
                (Ws.char '=')
                parser
              <*> Ws.string "in"
              <*> parser
            ; parse_alt
            ]
        in
        parse_let)
    <?> "parser"
  ;;
end

module Test_parsers = Parser.Test_parsers

let parse_core =
  let open Lvca_parsing in
  Lvca_core.Parse.term ~comment:c_comment
  >>| Lvca_core.Term.map_info ~f:Commented.get_range
;;

let parse_parser = Lvca_parsing.parse_string (Parse.t parse_core)

let%test_module "Parsing" =
  (module struct
    open Test_parsers

    let margin = Stdlib.Format.(pp_get_margin std_formatter ())
    let () = Stdlib.Format.(pp_set_margin std_formatter 80)

    let parse_print_parser : ?width:int -> string -> unit =
     fun ?width parser_str ->
      match Lvca_parsing.(parse_string (whitespace *> Parse.t parse_core) parser_str) with
      | Error msg -> Stdio.print_string ("failed to parse parser desc: " ^ msg)
      | Ok parser ->
        let pre_geom =
          Option.map width ~f:(fun n ->
              let geom = Format.get_geometry () in
              Format.set_margin n;
              geom)
        in
        Fmt.pr "%a\n" pp_plain parser;
        (match pre_geom with
        | None -> ()
        | Some { max_indent; margin } -> Format.set_geometry ~max_indent ~margin)
   ;;

    (*
    let%expect_test _ = parse_print
      {|chars=alpha+ -> { let str = string_of_chars chars in {var(str)} }|}
      "ab";
      (* TODO: should give comprehensible error *)
      [%expect]
    *)

    let%expect_test _ =
      parse_print_parser
        {|let atom = name | literal in
fix
(expr ->
(atom=atom ' '* '+'
' '*
expr=expr -> {{add(atom;
expr)}} | atom=atom -> {atom}))|};
      [%expect
        {|
       let atom = name | literal in
       fix
         (expr ->
                    | atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
                    | atom=atom -> {atom}
                  ) |}]
    ;;

    let%expect_test _ =
      (* TODO: decide about leading bars *)
      parse_print_parser
        {|let char = satisfy (c -> {is_alpha c}) in
let digit = satisfy (c -> {is_digit c}) in
let name = (chars=char+ -> {let str = string_of_chars chars in {var(str)} }) in
let literal = (chars=digit+ -> {{literal(chars)}}) in
let atom = (name | literal) in
fix (expr ->
  ( atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
  | atom=atom -> {{atom}}
))|};
      [%expect
        {|
       let char = satisfy (c -> {is_alpha c}) in
       let digit = satisfy (c -> {is_digit c}) in
       let name = chars=char+ -> {let str = string_of_chars chars in {var(str)}} in
       let literal = chars=digit+ -> {{literal(chars)}} in
       let atom = name | literal in
       fix
         (expr ->
                    | atom=atom ' '* '+' ' '* expr=expr -> {{add(atom; expr)}}
                    | atom=atom -> {{atom}}
                  ) |}]
    ;;

    let%expect_test _ =
      parse_print_parser "'c'2";
      [%expect {| 'c'2 |}]
    ;;

    let%expect_test _ =
      parse_print_parser "'c'{{2}}";
      [%expect {| 'c'2 |}]
    ;;

    let%expect_test _ =
      parse_print_parser "F++";
      [%expect {| F++ |}]
    ;;

    let%expect_test _ =
      parse_print_parser "F+?";
      [%expect {| F+? |}]
    ;;

    let%expect_test _ =
      parse_print_parser "(F+)+";
      [%expect {| F++ |}]
    ;;

    let%expect_test _ =
      parse_print_parser dot;
      [%expect {| . |}]
    ;;

    let%expect_test _ =
      parse_print_parser str_star;
      [%expect {| "str"* |}]
    ;;

    let%expect_test _ =
      parse_print_parser "('a' | 'b' | 'c')";
      [%expect {| 'a' | 'b' | 'c' |}]
    ;;

    let%expect_test _ =
      parse_print_parser "(. -> {Q} | .)";
      [%expect {| . -> {Q} | . |}]
    ;;

    let list_parser = {|fix (lst -> c='c' cs=lst -> {{cons(c; cs)}} | -> {{nil()}})|}

    let%expect_test _ =
      parse_print_parser list_parser;
      [%expect {| fix (lst -> c='c' cs=lst -> {{cons(c; cs)}} |  -> {{nil()}}) |}]
    ;;

    let%expect_test _ =
      parse_print_parser {|. ' '* '+' ' '* . -> {{"parsed an addition"}}|};
      [%expect {| . ' '* '+' ' '* . -> {{"parsed an addition"}} |}]
    ;;

    let%expect_test _ =
      parse_print_parser "a=. ' '* '+' ' '* b=. -> {{plus(a; b)}}";
      [%expect {| a=. ' '* '+' ' '* b=. -> {{plus(a; b)}} |}]
    ;;

    let%expect_test _ =
      parse_print_parser {|fail {{"some reason for failing"}}|};
      [%expect {| fail "some reason for failing" |}]
    ;;

    let%expect_test _ =
      parse_print_parser {|fail "some reason for failing"|};
      [%expect {| fail "some reason for failing" |}]
    ;;

    let%expect_test _ =
      parse_print_parser
        {|
     satisfy (c -> {match c with {
  | 'c' -> {true()}
  | _ -> {false()}
     }})|};
      [%expect {| satisfy (c -> {match c with { 'c' -> {true()} | _ -> {false()} }}) |}]
    ;;

    let%expect_test _ =
      parse_print_parser
        {|
let atom = name | literal in
fix (expr -> (
        a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
  | a=atom -> {a}
))|};
      [%expect
        {|
       let atom = name | literal in
       fix (expr -> a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}} | a=atom -> {a}) |}]
    ;;

    (* TODO
    let%expect_test _ =
      parse_print_parser
        {|
let atom = (name | literal) in
fix (expr -> (
        | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
  | a=atom -> a
))|};
      [%expect
        {|
       let atom = (name | literal) in
       fix
         (expr -> (
                    | a=atom ' '* '+' ' '* e=expr -> {{plus(a; e)}}
                    | a=atom -> a
                  )) |}]
    ;;
    *)

    let%expect_test _ =
      parse_print_parser "name | literal";
      [%expect {| name | literal |}]
    ;;

    let%expect_test _ =
      parse_print_parser ~width:12 "name | literal";
      [%expect {|
        | name
        | literal |}]
    ;;

    let%expect_test _ =
      parse_print_parser "fix (x -> (e=expr -> {e} | a=atom -> {a}))";
      [%expect {| fix (x -> e=expr -> {e} | a=atom -> {a}) |}]
    ;;

    let () = Stdlib.Format.(pp_set_margin std_formatter margin)
  end)
;;

module Properties = struct
  open Property_result

  let parse parser_str = Lvca_parsing.parse_string (Parse.t parse_core) parser_str
  let pp_str p = Fmt.to_to_string pp_plain p

  let string_round_trip1 t =
    match t |> pp_str |> parse with
    | Ok t' ->
      Property_result.check
        Lang.Term.Plain.(Lang.Term.to_plain t' = Lang.Term.to_plain t)
        (Fmt.str "%a <> %a" pp_plain t' pp_plain t)
    | Error msg -> Failed (Fmt.str {|parse_string "%s": %s|} (pp_str t) msg)
  ;;

  let string_round_trip2 str =
    match parse str with
    | Error _ -> Uninteresting
    | Ok t ->
      let str' = pp_str t in
      if String.(str' = str)
      then Ok
      else (
        match parse str with
        | Error msg -> Failed msg
        | Ok t' ->
          let str'' = pp_str t' in
          Property_result.check
            String.(str'' = str')
            (Fmt.str {|"%s" <> "%s"|} str'' str'))
  ;;
end

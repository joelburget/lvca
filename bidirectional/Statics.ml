open Base
open Lvca_syntax
open Lvca_util

let reserved = String.Set.empty

module Typing_rule = struct
  type t =
    { tm : Binding_aware_pattern.t
    ; ty : Binding_aware_pattern.t
    }

  let equivalent ?(info_eq = fun _ _ -> true) a b =
    Binding_aware_pattern.(equivalent ~info_eq a.tm b.tm && equivalent ~info_eq a.ty b.ty)
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
end

module Typing_clause = struct
  type inference_rule = Typing_rule.t
  type checking_rule = Typing_rule.t

  type t =
    | Inference_rule of inference_rule
    | Checking_rule of checking_rule

  let equivalent ?(info_eq = fun _ _ -> true) a b =
    match a, b with
    | Inference_rule a, Inference_rule b -> Typing_rule.equivalent ~info_eq a b
    | Checking_rule a, Checking_rule b -> Typing_rule.equivalent ~info_eq a b
    | _, _ -> false
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let pp ppf = function
    | Inference_rule { tm; ty } ->
      Fmt.pf ppf "%a => %a" Binding_aware_pattern.pp tm Binding_aware_pattern.pp ty
    | Checking_rule { tm; ty } ->
      Fmt.pf ppf "%a <= %a" Binding_aware_pattern.pp tm Binding_aware_pattern.pp ty
  ;;

  module Parse = struct
    open Lvca_parsing

    type arrow_dir =
      | LeftArr
      | RightArr

    let t =
      lift3
        (fun tm dir ty ->
          match dir with
          | LeftArr -> Checking_rule { tm; ty }
          | RightArr -> Inference_rule { tm; ty })
        (Binding_aware_pattern.parse reserved)
        (choice
           ~failure_msg:"looking for <= or =>"
           C_comment_parser.
             [ (string "<=" >>| fun _ -> LeftArr); (string "=>" >>| fun _ -> RightArr) ])
        (Binding_aware_pattern.parse reserved)
      <?> "typing clause"
    ;;
  end

  let parse = Parse.t

  let%test_module _ =
    (module struct
      let ( = ) = equivalent
      let here = Provenance.of_here [%here]

      let parse_exn =
        Lvca_parsing.(parse_string (whitespace *> parse)) >> Result.ok_or_failwith
      ;;

      let print_parse = parse_exn >> Fmt.pr "%a" pp

      let%test _ =
        parse_exn "tm => ty"
        = Inference_rule { tm = Var (here, "tm"); ty = Var (here, "ty") }
      ;;

      let%expect_test _ =
        print_parse "tm => ty";
        [%expect "tm => ty"]
      ;;
    end)
  ;;
end

exception StaticsParseError of string

module Hypothesis = struct
  type t = Binding_aware_pattern.t String.Map.t * Typing_clause.t

  let equivalent ?(info_eq = fun _ _ -> true) (m1, c1) (m2, c2) =
    Map.equal (Binding_aware_pattern.equivalent ~info_eq) m1 m2
    && Typing_clause.equivalent ~info_eq c1 c2
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let pp ppf (ctx, clause) =
    let open Fmt in
    let typed_term ppf (name, pat) =
      Fmt.pf ppf "%s : %a" name Binding_aware_pattern.pp pat
    in
    match Map.to_alist ctx with
    | [] -> pf ppf "ctx >> %a" Typing_clause.pp clause
    | ctx ->
      pf ppf "ctx, %a >> %a" (list ~sep:comma typed_term) ctx Typing_clause.pp clause
  ;;

  module Parse = struct
    open Lvca_parsing
    open C_comment_parser

    let typed_term =
      lift3
        (fun ident _ tm -> ident, tm)
        (lower_identifier reserved)
        (char ':')
        (Binding_aware_pattern.parse reserved)
      <?> "typed pattern"
    ;;

    let context =
      string "ctx"
      *> choice
           [ (let%bind _ = char ',' in
              let%bind ctx_entries = sep_by1 (char ',') typed_term in
              match String.Map.of_alist ctx_entries with
              | `Ok context -> return context
              | `Duplicate_key str ->
                raise
                  (StaticsParseError (Printf.sprintf "duplicate name in context: %s" str)))
           ; return String.Map.empty
           ]
      <?> "context"
    ;;

    let t =
      lift3 (fun ctx _ clause -> ctx, clause) context (string ">>") Typing_clause.Parse.t
      <?> "hypothesis"
    ;;
  end

  let%test_module "Parsing" =
    (module struct
      let here = Provenance.of_here [%here]

      let parse_exn =
        Lvca_parsing.(parse_string (whitespace *> Parse.t)) >> Result.ok_or_failwith
      ;;

      let print_parse = parse_exn >> Fmt.pr "%a" pp

      let%test _ =
        let m, rule = parse_exn "ctx >> t1 <= bool()" in
        Map.is_empty m
        && Typing_clause.(
             equivalent
               rule
               (Checking_rule { tm = Var (here, "t1"); ty = Operator (here, "bool", []) }))
      ;;

      let%expect_test _ =
        print_parse "ctx >> t1 <= bool()";
        [%expect "ctx >> t1 <= bool()"]
      ;;

      let%expect_test _ =
        print_parse "ctx, x : t >> t1 => bool()";
        [%expect "ctx, x : t >> t1 => bool()"]
      ;;
    end)
  ;;
end

module Rule = struct
  type t =
    { hypotheses : Hypothesis.t list
    ; name : string option
    ; conclusion : Hypothesis.t
    }

  let equivalent ?(info_eq = fun _ _ -> true) a b =
    List.equal (Hypothesis.equivalent ~info_eq) a.hypotheses b.hypotheses
    && Option.equal String.( = ) a.name b.name
    && Hypothesis.equivalent ~info_eq a.conclusion b.conclusion
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  module Parse = struct
    open Lvca_parsing
    open C_comment_parser

    let line : string option Lvca_parsing.t =
      lift3
        (fun _ _ ident -> ident)
        (string "--")
        (many (char '-'))
        (option None ((fun ident -> Some ident) <$> parens (lower_identifier reserved)))
      <?> "line"
    ;;

    let t =
      lift3
        (fun hypotheses name conclusion -> { hypotheses; name; conclusion })
        (many Hypothesis.Parse.t)
        line
        Hypothesis.Parse.t
      <?> "typing rule"
    ;;
  end
end

module Typing = struct
  type t = Typing of Nominal.Term.t * Nominal.Term.t

  let equivalent ?(info_eq = fun _ _ -> true) (Typing (t11, t12)) (Typing (t21, t22)) =
    Nominal.Term.equivalent ~info_eq t11 t21 && Nominal.Term.equivalent ~info_eq t12 t22
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
end

type t = Rule.t list

let parse = Lvca_parsing.many Rule.Parse.t

let%test_module "Parsing" =
  (module struct
    let parse_exn =
      Lvca_parsing.(parse_string (whitespace *> parse)) >> Result.ok_or_failwith
    ;;

    let print_parse = parse_exn >> Fn.const "parsed" >> Stdio.print_string

    let%expect_test _ =
      print_parse {|
    ---
    ctx >> tm => ty
  |};
      [%expect {| parsed |}]
    ;;

    let%expect_test _ =
      print_parse {|
    ctx, x : a >> Foo(x) <= a
    ---
    ctx >> tm => ty
  |};
      [%expect {| parsed |}]
    ;;

    let%expect_test _ =
      print_parse
        {|
    ---
    ctx, x : t >> x => t

    ---
    ctx >> Str(x) => str()

    ---
    ctx >> Num(x) => num()

    ctx >> e1 <= num()    ctx >> e2 <= num()
    ---
    ctx >> Plus(e1; e2) => num()

    ctx >> e1 <= str()    ctx >> e2 <= str()
    ---
    ctx >> Cat(e1; e2) => str()

    ctx >> e <= str()
    ---
    ctx >> Len(e) => num()

    ctx >> e1 => t1   ctx, x : t1 >> e2 <= t2
    --- (Let)
    ctx >> Let(e1; x. e2) <= t2
  |};
      [%expect {| parsed |}]
    ;;

    let%expect_test _ =
      print_parse {|

---
  ctx >> True() => bool()

---
  ctx >> False() => bool()

  |};
      [%expect {| parsed |}]
    ;;
  end)
;;

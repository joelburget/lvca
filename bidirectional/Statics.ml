open Base
open Lvca_syntax

module TypingRule = struct
  type 'info t =
    { tm : 'info BindingAwarePattern.t
    ; ty : 'info BindingAwarePattern.t
    }

  let equal ~info_eq a b =
    BindingAwarePattern.(equal ~info_eq a.tm b.tm && equal ~info_eq a.ty b.ty)
  ;;

  let erase { tm; ty } =
    { tm = BindingAwarePattern.erase tm; ty = BindingAwarePattern.erase ty }
  ;;
end

module TypingClause = struct
  type 'info inference_rule = 'info TypingRule.t
  type 'info checking_rule = 'info TypingRule.t

  type 'info t =
    | InferenceRule of 'info inference_rule
    | CheckingRule of 'info checking_rule

  let erase = function
    | InferenceRule rule -> InferenceRule (TypingRule.erase rule)
    | CheckingRule rule -> CheckingRule (TypingRule.erase rule)
  ;;

  let equal ~info_eq a b =
    match a, b with
    | InferenceRule a, InferenceRule b -> TypingRule.equal ~info_eq a b
    | CheckingRule a, CheckingRule b -> TypingRule.equal ~info_eq a b
    | _, _ -> false
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    module Pattern = BindingAwarePattern.Parse (Comment)
    open Parsers

    type arrow_dir =
      | LeftArr
      | RightArr

    let pattern = Pattern.t <?> "pattern"

    let t =
      lift3
        (fun tm dir ty ->
          match dir with
          | LeftArr -> CheckingRule { tm; ty }
          | RightArr -> InferenceRule { tm; ty })
        pattern
        (choice
           [ (string "<=" >>| fun _ -> LeftArr); (string "=>" >>| fun _ -> RightArr) ])
        pattern
      <?> "typing clause"
    ;;
  end

  let%test_module _ =
    (module struct
      module Parse = Parse (ParseUtil.NoComment)

      let ( = ) = Result.equal (equal ~info_eq:Unit.( = )) String.( = )

      let%test _ =
        ParseUtil.parse_string Parse.t "tm => ty"
        |> Result.map ~f:erase
        = Ok (InferenceRule { tm = Var ((), "tm"); ty = Var ((), "ty") })
      ;;
    end)
  ;;
end

exception StaticsParseError of string

module Hypothesis = struct
  type 'info t = 'info BindingAwarePattern.t Lvca_util.String.Map.t * 'info TypingClause.t

  let equal ~info_eq (m1, c1) (m2, c2) =
    Map.equal (BindingAwarePattern.equal ~info_eq) m1 m2
    && TypingClause.equal ~info_eq c1 c2
  ;;

  let erase (env, clause) =
    Map.map env ~f:BindingAwarePattern.erase, TypingClause.erase clause
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    module TypingClause = TypingClause.Parse (Comment)
    module Pattern = BindingAwarePattern.Parse (Comment)
    open Parsers

    (* TODO: remove duplication *)
    let pattern = Pattern.t <?> "pattern"

    let typed_term =
      lift3 (fun ident _ tm -> ident, tm) identifier (char ':') pattern
      <?> "typed pattern"
    ;;

    let context =
      string "ctx"
      *> choice
           [ (char ','
             >>= fun _ ->
             sep_by1 (char ',') typed_term
             >>= fun ctx_entries ->
             match Lvca_util.String.Map.of_alist ctx_entries with
             | `Ok context -> return context
             | `Duplicate_key str ->
               raise
                 (StaticsParseError (Printf.sprintf "duplicate name in context: %s" str))
             )
           ; return Lvca_util.String.Map.empty
           ]
      <?> "context"
    ;;

    let t =
      lift3 (fun ctx _ clause -> ctx, clause) context (string ">>") TypingClause.t
      <?> "hypothesis"
    ;;
  end

  let%test_module "Parsing" =
    (module struct
      module Parse = Parse (ParseUtil.NoComment)

      let%test _ =
        match ParseUtil.parse_string Parse.t "ctx >> t1 <= bool()" with
        | Error _ -> false
        | Ok (m, rule) ->
          Map.is_empty m
          && TypingClause.(
               equal
                 ~info_eq:Unit.( = )
                 (erase rule)
                 (CheckingRule { tm = Var ((), "t1"); ty = Operator ((), "bool", []) }))
      ;;
    end)
  ;;
end

module Rule = struct
  type 'a t =
    { hypotheses : 'a Hypothesis.t list
    ; name : string option
    ; conclusion : 'a Hypothesis.t
    }

  let erase { hypotheses; name; conclusion } =
    { hypotheses = List.map hypotheses ~f:Hypothesis.erase
    ; name
    ; conclusion = Hypothesis.erase conclusion
    }
  ;;

  module Parse (Comment : ParseUtil.Comment_int) = struct
    module Parsers = ParseUtil.Mk (Comment)
    module Hypothesis = Hypothesis.Parse (Comment)
    open Parsers

    let identifier, char, parens = Parsers.(identifier, char, parens)

    let bar =
      Angstrom.(
        lift3
          (fun start result finish -> result, Some Range.{ start; finish })
          pos
          (string "--") (* use Angstrom.string to prevent spaces here *)
          pos)
    ;;

    let line : string option Parsers.t =
      lift3
        (fun _ _ ident -> ident)
        bar
        (many (char '-'))
        (option None ((fun ident -> Some ident) <$> parens identifier))
      <?> "line"
    ;;

    let t =
      lift3
        (fun hypotheses name conclusion -> { hypotheses; name; conclusion })
        (many Hypothesis.t)
        line
        Hypothesis.t
      <?> "typing rule"
    ;;
  end
end

module Typing = struct
  type 'a t = Typing of 'a Nominal.Term.t * 'a Nominal.Term.t

  let erase (Typing (t1, t2)) = Typing (Nominal.Term.erase t1, Nominal.Term.erase t2)
end

type 'a t = 'a Rule.t list

let erase = List.map ~f:Rule.erase

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Rule = Rule.Parse (Comment)
  open Parsers

  let t = many Rule.t
  let whitespace_t = ParseUtil.whitespace *> t
end

let%test_module "Parsing" =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let print_parse desc =
      let str =
        ParseUtil.parse_string Parse.whitespace_t desc
        |> Result.ok_or_failwith
        |> Fn.const "parsed"
      in
      Stdio.print_string str
    ;;

    let%expect_test _ =
      print_parse {|
    ---
    ctx >> tm => ty
  |};
      [%expect {| parsed |}]
    ;;

    let%expect_test _ =
      print_parse {|
    ctx, x : a >> foo(x) <= a
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
    ctx >> str(x) => str()

    ---
    ctx >> num(x) => num()

    ctx >> e1 <= num()    ctx >> e2 <= num()
    ---
    ctx >> plus(e1; e2) => num()

    ctx >> e1 <= str()    ctx >> e2 <= str()
    ---
    ctx >> cat(e1; e2) => str()

    ctx >> e <= str()
    ---
    ctx >> len(e) => num()

    ctx >> e1 => t1   ctx, x : t1 >> e2 <= t2
    --- (let)
    ctx >> let(e1; x. e2) <= t2
  |};
      [%expect {| parsed |}]
    ;;
  end)
;;

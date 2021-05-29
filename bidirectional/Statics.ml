open Base
open Lvca_syntax
open Lvca_util

module Typing_rule = struct
  type 'info t =
    { tm : 'info Binding_aware_pattern.t
    ; ty : 'info Binding_aware_pattern.t
    }

  let equal ~info_eq a b =
    Binding_aware_pattern.(equal ~info_eq a.tm b.tm && equal ~info_eq a.ty b.ty)
  ;;

  let erase { tm; ty } =
    { tm = Binding_aware_pattern.erase tm; ty = Binding_aware_pattern.erase ty }
  ;;
end

module Typing_clause = struct
  type 'info inference_rule = 'info Typing_rule.t
  type 'info checking_rule = 'info Typing_rule.t

  type 'info t =
    | Inference_rule of 'info inference_rule
    | Checking_rule of 'info checking_rule

  let erase = function
    | Inference_rule rule -> Inference_rule (Typing_rule.erase rule)
    | Checking_rule rule -> Checking_rule (Typing_rule.erase rule)
  ;;

  let equal ~info_eq a b =
    match a, b with
    | Inference_rule a, Inference_rule b -> Typing_rule.equal ~info_eq a b
    | Checking_rule a, Checking_rule b -> Typing_rule.equal ~info_eq a b
    | _, _ -> false
  ;;

  module Parse = struct
    open Lvca_parsing

    type arrow_dir =
      | LeftArr
      | RightArr

    let pattern = Binding_aware_pattern.Parse.t <?> "pattern"

    let t =
      lift3
        (fun tm dir ty ->
          match dir with
          | LeftArr -> Checking_rule { tm; ty }
          | RightArr -> Inference_rule { tm; ty })
        pattern
        (choice
           [ (string "<=" >>| fun _ -> LeftArr); (string "=>" >>| fun _ -> RightArr) ])
        pattern
      <?> "typing clause"
    ;;
  end

  let%test_module _ =
    (module struct
      let ( = ) = Result.equal (equal ~info_eq:Unit.( = )) String.( = )

      let%test _ =
        Lvca_parsing.parse_string Parse.t "tm => ty"
        |> Result.map ~f:erase
        = Ok (Inference_rule { tm = Var ((), "tm"); ty = Var ((), "ty") })
      ;;
    end)
  ;;
end

exception StaticsParseError of string

module Hypothesis = struct
  type 'info t = 'info Binding_aware_pattern.t String.Map.t * 'info Typing_clause.t

  let equal ~info_eq (m1, c1) (m2, c2) =
    Map.equal (Binding_aware_pattern.equal ~info_eq) m1 m2
    && Typing_clause.equal ~info_eq c1 c2
  ;;

  let erase (env, clause) =
    Map.map env ~f:Binding_aware_pattern.erase, Typing_clause.erase clause
  ;;

  module Parse = struct
    open Lvca_parsing

    (* TODO: remove duplication *)
    let pattern = Binding_aware_pattern.Parse.t <?> "pattern"

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
             match String.Map.of_alist ctx_entries with
             | `Ok context -> return context
             | `Duplicate_key str ->
               raise
                 (StaticsParseError (Printf.sprintf "duplicate name in context: %s" str))
             )
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
      let%test _ =
        match Lvca_parsing.parse_string Parse.t "ctx >> t1 <= bool()" with
        | Error _ -> false
        | Ok (m, rule) ->
          Map.is_empty m
          && Typing_clause.(
               equal
                 ~info_eq:Unit.( = )
                 (erase rule)
                 (Checking_rule { tm = Var ((), "t1"); ty = Operator ((), "bool", []) }))
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

  module Parse = struct
    open Lvca_parsing

    let line : string option Lvca_parsing.t =
      lift3
        (fun _ _ ident -> ident)
        (string "--")
        (many (char '-'))
        (option None ((fun ident -> Some ident) <$> parens identifier))
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
  type 'a t = Typing of 'a Nominal.Term.t * 'a Nominal.Term.t

  let erase (Typing (t1, t2)) = Typing (Nominal.Term.erase t1, Nominal.Term.erase t2)
end

type 'a t = 'a Rule.t list

let erase = List.map ~f:Rule.erase

module Parse = struct
  open Lvca_parsing

  let t = many Rule.Parse.t
  let whitespace_t = whitespace *> t
end

let%test_module "Parsing" =
  (module struct
    let print_parse desc =
      let str =
        Lvca_parsing.parse_string Parse.whitespace_t desc
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

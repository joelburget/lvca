open Base
open Lvca_syntax
open Lvca_util

module Typing_rule = struct
  type t =
    { tm : Binding_aware_pattern.t
    ; ty : Binding_aware_pattern.t
    }

  let equivalent ?(info_eq = fun _ _ -> true) a b =
    Binding_aware_pattern.(equivalent ~info_eq a.tm b.tm && equivalent ~info_eq a.ty b.ty)
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  (*
  let erase { tm; ty } =
    { tm = Binding_aware_pattern.erase tm; ty = Binding_aware_pattern.erase ty }
  ;;
     *)
end

module Typing_clause = struct
  type inference_rule = Typing_rule.t
  type checking_rule = Typing_rule.t

  type t =
    | Inference_rule of inference_rule
    | Checking_rule of checking_rule

  (*
  let erase = function
    | Inference_rule rule -> Inference_rule (Typing_rule.erase rule)
    | Checking_rule rule -> Checking_rule (Typing_rule.erase rule)
  ;;
     *)

  let equivalent ?(info_eq = fun _ _ -> true) a b =
    match a, b with
    | Inference_rule a, Inference_rule b -> Typing_rule.equivalent ~info_eq a b
    | Checking_rule a, Checking_rule b -> Typing_rule.equivalent ~info_eq a b
    | _, _ -> false
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

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
        Binding_aware_pattern.parse
        (choice
           ~failure_msg:"looking for <= or =>"
           [ (Ws.string "<=" >>| fun _ -> LeftArr)
           ; (Ws.string "=>" >>| fun _ -> RightArr)
           ])
        Binding_aware_pattern.parse
      <?> "typing clause"
    ;;
  end

  let parse = Parse.t

  let%test_module _ =
    (module struct
      let ( = ) = Result.equal (equivalent ~info_eq:(fun _ _ -> true)) String.( = )

      let%test _ =
        Lvca_parsing.parse_string parse "tm => ty"
        = Ok (Inference_rule { tm = Var (`Empty, "tm"); ty = Var (`Empty, "ty") })
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

  (*
  let erase (env, clause) =
    Map.map env ~f:Binding_aware_pattern.erase, Typing_clause.erase clause
  ;;
     *)

  module Parse = struct
    open Lvca_parsing

    let typed_term =
      lift3
        (fun ident _ tm -> ident, tm)
        Ws.identifier
        (Ws.char ':')
        Binding_aware_pattern.parse
      <?> "typed pattern"
    ;;

    let context =
      Ws.string "ctx"
      *> choice
           [ (Ws.char ','
             >>= fun _ ->
             sep_by1 (Ws.char ',') typed_term
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
      lift3
        (fun ctx _ clause -> ctx, clause)
        context
        (Ws.string ">>")
        Typing_clause.Parse.t
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
               equivalent
                 rule
                 (Checking_rule
                    { tm = Var (`Empty, "t1"); ty = Operator (`Empty, "bool", []) }))
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

  (*
  let erase { hypotheses; name; conclusion } =
    { hypotheses = List.map hypotheses ~f:Hypothesis.erase
    ; name
    ; conclusion = Hypothesis.erase conclusion
    }
  ;;
     *)

  let equivalent ?(info_eq = fun _ _ -> true) a b =
    List.equal (Hypothesis.equivalent ~info_eq) a.hypotheses b.hypotheses
    && Option.equal String.( = ) a.name b.name
    && Hypothesis.equivalent ~info_eq a.conclusion b.conclusion
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  module Parse = struct
    open Lvca_parsing

    let line : string option Lvca_parsing.t =
      lift3
        (fun _ _ ident -> ident)
        (Ws.string "--")
        (many (Ws.char '-'))
        (option None ((fun ident -> Some ident) <$> Ws.parens Ws.identifier))
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

  (* let erase (Typing (t1, t2)) = Typing (Nominal.Term.erase t1, Nominal.Term.erase t2) *)

  let equivalent ?(info_eq = fun _ _ -> true) (Typing (t11, t12)) (Typing (t21, t22)) =
    Nominal.Term.equivalent ~info_eq t11 t21 && Nominal.Term.equivalent ~info_eq t12 t22
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
end

type t = Rule.t list

(* let erase = List.map ~f:Rule.erase *)
let parse = Lvca_parsing.many Rule.Parse.t

let%test_module "Parsing" =
  (module struct
    let print_parse desc =
      let str =
        Lvca_parsing.(parse_string (whitespace *> parse)) desc
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

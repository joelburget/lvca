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
    open Lvca_parsing.Parser
    open Construction

    type arrow_dir =
      | Left_arr
      | Right_arr

    let t =
      let p =
        let+ tm = Binding_aware_pattern.parse
        and+ dir =
          choice
            ~failure_msg:"looking for <= or =>"
            [ (symbol "<=" >>| fun _ -> Left_arr); (symbol "=>" >>| fun _ -> Right_arr) ]
        and+ ty = Binding_aware_pattern.parse in
        match dir with
        | Left_arr -> Checking_rule { tm; ty }
        | Right_arr -> Inference_rule { tm; ty }
      in
      p <?> "typing clause"
    ;;
  end

  let parse = Parse.t

  let%test_module _ =
    (module struct
      let ( = ) = equivalent
      let here = Provenance.of_here [%here]
      let parse_exn = Lvca_parsing.Parser.parse_string_or_failwith parse
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
    open Lvca_parsing.Parser
    open Construction

    let typed_term =
      let p =
        let+ ident = lower_identifier <* symbol ":"
        and+ tm = Binding_aware_pattern.parse in
        ident, tm
      in
      p <?> "typed pattern"
    ;;

    let context =
      keyword "ctx"
      *> choice
           ~failure_msg:"Hypothesis"
           [ (let+ ctx_entries = symbol "," *> sep_by1 (symbol ",") typed_term in
              match String.Map.of_alist ctx_entries with
              | `Ok context -> context
              | `Duplicate_key str ->
                raise
                  (StaticsParseError (Printf.sprintf "duplicate name in context: %s" str)))
           ; return String.Map.empty
           ]
      <?> "context"
    ;;

    let t =
      let p =
        let+ ctx = context <* symbol ">>"
        and+ clause = Typing_clause.Parse.t in
        ctx, clause
      in
      p <?> "hypothesis"
    ;;
  end

  let%test_module "Parsing" =
    (module struct
      let here = Provenance.of_here [%here]
      let parse_exn = Lvca_parsing.Parser.parse_string_or_failwith Parse.t
      let print_parse = parse_exn >> pp Fmt.stdout

      let%test _ =
        let m, rule = parse_exn "ctx >> t1 <= Bool()" in
        Map.is_empty m
        && Typing_clause.(
             equivalent
               rule
               (Checking_rule { tm = Var (here, "t1"); ty = Operator (here, "Bool", []) }))
      ;;

      let%expect_test _ =
        print_parse "ctx >> t1 <= Bool()";
        [%expect "ctx >> t1 <= Bool()"]
      ;;

      let%expect_test _ =
        print_parse "ctx, x : t >> t1 => Bool()";
        [%expect "ctx, x : t >> t1 => Bool()"]
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
    open Lvca_parsing.Parser
    open Construction

    let line : string option Lvca_parsing.Parser.t =
      let p =
        let+ _ = symbol "--" (* TODO: allow longer separators *)
        and+ ident = option (parens lower_identifier) in
        ident
      in
      p <?> "line"
    ;;

    let t =
      let p =
        let+ hypotheses = plus Hypothesis.Parse.t
        and+ name = line
        and+ conclusion = Hypothesis.Parse.t in
        { hypotheses; name; conclusion }
      in
      p <?> "typing rule"
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

let parse = Lvca_parsing.Parser.Construction.plus Rule.Parse.t

let%test_module "Parsing" =
  (module struct
    let parse_exn = Lvca_parsing.Parser.parse_string_or_failwith parse
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
    ctx >> Str(x) => Str()

    ---
    ctx >> Num(x) => Num()

    ctx >> e1 <= Num()    ctx >> e2 <= Num()
    ---
    ctx >> Plus(e1; e2) => Num()

    ctx >> e1 <= Str()    ctx >> e2 <= Str()
    ---
    ctx >> Cat(e1; e2) => Str()

    ctx >> e <= Str()
    ---
    ctx >> Len(e) => Num()

    ctx >> e1 => t1   ctx, x : t1 >> e2 <= t2
    --- (let)
    ctx >> Let(e1; x. e2) <= t2
  |};
      [%expect {| parsed |}]
    ;;

    let%expect_test _ =
      print_parse {|

---
  ctx >> True() => Bool()

---
  ctx >> False() => Bool()

  |};
      [%expect {| parsed |}]
    ;;
  end)
;;

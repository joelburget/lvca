(** A small "core" language. *)

open Base
open Lvca_syntax
module Format = Caml.Format

type 'info n_term = ('info, Primitive.t) Nominal.Term.t
type 'info pattern = ('info, Primitive.t) BindingAwarePattern.t

type is_rec =
  | Rec
  | NoRec

let is_rec_equal x y = match x, y with Rec, Rec | NoRec, NoRec -> true | _, _ -> false

type 'info term =
  | Term of 'info n_term
  | CoreApp of 'info * 'info term * 'info term list
  | Case of 'info * 'info term * 'info cases (** Cases match patterns *)
  | Lambda of 'info * 'info Sort.t * 'info scope
      (** Lambdas bind variables. Patterns not allowed. *)
  | Let of 'info let_ (** Lets bind variables. Patterns not allowed. *)
  | Var of 'info * string

and 'info let_ =
  { info : 'info
  ; is_rec : is_rec
  ; tm : 'info term
  ; ty : 'info n_term option
  ; scope : 'info scope
  }

and 'info scope = Scope of string * 'info term

and 'info cases = 'info case_scope list

and 'info case_scope = CaseScope of 'info pattern * 'info term

let rec equal ~info_eq x y =
  match x, y with
  | Term x, Term y -> Nominal.Term.equal info_eq Primitive.( = ) x y
  | CoreApp (x1, x2, x3), CoreApp (y1, y2, y3) ->
    info_eq x1 y1 && equal ~info_eq x2 y2 && List.equal (equal ~info_eq) x3 y3
  | Case (x1, x2, x3), Case (y1, y2, y3) ->
    info_eq x1 y1 && equal ~info_eq x2 y2 && List.equal (case_scope_equal ~info_eq) x3 y3
  | Lambda (x1, x2, x3), Lambda (y1, y2, y3) ->
    info_eq x1 y1 && Sort.equal info_eq x2 y2 && scope_equal ~info_eq x3 y3
  | Let x, Let y -> let_equal ~info_eq x y
  | Var (x1, x2), Var (y1, y2) -> info_eq x1 y1 && String.(x2 = y2)
  | _, _ -> false

and case_scope_equal ~info_eq (CaseScope (x1, x2)) (CaseScope (y1, y2)) =
  BindingAwarePattern.equal info_eq Primitive.( = ) x1 y1 && equal ~info_eq x2 y2

and scope_equal ~info_eq (Scope (x1, x2)) (Scope (y1, y2)) =
  String.(x1 = y1) && equal ~info_eq x2 y2

and let_equal ~info_eq x y =
  info_eq x.info y.info
  && is_rec_equal x.is_rec y.is_rec
  && equal ~info_eq x.tm y.tm
  && Option.equal (Nominal.Term.equal info_eq Primitive.( = )) x.ty y.ty
  && scope_equal ~info_eq x.scope y.scope
;;

type 'a env = ('a, Primitive.t) Nominal.Term.t Lvca_util.String.Map.t

let preimage _ = failwith "TODO"
let reverse _tm _cases = failwith "TODO"

let rec map_info ~f = function
  | Term tm -> Term (Nominal.Term.map_info ~f tm)
  | CoreApp (info, t1, args) ->
    CoreApp (f info, map_info ~f t1, List.map ~f:(map_info ~f) args)
  | Case (info, tm, cases) -> Case (f info, map_info ~f tm, map_info_cases ~f cases)
  | Lambda (info, sort, scope) ->
    Lambda (f info, Sort.map_info ~f sort, map_info_core_scope ~f scope)
  | Let { info; is_rec; tm; ty; scope } ->
    Let
      { info = f info
      ; is_rec
      ; tm = map_info ~f tm
      ; ty = Option.map ~f:(Nominal.Term.map_info ~f) ty
      ; scope = map_info_core_scope ~f scope
      }
  | Var (info, name) -> Var (f info, name)

and map_info_core_scope ~f (Scope (name, tm)) = Scope (name, map_info ~f tm)
and map_info_cases ~f = List.map ~f:(map_info_case_scope ~f)

and map_info_case_scope ~f (CaseScope (pat, tm)) =
  CaseScope (BindingAwarePattern.map_info ~f pat, map_info ~f tm)
;;

let info = function
  | Term tm -> Nominal.Term.info tm
  | CoreApp (info, _, _)
  | Case (info, _, _)
  | Lambda (info, _, _)
  | Let { info; _ }
  | Var (info, _) ->
    info
;;

let erase tm = map_info ~f:(fun _ -> ()) tm

module PP = struct
  let braces, list, any, pf, sp = Fmt.(braces, list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  let rec pp ppf = function
    | Var (_, v) -> pf ppf "%s" v
    | Term tm -> pf ppf "%a" (braces (Nominal.Term.pp Primitive.pp)) tm
    | Lambda (_, sort, Scope (name, body)) ->
      pf ppf "\\(%s : %a) ->@ %a" name Sort.pp sort pp body
    (* TODO: parens if necessary *)
    | CoreApp (_, f, args) -> pf ppf "@[<h>%a@ @[<hov>%a@]@]" pp f (list ~sep:sp pp) args
    | Case (_, arg, cases) ->
      pf
        ppf
        "@[<hv>match %a with {%t%a@ }@]"
        pp
        arg
        (* Before `|`, emit a single space if on the same line, or two when broken *)
        (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
        pp_cases
        cases
    | Let { info = _; is_rec; tm; ty; scope = Scope (name, body) } ->
      let pp_ty ppf = function
        | Some ty -> pf ppf ": %a" (Nominal.Term.pp Primitive.pp) ty
        | None -> ()
      in
      pf
        ppf
        "@[let %s%s%a =@ %a in@ @[%a@]@]"
        (match is_rec with Rec -> "rec " | NoRec -> "")
        name
        pp_ty
        ty
        pp
        tm
        pp
        body

  and pp_cases ppf cases = list ~sep:(any "@;<1 2>| ") pp_core_case_scope ppf cases

  and pp_core_case_scope : Format.formatter -> 'a case_scope -> unit =
   fun ppf (CaseScope (pat, body)) ->
    pf ppf "@[%a@ -> %a@]" (BindingAwarePattern.pp Primitive.pp) pat pp body
 ;;
end

let pp : Format.formatter -> 'a term -> unit = PP.pp
let to_string : 'a term -> string = fun tm -> Format.asprintf "%a" pp tm

type 'info check_env = 'info Lvca_syntax.Sort.t Lvca_util.String.Map.t
type 'info check_error

let check _env tm =
  match tm with
  | Term _ -> failwith "TODO"
  | CoreApp _ -> failwith "TODO"
  | Case _ -> failwith "TODO"
  | Lambda _ -> failwith "TODO"
  | Let _ -> failwith "TODO"
  | Var _ -> failwith "TODO"
;;

let merge_results
    :  'a n_term Lvca_util.String.Map.t option list
    -> 'a n_term Lvca_util.String.Map.t option
  =
 fun results ->
  if List.for_all results ~f:Option.is_some
  then
    Some
      (results
      |> List.map
           ~f:(Lvca_util.Option.get_invariant (fun () -> "we just checked all is_some"))
      |> Lvca_util.String.Map.strict_unions
      |> function
      | `Duplicate_key k ->
        Lvca_util.invariant_violation
          (Printf.sprintf "multiple variables with the same name (%s) in one pattern" k)
      | `Ok m -> m)
  else None
;;

let rec match_pattern
    :  'a n_term -> ('b, Primitive.t) BindingAwarePattern.t
    -> 'a n_term Lvca_util.String.Map.t option
  =
 fun v pat ->
  match v, pat with
  | Operator (_, tag1, vals), Operator (_, tag2, pats) ->
    if String.(tag1 = tag2)
    then (
      match List.map2 pats vals ~f:match_pattern_scope with
      | Ok results -> merge_results results
      | Unequal_lengths -> None)
    else None
  | Primitive (_, l1), Primitive (_, l2) ->
    if Primitive.(l1 = l2) then Some Lvca_util.String.Map.empty else None
  | _, Ignored _ -> Some Lvca_util.String.Map.empty
  | tm, Var (_, v) -> Some (Lvca_util.String.Map.of_alist_exn [ v, tm ])
  | _ -> None

and match_pattern_scope
    (BindingAwarePattern.Scope (_namesXXX, pat))
    (Nominal.Scope.Scope (_patsXXX, body))
  =
  match_pattern body pat
;;

let find_match : 'a n_term -> 'b cases -> ('b term * 'a env) option =
 fun v branches ->
  branches
  |> List.find_map ~f:(fun (CaseScope (pat, rhs)) ->
         match match_pattern v pat with
         | None -> None
         | Some bindings -> Some (rhs, bindings))
;;

type 'a eval_error = string * 'a term

let true_tm info = Nominal.Term.Operator (info, "true", [])
let false_tm info = Nominal.Term.Operator (info, "false", [])

let eval_char_bool_fn eval_ctx' name f ctx tm c =
  let open Result.Let_syntax in
  let%bind c_result = eval_ctx' ctx c in
  match c_result with
  | Nominal.Term.Primitive (info, Primitive.PrimChar c') ->
    Ok (if f c' then true_tm info else false_tm info)
  | _ -> Error (Printf.sprintf "Invalid argument to %s" name, tm)
;;

type 'info primitive_eval =
  ('info env -> 'info term -> ('info n_term, 'info eval_error) Result.t)
  -> ('info env -> 'info n_term -> ('info n_term, 'info eval_error) Result.t)
  -> 'info env
  -> 'info term
  -> string
  -> 'info term list
  -> ('info n_term, 'info eval_error) Result.t

let rec eval_ctx
    :  'info primitive_eval -> 'info n_term Lvca_util.String.Map.t -> 'info term
    -> ('info n_term, 'info eval_error) Result.t
  =
 fun eval_primitive ctx tm ->
  let open Result.Let_syntax in
  let eval_ctx = eval_ctx eval_primitive in
  match tm with
  | Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, tm))
  | Case (_, tm, branches) ->
    let%bind tm_val = eval_ctx ctx tm in
    (match find_match tm_val branches with
    | None -> Error ("no match found in case", Term tm_val)
    | Some (branch, bindings) ->
      eval_ctx (Lvca_util.Map.union_right_biased ctx bindings) branch)
  | CoreApp (_, Lambda (_, _ty, Scope (name, body)), [ arg ]) ->
    let%bind arg_val = eval_ctx ctx arg in
    eval_ctx (Map.set ctx ~key:name ~data:arg_val) body
  | CoreApp (_, Var (_, name), args) ->
    if Map.mem ctx name
    then failwith "TODO"
    else eval_primitive eval_ctx eval_ctx' ctx tm name args
  | Term tm -> Ok (Nominal.Term.subst_all ctx tm)
  | Let { tm; scope = Scope (name, body); _ } ->
    let%bind tm_val = eval_ctx ctx tm in
    eval_ctx (Map.set ctx ~key:name ~data:tm_val) body
  | _ -> Error ("Found a term we can't evaluate", tm)

and eval_ctx'
    : 'a n_term Lvca_util.String.Map.t -> 'a n_term -> ('a n_term, 'a eval_error) Result.t
  =
 fun ctx tm ->
  match tm with
  | Var (_, v) ->
    (match Map.find ctx v with
    | Some result -> Ok result
    | None -> Error ("Unbound variable " ^ v, Term tm))
  | _ -> Ok tm
;;

let eval : 'a primitive_eval -> 'a term -> ('a n_term, 'a eval_error) Result.t =
 fun eval_primitive core -> eval_ctx eval_primitive Lvca_util.String.Map.empty core
;;

let eval_primitive eval_ctx eval_ctx' ctx tm name args =
  let open Result.Let_syntax in
  let open Nominal.Term in
  let open Lvca_syntax.Primitive in
  match name, args with
  | "add", [ Term a; Term b ] ->
    let%bind a_result = eval_ctx' ctx a in
    let%bind b_result = eval_ctx' ctx b in
    (match a_result, b_result with
    | Primitive (info, PrimInteger a'), Primitive (_, PrimInteger b') ->
      (* XXX can't reuse info *)
      Ok (Nominal.Term.Primitive (info, PrimInteger Z.(a' + b')))
    | _ -> Error ("Invalid arguments to add", tm))
  | "sub", [ Term a; Term b ] ->
    let%bind a_result = eval_ctx' ctx a in
    let%bind b_result = eval_ctx' ctx b in
    (match a_result, b_result with
    | Primitive (info, PrimInteger a'), Primitive (_, PrimInteger b') ->
      Ok (Nominal.Term.Primitive (info, PrimInteger Z.(a' - b')))
    | _ -> Error ("Invalid arguments to sub", tm))
  | "string_of_chars", [ char_list ] ->
    let%bind char_list = eval_ctx ctx char_list in
    (match char_list with
    | Operator (info, "list", chars) ->
      chars
      |> List.map ~f:(function
             | Nominal.Scope.Scope ([], Nominal.Term.Primitive (_, PrimChar c)) -> Ok c
             | tm ->
               Error
                 (Printf.sprintf
                    "string_of_chars `list(%s)`"
                    (Nominal.Scope.pp_str Lvca_syntax.Primitive.pp tm)))
      |> Result.all
      |> Result.map ~f:(fun cs ->
             Nominal.Term.Primitive (info, PrimString (Base.String.of_char_list cs)))
      |> Result.map_error ~f:(fun msg -> msg, tm)
    | _ -> Error ("expected a list of characters", tm))
  | "var", [ str_tm ] ->
    let%bind str = eval_ctx ctx str_tm in
    (match str with
    | Primitive (info, PrimString name) -> Ok (Nominal.Term.Var (info, name))
    | _ -> Error ("expected a string", tm))
  | "is_digit", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_digit" Char.is_digit ctx tm c
  | "is_lowercase", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_lowercase" Char.is_lowercase ctx tm c
  | "is_uppercase", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_uppercase" Char.is_uppercase ctx tm c
  | "is_alpha", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_alpha" Char.is_alpha ctx tm c
  | "is_alphanum", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_alphanum" Char.is_alphanum ctx tm c
  | "is_whitespace", [ Term c ] ->
    eval_char_bool_fn eval_ctx' "is_whitespace" Char.is_whitespace ctx tm c
  | _ ->
    failwith (Printf.sprintf "Unknown function (%s), or wrong number of arguments" name)
;;

module Parse (Comment : ParseUtil.Comment_int) = struct
  module Parsers = ParseUtil.Mk (Comment)
  module Term = Nominal.Term.Parse (Comment)
  module ParsePrimitive = Primitive.Parse (Comment)
  module Abstract = AbstractSyntax.Parse (Comment)
  module Sort = Sort.Parse (Comment)
  module BindingAwarePattern = BindingAwarePattern.Parse (Comment)
  open Parsers

  let reserved = Lvca_util.String.Set.of_list [ "let"; "rec"; "in"; "match"; "with" ]

  let identifier =
    identifier
    >>= fun ident -> if Set.mem reserved ident then fail "reserved word" else return ident
  ;;

  let make_apps : 'a term list -> 'a term = function
    | [] -> Lvca_util.invariant_violation "make_apps: must be a nonempty list"
    | [ x ] -> x
    | f :: args as xs ->
      let pos = xs |> List.map ~f:info |> OptRange.list_range in
      CoreApp (pos, f, args)
  ;;

  let term : OptRange.t term Parsers.t =
    fix (fun term ->
        let atomic_term =
          choice
            [ parens term
            ; (identifier >>|| fun ~pos ident -> Var (pos, ident), pos)
            ; braces (Term.t ParsePrimitive.t) >>| (fun tm -> Term tm) <?> "quoted term"
            ]
        in
        let pattern = BindingAwarePattern.t ParsePrimitive.t in
        let pattern = pattern <?> "pattern" in
        let case_line =
          lift3 (fun pat _ tm -> CaseScope (pat, tm)) pattern (string "->") term
          <?> "case line"
        in
        choice
          [ lift4
              (fun (_, lam_loc) ((name, sort), parens_loc) _ body ->
                let info = OptRange.union lam_loc parens_loc in
                Lambda (info, sort, Scope (name, body)))
              (attach_pos (char '\\'))
              (attach_pos
                 (parens
                    (lift3 (fun ident _ sort -> ident, sort) identifier (char ':') Sort.t)))
              (string "->")
              term
            <?> "lambda"
          ; lift4
              (fun (_let, let_pos) is_rec name ty _eq tm _in (body, body_pos) ->
                let info = OptRange.union let_pos body_pos in
                Let { info; is_rec; ty; tm; scope = Scope (name, body) })
              (attach_pos (string "let"))
              (option NoRec (Fn.const Rec <$> string "rec"))
              identifier
              (option None (char ':' *> Term.t ParsePrimitive.t >>| fun tm -> Some tm))
            <*> string "="
            <*> term
            <*> string "in"
            <*> attach_pos term
            <?> "let"
          ; lift4
              (fun (_match, match_pos) tm _with (lines, lines_pos) ->
                let pos = OptRange.union match_pos lines_pos in
                Case (pos, tm, lines))
              (attach_pos (string "match"))
              term
              (string "with")
              (attach_pos (braces (option '|' (char '|') *> sep_by (char '|') case_line)))
            <?> "match"
          ; many1 atomic_term >>| make_apps <?> "application"
          ])
    <?> "core term"
  ;;
end

let%test_module "Parsing" =
  (module struct
    module Parse = Parse (ParseUtil.NoComment)

    let parse str =
      ParseUtil.parse_string Parse.term str |> Result.ok_or_failwith |> erase
    ;;

    let ( = ) = equal ~info_eq:Unit.( = )
    let one = Nominal.Term.Primitive ((), Primitive.PrimInteger (Z.of_int 1))
    let var name = Var ((), name)
    let ignored name = BindingAwarePattern.Ignored ((), name)
    let operator tag children = Nominal.Term.Operator ((), tag, children)
    let app f a = CoreApp ((), f, a)

    let%test _ = parse "{1}" = Term one
    let%test _ = parse "{true()}" = Term (operator "true" [])
    let%test _ = parse "not x" = app (var "not") [ var "x" ]

    let%test _ =
      parse "let str = string_of_chars chars in {var(str)}"
      = Let
          { info = ()
          ; is_rec = NoRec
          ; ty = None
          ; tm = app (var "string_of_chars") [ var "chars" ]
          ; scope =
              Scope
                ( "str"
                , Term (operator "var" Nominal.[ Scope.Scope ([], Term.Var ((), "str")) ])
                )
          }
    ;;

    let%test _ =
      parse {|\(x : bool) -> x|}
      = Lambda ((), Sort.Name ((), "bool"), Scope ("x", var "x"))
    ;;

    let%test _ =
      parse {|match x with { _ -> {1} }|}
      = Case ((), var "x", [ CaseScope (ignored "", Term one) ])
    ;;

    let%test _ = parse {|match empty with { }|} = Case ((), var "empty", [])

    let%test _ =
      parse {|match x with { | _ -> {1} }|}
      = Case ((), var "x", [ CaseScope (ignored "", Term one) ])
    ;;

    let%test _ =
      parse {|match x with { true() -> {false()} | false() -> {true()} }|}
      = Case
          ( ()
          , var "x"
          , [ CaseScope (Operator ((), "true", []), Term (operator "false" []))
            ; CaseScope (Operator ((), "false", []), Term (operator "true" []))
            ] )
    ;;

    let%test _ =
      parse "let x = {true()} in not x"
      = Let
          { info = ()
          ; is_rec = NoRec
          ; ty = None
          ; tm = Term (operator "true" [])
          ; scope = Scope ("x", CoreApp ((), var "not", [ var "x" ]))
          }
    ;;
  end)
;;

let%test_module "Core parsing" =
  (module struct
    module ParseCore = Parse (ParseUtil.CComment)

    let scope : (unit, Primitive.t) Nominal.Term.t -> (unit, Primitive.t) Nominal.Scope.t =
     fun body -> Scope ([], body)
   ;;

    let dynamics_str =
      {|\(tm : ty) -> match tm with {
    | true() -> {true()}
    | false() -> {false()}
    | ite(t1; t2; t3) -> match meaning t1 with {
      | true()  -> meaning t2
      | false() -> meaning t3
    }
    | ap(f; arg) -> (meaning f) (meaning arg)
    | fun(scope) -> {lambda(list(); scope)} // TODO: add type
  }
  |}
    ;;

    let p_var name = BindingAwarePattern.Var ((), name)
    let c_var name = Var ((), name)

    let p_operator tag children =
      BindingAwarePattern.Operator
        ((), tag, children |> List.map ~f:(fun pat -> BindingAwarePattern.Scope ([], pat)))
    ;;

    let t_operator tag children = Nominal.Term.Operator ((), tag, children)
    let meaning x = CoreApp ((), c_var "meaning", [ x ])
    let ty = Sort.Name ((), "ty")

    let dynamics =
      Lambda
        ( ()
        , ty
        , Scope
            ( "tm"
            , Case
                ( ()
                , c_var "tm"
                , [ CaseScope (p_operator "true" [], Term (t_operator "true" []))
                  ; CaseScope (p_operator "false" [], Term (t_operator "false" []))
                  ; CaseScope
                      ( p_operator "ite" [ p_var "t1"; p_var "t2"; p_var "t3" ]
                      , Case
                          ( ()
                          , meaning (c_var "t1")
                          , [ CaseScope (p_operator "true" [], meaning (c_var "t2"))
                            ; CaseScope (p_operator "false" [], meaning (c_var "t3"))
                            ] ) )
                  ; CaseScope
                      ( p_operator "ap" [ p_var "f"; p_var "arg" ]
                      , CoreApp ((), meaning @@ c_var "f", [ meaning @@ c_var "arg" ]) )
                  ; CaseScope
                      ( p_operator "fun" [ p_var "scope" ]
                      , Term
                          (t_operator
                             "lambda"
                             [ scope @@ t_operator "list" []; scope @@ Var ((), "scope") ])
                      )
                  ] ) ) )
    ;;

    let%test "dynamics as expected" =
      let parse_term str =
        ParseUtil.parse_string ParseCore.term str |> Base.Result.ok_or_failwith
      in
      Caml.(parse_term dynamics_str |> erase = dynamics)
    ;;
  end)
;;

let%test_module "Core eval" =
  (module struct
    module ParseCore = Parse (ParseUtil.CComment)

    let eval_str str =
      let parse_term str =
        ParseUtil.parse_string ParseCore.term str |> Base.Result.ok_or_failwith
      in
      let core = parse_term str in
      let result =
        match eval eval_primitive core with
        | Error (msg, tm) -> msg ^ ": " ^ to_string tm
        | Ok result -> Nominal.Term.pp_str Primitive.pp result
      in
      Stdio.print_string result
    ;;

    let%expect_test _ =
      eval_str "{1}";
      [%expect {| 1 |}]
    ;;

    let%expect_test _ =
      eval_str "{foo(1)}";
      [%expect {| foo(1) |}]
    ;;

    let%expect_test _ =
      eval_str "{true()}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "{false()}";
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str
        {|match {true()} with {
          | true() -> {false()}
          | false() -> {true()}
        }
      |};
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str {|(\(x: bool) -> x) {true()}|};
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "add {1} {2}";
      [%expect {| 3 |}]
    ;;

    let%expect_test _ =
      eval_str "sub {1} {2}";
      [%expect {| -1 |}]
    ;;

    let%expect_test _ =
      eval_str "is_digit {'9'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_digit {'a'}";
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha {'a'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha {'A'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alpha {'9'}";
      [%expect {| false() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum {'9'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum {'Z'}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      eval_str "is_alphanum {'_'}";
      [%expect {| false() |}]
    ;;

    (* let%expect_test _ = eval_str "sub 1 2"; [%expect{| -1 |}] *)
  end)
;;

let%test_module "Core pretty" =
  (module struct
    module ParseCore = Parse (ParseUtil.CComment)

    let pretty width str =
      let str =
        match
          ParseUtil.parse_string Angstrom.(ParseUtil.whitespace *> ParseCore.term) str
        with
        | Error err -> err
        | Ok core ->
          let module Format = Stdlib.Format in
          let fmt = Format.str_formatter in
          Format.pp_set_geometry fmt ~max_indent:width ~margin:(width + 1);
          pp fmt core;
          Format.flush_str_formatter ()
      in
      Stdio.print_string str
    ;;

    let%expect_test _ =
      pretty 22 "match {true()} with { true() -> {false()} | false() -> {true()} }";
      [%expect
        {|
        match {true()} with {
          | true()
            -> {false()}
          | false()
            -> {true()}
        } |}]
    ;;

    let%expect_test _ =
      pretty 23 "match {true()} with { true() -> {false()} | false() -> {true()} }";
      [%expect
        {|
        match {true()} with {
          | true() -> {false()}
          | false() -> {true()}
        } |}]
    ;;

    let%expect_test _ =
      pretty 25 "match x with { _ -> {1} }";
      [%expect {| match x with { _ -> {1} } |}]
    ;;

    let%expect_test _ =
      pretty 24 "match x with { _ -> {1} }";
      [%expect {|
        match x with {
          | _ -> {1}
        } |}]
    ;;

    let%expect_test _ =
      pretty 20 "foo a b c d e f g h i j k l";
      [%expect {|
        foo a b c d e f g h
            i j k l |}]
    ;;

    let%expect_test _ =
      pretty 20 "f a b c d e f g h i j k l";
      [%expect {|
        f a b c d e f g h i
          j k l |}]
    ;;

    let%expect_test _ =
      pretty 20 "let x = {true()} in not x";
      [%expect {|
        let x = {true()} in
        not x |}]
    ;;

    let%expect_test _ =
      pretty 20 "let x: bool = {true()} in not x";
      [%expect {|
        let x: bool =
        {true()} in not x |}]
    ;;
  end)
;;

let%test_module "Core eval in dynamics" =
  (module struct
    module ParseCore = Parse (ParseUtil.CComment)

    let eval_in dynamics_str str =
      let parse_term str =
        ParseUtil.parse_string ParseCore.term str |> Base.Result.ok_or_failwith
      in
      let defn = parse_term dynamics_str in
      let core = parse_term str in
      match eval eval_primitive (CoreApp (None, defn, [ core ])) with
      | Error (msg, tm) -> msg ^ ": " ^ to_string tm
      | Ok result -> Nominal.Term.pp_str Primitive.pp result
    ;;

    let dynamics_str =
      {|\(tm : ty) -> match tm with {
  | true() -> {true()}
  | false() -> {false()}
  | ite(t1; t2; t3) -> match meaning t1 with {
    | true()  -> meaning t2
    | false() -> meaning t3
  }
  | ap(f; arg) -> (meaning f) (meaning arg)
  | fun(scope) -> {lambda(list(); scope)} // TODO: add type
}
      |}
    ;;

    let%expect_test _ =
      Stdio.print_string @@ eval_in dynamics_str "{true()}";
      [%expect {| true() |}]
    ;;

    let id_dynamics = {|\(tm : ty) -> tm|}

    let%expect_test _ =
      Stdio.print_string @@ eval_in id_dynamics "{true()}";
      [%expect {| true() |}]
    ;;

    let%expect_test _ =
      Stdio.print_string @@ eval_in id_dynamics "{lambda(tm. tm; list(ty()))}";
      [%expect {| lambda(tm. tm; list(ty())) |}]
    ;;
  end)
;;

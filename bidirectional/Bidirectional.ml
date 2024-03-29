open Base
open Lvca_syntax
open Statics
module SMap = Lvca_util.String.Map

module Env = struct
  type t =
    { rules : Rule.t list (** The (checking / inference) rules we can apply *)
    ; var_types : Nominal.Term.t SMap.t (** The types of all known free variables *)
    }

  let equivalent ?(info_eq = fun _ _ -> true) a b =
    List.equal (Rule.equivalent ~info_eq) a.rules b.rules
    && Map.equal (Nominal.Term.equivalent ~info_eq) a.var_types b.var_types
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
end

module Capture = Binding_aware_pattern.Capture

module Check_error = struct
  type t =
    | Check_error of string
    | Bad_merge of Capture.t * Capture.t

  let equivalent ?(info_eq = fun _ _ -> true) t1 t2 =
    match t1, t2 with
    | Check_error msg1, Check_error msg2 -> String.(msg1 = msg2)
    | Bad_merge (cap11, cap12), Bad_merge (cap21, cap22) ->
      Capture.equivalent ~info_eq cap11 cap21 && Capture.equivalent ~info_eq cap12 cap22
    | _, _ -> false
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )

  let pp ppf = function
    | Check_error msg -> Fmt.string ppf msg
    | Bad_merge (cap1, cap2) ->
      Fmt.pf
        ppf
        "%a / %a"
        Binding_aware_pattern.Capture.pp
        cap1
        Binding_aware_pattern.Capture.pp
        cap2
  ;;
end

module Trace_entry = struct
  type t =
    | Check_trace of Env.t * Typing.t
    | Check_success
    | Check_failure of Check_error.t
    | Infer_trace of Env.t * Nominal.Term.t
    | Inferred of Nominal.Term.t

  let equivalent ?(info_eq = fun _ _ -> true) t1 t2 =
    match t1, t2 with
    | Check_trace (a_env, a_typing), Check_trace (b_env, b_typing) ->
      Env.equivalent ~info_eq a_env b_env && Typing.equivalent ~info_eq a_typing b_typing
    | Check_success, Check_success -> true
    | Check_failure a, Check_failure b -> Check_error.equivalent ~info_eq a b
    | Infer_trace (a_env, a_tm), Infer_trace (b_env, b_tm) ->
      Env.equivalent ~info_eq a_env b_env && Nominal.Term.equivalent ~info_eq a_tm b_tm
    | Inferred a, Inferred b -> Nominal.Term.equivalent ~info_eq a b
    | _, _ -> false
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
end

module Trace_step = struct
  type t = Trace_entry.t list

  let equivalent ?(info_eq = fun _ _ -> true) xs ys =
    List.equal (Trace_entry.equivalent ~info_eq) xs ys
  ;;

  let ( = ) = equivalent ~info_eq:Provenance.( = )
end

(* [pat] is a (binding-aware) pattern taken from the typing rule. Use the
   values in the context to fill it in. *)
let instantiate
    :  string option -> Capture.t SMap.t -> Binding_aware_pattern.t
    -> (Nominal.Term.t, Check_error.t) Result.t
  =
 fun name env pat ->
  let open Result.Let_syntax in
  let err msg =
    let msg =
      match name with
      | Some name -> Printf.sprintf "instantiate %s: %s" name msg
      | None -> Printf.sprintf "instantiate: %s" msg
    in
    Error (Check_error.Check_error msg)
  in
  let rec go_term pat =
    match pat with
    | Binding_aware_pattern.Operator (info, op_name, scopes) ->
      let%map scopes = scopes |> List.map ~f:go_scope |> Result.all in
      Nominal.Term.Operator (info, op_name, scopes)
    | Var (_info, pattern_var_name) ->
      (match Map.find env pattern_var_name with
      | Some (Binding_aware_pattern.Capture.Term tm) -> Ok tm
      | Some _ ->
        err
          (Printf.sprintf
             "%s: found a captured binder but expected a term"
             pattern_var_name)
      | None -> err (Printf.sprintf "didn't find variable %s in context" pattern_var_name))
    | Primitive p -> Ok (Primitive p)
  and go_scope (Scope (binders, body)) =
    let%bind binders =
      binders
      |> List.map ~f:(fun (_info, binder_name) ->
             match Map.find env binder_name with
             | Some (Term _) ->
               err
                 (Printf.sprintf
                    "%s: found a captured term but expected a binder"
                    binder_name)
             | Some (Binder pat) -> Ok pat
             | None ->
               err (Printf.sprintf "didn't find variable %s in context" binder_name))
      |> Result.all
    in
    let%map body = go_term body in
    Nominal.Scope.Scope (binders, body)
  in
  go_term pat
;;

let update_ctx : Capture.t SMap.t ref -> Capture.t SMap.t -> Check_error.t option =
 fun ctx_state learned_tys ->
  let do_assignment (k, v) =
    match Map.find !ctx_state k with
    | None ->
      let state = Map.remove !ctx_state k in
      ctx_state := Map.set state ~key:k ~data:v;
      None
    | Some v' ->
      if not (Binding_aware_pattern.Capture.equivalent v v')
      then Some (Check_error.Bad_merge (v, v'))
      else None
  in
  learned_tys |> Map.to_alist |> List.find_map ~f:do_assignment
;;

let ctx_infer
    : Nominal.Term.t SMap.t -> Nominal.Term.t -> (Nominal.Term.t, Check_error.t) Result.t
  =
 fun var_types -> function
  | Nominal.Term.Var (_, name) ->
    (match Map.find var_types name with
    | None ->
      let var_types = var_types |> Map.keys |> String.concat ~sep:", " in
      Error
        (Check_error
           (Printf.sprintf
              "ctx_infer: couldn't find variable %s among {%s}"
              name
              var_types))
    | Some ty -> Ok ty)
  | tm ->
    Error
      (Check_error
         (Fmt.str "unable to infer type (no matching rule) for %a" Nominal.Term.pp tm))
;;

let rec check'
    : Trace_step.t -> (Trace_step.t -> unit) -> Env.t -> Typing.t -> Check_error.t option
  =
 fun trace_stack emit_trace ({ rules; _ } as env) (Typing (tm, ty) as typing) ->
  let trace_entry = Trace_entry.Check_trace (env, typing) in
  let trace_stack = trace_entry :: trace_stack in
  emit_trace trace_stack;
  let match_rule = function
    | Rule.{ conclusion = _, Inference_rule _; _ } -> None
    (* TODO: the conclusion may have a context, which we're currently ignoring *)
    | { name; hypotheses; conclusion = _, Checking_rule { tm = rule_tm; ty = rule_ty } }
      ->
      let match1 = Binding_aware_pattern.match_term rule_tm tm in
      let match2 = Binding_aware_pattern.match_term rule_ty ty in
      (match match1, match2 with
      | Some tm_assignments, Some ty_assignments ->
        Some (name, hypotheses, tm_assignments, ty_assignments)
      | _ -> None)
  in
  match rules |> List.find_map ~f:match_rule with
  | None -> Some (Check_error "check': no matching rule found")
  | Some (name, hypotheses, tm_assignments, ty_assignments) ->
    (match SMap.strict_union tm_assignments ty_assignments with
    | `Duplicate_key k ->
      Some (Check_error ("Found two different bindings for the same variable: " ^ k))
    | `Ok schema_assignments ->
      (* ctx_state is a mapping of schema variables we've learned:
       * We fill this in initially with `schema_assignments` from matching the
     conclusion.
       * It's also updated as we evaluate the hypotheses (from left to right) if
     we learn any more assignments. Example:

     ctx >> tm1 => arr(ty1; ty2)   ctx >> tm2 <= ty1
     ------- (infer app)
     ctx >> app(tm1; tm2) => ty2

     Here we initially learn tm1, tm2, and ty2, but only learn ty1 via
     checking of hypotheses.
       *)
      let ctx_state : Capture.t SMap.t ref = ref schema_assignments in
      let result =
        hypotheses
        |> List.find_map ~f:(check_hyp trace_stack emit_trace name ctx_state env)
      in
      let trace_entry =
        match result with
        | None -> Trace_entry.Check_success
        | Some err -> Check_failure err
      in
      emit_trace (trace_entry :: trace_stack);
      result)

and infer'
    :  Trace_step.t -> (Trace_step.t -> unit) -> Env.t -> Nominal.Term.t
    -> (Nominal.Term.t, Check_error.t) Result.t
  =
 fun trace_stack emit_trace ({ rules; var_types } as env) tm ->
  let open Result.Let_syntax in
  let var_types : Nominal.Term.t SMap.t ref = ref var_types in
  let trace_stack = Trace_entry.Infer_trace (env, tm) :: trace_stack in
  emit_trace trace_stack;
  let match_rule Rule.{ name; hypotheses; conclusion } =
    match conclusion with
    | _, Checking_rule _ -> None
    (* TODO: the conclusion may have a context, which we're currently ignoring *)
    | _, Inference_rule { tm = rule_tm; ty = rule_ty } ->
      let match_ = Binding_aware_pattern.match_term rule_tm tm in
      match_
      |> Option.map ~f:(fun schema_assignments ->
             name, hypotheses, schema_assignments, rule_ty)
  in
  let%map ty =
    match List.find_map rules ~f:match_rule with
    | Some (name, hypotheses, schema_assignments, rule_ty) ->
      let ctx_state : Capture.t SMap.t ref = ref schema_assignments in
      (match
         hypotheses
         |> List.find_map ~f:(check_hyp trace_stack emit_trace name ctx_state env)
       with
      | None -> instantiate name !ctx_state rule_ty
      | Some err -> Error err)
    | None -> ctx_infer !var_types tm
  in
  emit_trace (Inferred ty :: trace_stack);
  ty

(* Check (or infer, depending on the rule) a hypothesis *)
and check_hyp
    :  Trace_step.t -> (Trace_step.t -> unit) -> string option -> Capture.t SMap.t ref
    -> Env.t -> Hypothesis.t -> Check_error.t option
  =
 fun trace_stack emit_trace name ctx_state env (pattern_ctx, rule) ->
  let open Result.Let_syntax in
  match
    let%map var_type_list =
      (* For every variable in the context, for example, rule:

        ctx >> e1 => t1     ctx, x : t1 >> e2 => t2
        ---
        ctx >> Let(e1; x. e2) => t2

      and term [Let(Num(1); v. v)]:

        [x] is the only variable that appears in a context. We first look up
        its [ty], in this case [t1]. Then we find the name the variable is
        actually called in the term ([v]).
        *)
      pattern_ctx
      |> Map.to_alist
      |> List.map ~f:(fun (ctx_var_name, pat) ->
             match pat with
             | Binding_aware_pattern.Var (_, ctx_ty_name) ->
               let%bind ty =
                 match Map.find_exn !ctx_state ctx_ty_name with
                 | Term ty -> Ok ty
                 | Binder pat ->
                   Error
                     (Fmt.str
                        "Expected type reference in context to refer to a term, not a \
                         binder (%a)"
                        Pattern.pp
                        pat)
               in
               let%map term_var_name =
                 match Map.find_exn !ctx_state ctx_var_name with
                 | Binder (Pattern.Var (_, name)) -> Ok name
                 | Binder pat ->
                   Error
                     (Fmt.str
                        "Binding non-variable patterns (%a) is not yet supported! This \
                         should be supported but we need to extend bidirectional to have \
                         a notion of pattern types (ty1[ty2]) TODO!"
                        Pattern.pp
                        pat)
                 | Term tm ->
                   Error
                     (Fmt.str
                        "Expected a binder but found a term (%a) -- only binders can \
                         occur in the context"
                        Nominal.Term.pp
                        tm)
               in
               term_var_name, ty
             | pat ->
               Error
                 (Fmt.str
                    "Binding non-variable patterns (%a) is not yet supported! This \
                     should be supported but we need to extend bidirectional to have a \
                     notion of pattern types (ty1[ty2]) TODO!"
                    Binding_aware_pattern.pp
                    pat))
      |> Result.all
    in
    SMap.of_alist_exn var_type_list
  with
  | Error msg -> Some (Check_error msg)
  | Ok new_var_types ->
    let var_types = SMap.unions_right_biased [ env.var_types; new_var_types ] in
    let env = { env with var_types } in
    (match rule with
    | Checking_rule { tm = hyp_tm; ty = hyp_ty } ->
      let tm_ty =
        let%bind tm = instantiate name !ctx_state hyp_tm in
        let%map ty = instantiate name !ctx_state hyp_ty in
        tm, ty
      in
      (match tm_ty with
      | Ok (tm, ty) -> check' trace_stack emit_trace env (Typing (tm, ty))
      | Error err -> Some err)
    | Inference_rule { tm = hyp_tm; ty = hyp_ty } ->
      let ty =
        let%bind tm = instantiate name !ctx_state hyp_tm in
        infer' trace_stack emit_trace env tm
      in
      (match ty with
      | Ok ty ->
        (match Binding_aware_pattern.match_term hyp_ty ty with
        | None -> Some (Check_error "check_hyp: failed to match schema vars")
        | Some learned_tys -> update_ctx ctx_state learned_tys)
      | Error err -> Some err))
;;

let check_trace f env typing = check' [] f env typing
let infer_trace f env term = infer' [] f env term
let check env typing = check_trace (fun _ -> ()) env typing
let infer env term = infer_trace (fun _ -> ()) env term

let%test_module "check / infer" =
  (module struct
    let reserved = Lvca_util.String.Set.empty
    let parse p = Lvca_parsing.(parse_string (whitespace *> p))
    let parse_statics = parse Statics.parse
    let parse_tm = parse (Nominal.Term.parse' reserved)

    let rules =
      {|
    --- (str_literal)
    ctx >> Str(x) => Str()

    --- (num_literal)
    ctx >> Num(x) => Num()

    ctx >> e1 <= Num()    ctx >> e2 <= Num()
    --- (plus)
    ctx >> Plus(e1; e2) => Num()

    ctx >> e1 <= Str()    ctx >> e2 <= Str()
    --- (cat)
    ctx >> Cat(e1; e2) => Str()

    ctx >> e <= Str()
    --- (len)
    ctx >> Len(e) => Num()

    ctx >> e1 => t1   ctx, x : t1 >> e2 => t2
    --- (let)
    ctx >> Let(e1; x. e2) => t2

    ctx >> e => t
    --- (infer_check)
    ctx >> e <= t
  |}
      |> parse_statics
      |> Result.ok_or_failwith
    ;;

    let print_check ~tm ~ty =
      let env = Env.{ rules; var_types = SMap.empty } in
      match parse_tm tm, parse_tm ty with
      | Ok tm, Ok ty ->
        (match check env (Typing (tm, ty)) with
        | None -> ()
        | Some err -> Fmt.pr "%a\n" Check_error.pp err)
      | _, _ -> failwith "failed to parse term or type"
    ;;

    let print_infer tm =
      let env = Env.{ rules; var_types = SMap.empty } in
      match parse_tm tm with
      | Ok tm ->
        (match infer env tm with
        | Ok ty -> Fmt.pr "%a\n" Nominal.Term.pp ty
        | Error err -> Fmt.pr "%a\n" Check_error.pp err)
      | _ -> failwith "failed to parse term"
    ;;

    let%expect_test _ =
      print_check ~tm:"Num(1)" ~ty:"Num()";
      [%expect {| |}]
    ;;

    let%expect_test _ =
      print_infer "Num(1)";
      [%expect {| Num() |}]
    ;;

    let%expect_test _ =
      print_check ~tm:"Num(1)" ~ty:"Num()";
      [%expect {| |}]
    ;;

    let%expect_test _ =
      print_infer {| Len(Str("foo")) |};
      [%expect {| Num() |}]
    ;;

    let%expect_test _ =
      print_check ~tm:{| Len(Str("foo")) |} ~ty:"Num()";
      [%expect {| |}]
    ;;

    let%expect_test _ =
      print_infer {| Plus(Len(Str("foo")); Num(1)) |};
      [%expect {| Num() |}]
    ;;

    let%expect_test _ =
      print_infer {| Let(Num(1); v. v) |};
      [%expect {| Num() |}]
    ;;

    let%expect_test _ =
      print_infer {| Let(Len(Str("foo")); x. Plus(x; Num(1))) |};
      [%expect {| Num() |}]
    ;;

    let%expect_test _ =
      print_infer {| Let(Num(1); x. Let(Str("foo"); y. y)) |};
      [%expect {| Str() |}]
    ;;

    let%expect_test _ =
      print_infer {| Let(Num(1); x. Let(Str("foo"); x. x)) |};
      [%expect {| Str() |}]
    ;;
  end)
;;

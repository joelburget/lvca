open Base
open Lvca_syntax
open Statics
module SMap = Lvca_util.String.Map

type 'info capture = 'info BindingAwarePattern.capture

type 'a env =
  { rules : 'a Rule.t list (** The (checking / inference) rules we can apply *)
  ; var_types : 'a Nominal.Term.t SMap.t (** The types of all known free variables *)
  }

type 'info check_error =
  | CheckError of string
  | BadMerge of 'info capture * 'info capture

type 'a trace_entry =
  | CheckTrace of 'a env * 'a Typing.t
  | CheckSuccess
  | CheckFailure of 'a check_error
  | InferTrace of 'a env * 'a Nominal.Term.t
  | Inferred of 'a Nominal.Term.t

type 'a trace_step = 'a trace_entry list

let pp_bpat : _ BindingAwarePattern.t Fmt.t = BindingAwarePattern.pp
let pp_pat = Pattern.pp
let pp_capture = BindingAwarePattern.pp_capture
let pp_term = Nominal.Term.pp

let pp_err ppf = function
  | CheckError msg -> Fmt.string ppf msg
  | BadMerge (cap1, cap2) -> Fmt.pf ppf "%a / %a" pp_capture cap1 pp_capture cap2
;;

(* [pat] is a (binding-aware) pattern taken from the typing rule. Use the
   values in the context to fill it in. *)
let instantiate
    :  string option -> 'info capture SMap.t -> 'info BindingAwarePattern.t
    -> ('info Nominal.Term.t, 'info check_error) Result.t
  =
 fun name env pat ->
  let open Result.Let_syntax in
  let err msg =
    let msg =
      match name with
      | Some name -> Printf.sprintf "instantiate %s: %s" name msg
      | None -> Printf.sprintf "instantiate: %s" msg
    in
    Error (CheckError msg)
  in
  let rec go_term pat =
    match pat with
    | BindingAwarePattern.Operator (info, op_name, scopes) ->
      let%map scopes = scopes |> List.map ~f:go_scope |> Result.all in
      Nominal.Term.Operator (info, op_name, scopes)
    | Var (_info, pattern_var_name) ->
      (match Map.find env pattern_var_name with
      | Some (BindingAwarePattern.CapturedTerm tm) -> Ok tm
      | Some _ ->
        err
          (Printf.sprintf
             "%s: found a captured binder but expected a term"
             pattern_var_name)
      | None -> err (Printf.sprintf "didn't find variable %s in context" pattern_var_name))
    | Primitive p -> Ok (Primitive p)
    | Ignored (_, name) ->
      err (Printf.sprintf "Can't instantiate ignored variable _%s" name)
  and go_scope (Scope (binders, body)) =
    let%bind binders =
      binders
      |> List.map ~f:(fun (_info, binder_name) ->
             match Map.find env binder_name with
             | Some (CapturedTerm _) ->
               err
                 (Printf.sprintf
                    "%s: found a captured term but expected a binder"
                    binder_name)
             | Some (CapturedBinder pat) -> Ok pat
             | None ->
               err (Printf.sprintf "didn't find variable %s in context" binder_name))
      |> Result.all
    in
    let%map body = go_term body in
    Nominal.Scope.Scope (binders, body)
  in
  go_term pat
;;

let info_eq _ _ = true

let update_ctx
    : 'info capture SMap.t ref -> 'info capture SMap.t -> 'info check_error option
  =
 fun ctx_state learned_tys ->
  let do_assignment (k, v) =
    match Map.find !ctx_state k with
    | None ->
      let state = Map.remove !ctx_state k in
      ctx_state := Map.set state ~key:k ~data:v;
      None
    | Some v' ->
      if not (BindingAwarePattern.capture_eq ~info_eq v v')
      then Some (BadMerge (v, v'))
      else None
  in
  learned_tys |> Map.to_alist |> List.find_map ~f:do_assignment
;;

let ctx_infer
    :  'info Nominal.Term.t SMap.t -> 'info Nominal.Term.t
    -> ('info Nominal.Term.t, 'info check_error) Result.t
  =
 fun var_types -> function
  | Nominal.Term.Var (_, name) ->
    (match Map.find var_types name with
    | None ->
      let var_types = var_types |> Map.keys |> String.concat ~sep:", " in
      Error
        (CheckError
           (Printf.sprintf
              "ctx_infer: couldn't find variable %s among {%s}"
              name
              var_types))
    | Some ty -> Ok ty)
  | tm ->
    Error
      (CheckError (Fmt.str "unable to infer type (no matching rule) for %a" pp_term tm))
;;

let rec check'
    :  'info trace_step -> ('info trace_step -> unit) -> 'info env -> 'info Typing.t
    -> 'info check_error option
  =
 fun trace_stack emit_trace ({ rules; _ } as env) (Typing (tm, ty) as typing) ->
  let trace_entry = CheckTrace (env, typing) in
  let trace_stack = trace_entry :: trace_stack in
  emit_trace trace_stack;
  let match_rule = function
    | Rule.{ conclusion = _, InferenceRule _; _ } -> None
    (* TODO: the conclusion may have a context, which we're currently ignoring *)
    | { name; hypotheses; conclusion = _, CheckingRule { tm = rule_tm; ty = rule_ty } } ->
      let match1 = BindingAwarePattern.match_term ~info_eq:(fun _ _ -> true) rule_tm tm in
      let match2 = BindingAwarePattern.match_term ~info_eq:(fun _ _ -> true) rule_ty ty in
      (match match1, match2 with
      | Some tm_assignments, Some ty_assignments ->
        Some (name, hypotheses, tm_assignments, ty_assignments)
      | _ -> None)
  in
  match rules |> List.find_map ~f:match_rule with
  | None -> Some (CheckError "check': no matching rule found")
  | Some (name, hypotheses, tm_assignments, ty_assignments) ->
    (match SMap.strict_union tm_assignments ty_assignments with
    | `Duplicate_key k ->
      Some (CheckError ("Found two different bindings for the same variable: " ^ k))
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
      let ctx_state : 'a capture SMap.t ref = ref schema_assignments in
      let result =
        hypotheses
        |> List.find_map ~f:(check_hyp trace_stack emit_trace name ctx_state env)
      in
      let trace_entry =
        match result with None -> CheckSuccess | Some err -> CheckFailure err
      in
      emit_trace (trace_entry :: trace_stack);
      result)

and infer'
    :  'info trace_step -> ('info trace_step -> unit) -> 'info env -> 'info Nominal.Term.t
    -> ('info Nominal.Term.t, 'info check_error) Result.t
  =
 fun trace_stack emit_trace ({ rules; var_types } as env) tm ->
  let open Result.Let_syntax in
  let var_types : 'a Nominal.Term.t SMap.t ref = ref var_types in
  let trace_stack = InferTrace (env, tm) :: trace_stack in
  emit_trace trace_stack;
  let match_rule Rule.{ name; hypotheses; conclusion } =
    match conclusion with
    | _, CheckingRule _ -> None
    (* TODO: the conclusion may have a context, which we're currently ignoring *)
    | _, InferenceRule { tm = rule_tm; ty = rule_ty } ->
      let match_ = BindingAwarePattern.match_term ~info_eq:(fun _ _ -> true) rule_tm tm in
      match_
      |> Option.map ~f:(fun schema_assignments ->
             name, hypotheses, schema_assignments, rule_ty)
  in
  let%map ty =
    match List.find_map rules ~f:match_rule with
    | Some (name, hypotheses, schema_assignments, rule_ty) ->
      let ctx_state : 'a capture SMap.t ref = ref schema_assignments in
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
    :  'info trace_step -> ('info trace_step -> unit) -> string option
    -> 'a capture SMap.t ref -> 'a env -> 'a Hypothesis.t -> 'a check_error option
  =
 fun trace_stack emit_trace name ctx_state env (pattern_ctx, rule) ->
  let open Result.Let_syntax in
  match
    let%map var_type_list =
      (* For every variable in the context, for example, rule:

        ctx >> e1 => t1     ctx, x : t1 >> e2 => t2
        ---
        ctx >> let(e1; x. e2) => t2

      and term [let(num(1); v. v)]:

        [x] is the only variable that appears in a context. We first look up
        its [ty], in this case [t1]. Then we find the name the variable is
        actually called in the term ([v]).
        *)
      pattern_ctx
      |> Map.to_alist
      |> List.map ~f:(fun (ctx_var_name, pat) ->
             match pat with
             | BindingAwarePattern.Var (_, ctx_ty_name) ->
               let%bind ty =
                 match Map.find_exn !ctx_state ctx_ty_name with
                 | CapturedTerm ty -> Ok ty
                 | CapturedBinder pat ->
                   Error
                     (Fmt.str
                        "Expected type reference in context to refer to a term, not a \
                         binder (%a)"
                        pp_pat
                        pat)
               in
               let%map term_var_name =
                 match Map.find_exn !ctx_state ctx_var_name with
                 | CapturedBinder (Pattern.Var (_, name)) -> Ok name
                 | CapturedBinder pat ->
                   Error
                     (Fmt.str
                        "Binding non-variable patterns (%a) is not yet supported! This \
                         should be supported but we need to extend bidirectional to have \
                         a notion of pattern types (ty1[ty2]) TODO!"
                        pp_pat
                        pat)
                 | CapturedTerm tm ->
                   Error
                     (Fmt.str
                        "Expected a binder but found a term (%a) -- only binders can \
                         occur in the context"
                        pp_term
                        tm)
               in
               term_var_name, ty
             | pat ->
               Error
                 (Fmt.str
                    "Binding non-variable patterns (%a) is not yet supported! This \
                     should be supported but we need to extend bidirectional to have a \
                     notion of pattern types (ty1[ty2]) TODO!"
                    pp_bpat
                    pat))
      |> Result.all
    in
    SMap.of_alist_exn var_type_list
  with
  | Error msg -> Some (CheckError msg)
  | Ok new_var_types ->
    let var_types = SMap.unions_right_biased [ env.var_types; new_var_types ] in
    let env = { env with var_types } in
    (match rule with
    | CheckingRule { tm = hyp_tm; ty = hyp_ty } ->
      let tm_ty =
        let%bind tm = instantiate name !ctx_state hyp_tm in
        let%map ty = instantiate name !ctx_state hyp_ty in
        tm, ty
      in
      (match tm_ty with
      | Ok (tm, ty) -> check' trace_stack emit_trace env (Typing (tm, ty))
      | Error err -> Some err)
    | InferenceRule { tm = hyp_tm; ty = hyp_ty } ->
      let ty =
        let%bind tm = instantiate name !ctx_state hyp_tm in
        infer' trace_stack emit_trace env tm
      in
      (match ty with
      | Ok ty ->
        (match BindingAwarePattern.match_term ~info_eq:(fun _ _ -> true) hyp_ty ty with
        | None -> Some (CheckError "check_hyp: failed to match schema vars")
        | Some learned_tys -> update_ctx ctx_state learned_tys)
      | Error err -> Some err))
;;

let check_trace f env typing = check' [] f env typing
let infer_trace f env term = infer' [] f env term
let check env typing = check_trace (fun _ -> ()) env typing
let infer env term = infer_trace (fun _ -> ()) env term

let%test_module "check / infer" =
  (module struct
    let parse_statics = Lvca_parsing.parse_string Statics.Parse.whitespace_t
    let parse_tm = Lvca_parsing.parse_string Nominal.Term.Parse.whitespace_t

    let rules =
      {|
    --- (str_literal)
    ctx >> str(x) => str()

    --- (num_literal)
    ctx >> num(x) => num()

    ctx >> e1 <= num()    ctx >> e2 <= num()
    --- (plus)
    ctx >> plus(e1; e2) => num()

    ctx >> e1 <= str()    ctx >> e2 <= str()
    --- (cat)
    ctx >> cat(e1; e2) => str()

    ctx >> e <= str()
    --- (len)
    ctx >> len(e) => num()

    ctx >> e1 => t1   ctx, x : t1 >> e2 => t2
    --- (let)
    ctx >> let(e1; x. e2) => t2

    ctx >> e => t
    --- (infer_check)
    ctx >> e <= t
  |}
      |> parse_statics
      |> Result.ok_or_failwith
    ;;

    let print_check ~tm ~ty =
      let env = { rules; var_types = SMap.empty } in
      match parse_tm tm, parse_tm ty with
      | Ok tm, Ok ty ->
        (match check env (Typing (tm, ty)) with
        | None -> ()
        | Some err -> Fmt.pr "%a\n" pp_err err)
      | _, _ -> failwith "failed to parse term or type"
    ;;

    let print_infer tm =
      let env = { rules; var_types = SMap.empty } in
      match parse_tm tm with
      | Ok tm ->
        (match infer env tm with
        | Ok ty -> Fmt.pr "%a\n" pp_term ty
        | Error err -> Fmt.pr "%a\n" pp_err err)
      | _ -> failwith "failed to parse term"
    ;;

    let%expect_test _ =
      print_check ~tm:"num(1)" ~ty:"num()";
      [%expect {| |}]
    ;;

    let%expect_test _ =
      print_infer "num(1)";
      [%expect {| num() |}]
    ;;

    let%expect_test _ =
      print_check ~tm:"num(1)" ~ty:"num()";
      [%expect {| |}]
    ;;

    let%expect_test _ =
      print_infer {| len(str("foo")) |};
      [%expect {| num() |}]
    ;;

    let%expect_test _ =
      print_check ~tm:{| len(str("foo")) |} ~ty:"num()";
      [%expect {| |}]
    ;;

    let%expect_test _ =
      print_infer {| plus(len(str("foo")); num(1)) |};
      [%expect {| num() |}]
    ;;

    let%expect_test _ =
      print_infer {| let(num(1); v. v) |};
      [%expect {| num() |}]
    ;;

    let%expect_test _ =
      print_infer {| let(len(str("foo")); x. plus(x; num(1))) |};
      [%expect {| num() |}]
    ;;

    let%expect_test _ =
      print_infer {| let(num(1); x. let(str("foo"); y. y)) |};
      [%expect {| str() |}]
    ;;

    let%expect_test _ =
      print_infer {| let(num(1); x. let(str("foo"); x. x)) |};
      [%expect {| str() |}]
    ;;
  end)
;;

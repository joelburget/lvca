open Lvca_syntax
open Statics
module Fn = Base.Fn
module List = Base.List
module Map = Base.Map
module Option = Base.Option
module Result = Base.Result
module String = Lvca_util.String

type 'a env =
  { rules : 'a rule list (** The (checking / inference) rules we can apply *)
  ; var_types : 'a term String.Map.t (** The types of all known free variables *)
  }

let enscope (binders : 'a Pattern.t list) (Scope (binders', body)) =
  Scope (binders' @ binders, body)
;;

(** Only used internally to match_schema_vars *)
exception NoMatch

let rec match_schema_vars' : 'a term -> 'a term -> 'a scope String.Map.t =
 fun t1 t2 ->
  match t1, t2 with
  | Free (_, v), tm -> String.Map.of_alist_exn [ v, Scope ([], [tm]) ]
  | Operator (_, tag1, args1), Operator (_, tag2, args2) ->
    if String.(tag1 = tag2) && List.(length args1 = length args2)
    then
      let matched_scopes = List.map2_exn args1 args2 ~f:match_schema_vars_scope in
      match String.Map.strict_unions matched_scopes with
        | `Ok result -> result
        | `Duplicate_key str -> failwith ("TODO: error: duplicate key: " ^ str)
    else raise NoMatch
  | _, _ -> raise NoMatch

and match_schema_vars_scope (Scope (names1, body1)) (Scope (names2, body2)) =
  if List.(length names1 = length names2)
     (* TODO: is it okay to use names1? what happens to names2? *)
  then
    match
      List.zip_exn body1 body2
        |> List.map ~f:(fun (tm1, tm2) -> match_schema_vars' tm1 tm2
          |> Map.map ~f:(enscope names1))
        |> String.Map.strict_unions
    with
      | `Ok map -> map
      | `Duplicate_key name -> failwith ("TODO error: duplicate key: " ^ name)
  else raise NoMatch
;;

let match_schema_vars : 'a term -> 'a term -> 'a scope String.Map.t option =
 fun t1 t2 -> try Some (match_schema_vars' t1 t2) with NoMatch -> None
;;

(** Open a scope, instantiating all of its bound variables *)
let open_scope (args : 'a term list list) (Scope (names, body))
  : 'a term list option
  =
  if List.(length args <> length names)
  then None
  else (
    let rec open' offset tm =
      match tm with
      | Operator (loc, tag, subtms) ->
        subtms
        |> List.map ~f:(fun (Scope (binders, subtms)) -> subtms
          |> List.map ~f:(open' (offset + List.length binders))
          |> Option.all
          |> Option.map ~f:(fun subtms' -> Scope (binders, subtms')))
        |> Option.all
        |> Option.map ~f:(fun subtms' -> Operator (loc, tag, subtms'))
      | Bound (_, i, j) ->
        if i >= offset
        then
          args
          |> Fn.flip List.nth (i - offset)
          |> Option.bind ~f:(fun args' -> List.nth args' j)
        else Some tm
      | Free _ -> Some tm
      | Primitive _ -> Some tm
    in
    body
      |> List.map ~f:(open' 0)
      |> Option.all)
;;

(** Create free variables from a pattern *)
let pat_to_free_vars
  = fun pat -> pat
    |> Pattern.list_vars_of_pattern
    |> List.map ~f:(fun (loc, name) -> Free (loc, name))
;;

let rec instantiate (env : 'a scope String.Map.t) (tm : 'a term)
  : ('a term, string) Result.t
  = match tm with
  | Operator (loc, tag, subtms) ->
    subtms
    |> List.map ~f:(fun (Scope (binders, body)) ->
      let new_var_names : string array =
        binders
        |> List.map ~f:(fun pat -> pat
          |> Pattern.list_vars_of_pattern
          |> List.map ~f:(fun (_loc, name) -> name)
          |> Array.of_list)
        |> Array.concat
      in
      body
        |> List.map ~f:(instantiate (Lvca_util.Map.remove_many env new_var_names))
        |> Result.all
        |> Result.map ~f:(fun body' -> Scope (binders, body')))
    |> Result.all
    |> Result.map ~f:(fun subtms' -> Operator (loc, tag, subtms'))
  | Bound _ -> Ok tm
  | Free (_, v) ->
    (match Map.find env v with
    | None -> Error ("instantiate: couldn't find var " ^ v)
    | Some (Scope (pats, _) as sc)
    (* Open the scope, instantiating all variables it binds as free *) ->
      (match open_scope (List.map pats ~f:pat_to_free_vars) sc with
      | None -> Error "instantiate: failed to open scope"
      | Some [tm] -> Ok tm (* XXX *)
      | Some _ -> Error "XXX"))
  | Primitive _ -> Ok tm
;;

(* TODO: remove exceptions, unit -> 'a *)
exception BadTermMerge of unit term * unit term
exception BadScopeMerge of unit scope * unit scope

(* TODO: remove? *)
(* let safe_union m1 m2 : 'a String.Map.t = String.Map.merge m1 m2 ~f:(fun ~key:_ ->
   function | `Both (v1, v2) -> if Caml.(v1 = v2) then Some v1 else raise (BadTermMerge
   (v1, v2)) | `Left v | `Right v -> Some v ) *)

exception CheckError of string

let update_ctx (ctx_state : scope String.Map.t ref) (learned_tys : scope String.Map.t) =
  let do_assignment (k, v) =
    match Map.find !ctx_state k with
    | None ->
      let state' = Map.remove !ctx_state k in
      ctx_state := Map.set state' ~key:k ~data:v
    | Some v' -> if Caml.(v <> v') then raise (BadScopeMerge (v, v'))
  in
  List.iter ~f:do_assignment (Map.to_alist learned_tys)
;;

let get_or_raise msg = function Some x -> x | None -> raise (CheckError msg)

let raise_if_not_ok outer_msg : ('a, 'err) Result.t -> 'a = function
  | Ok x -> x
  | Error msg -> raise (CheckError (outer_msg ^ ":\n" ^ msg))
;;

let ctx_infer (var_types : term String.Map.t) : term -> term = function
  | Free v ->
    (match Map.find var_types v with
    | None -> raise (CheckError ("ctx_infer: couldn't find variable " ^ v))
    | Some ty -> ty)
  | _ -> raise (CheckError "ctx_infer: called with non-free-variable")
;;

type trace_entry =
  | CheckTrace of env * typing
  | CheckSuccess
  | CheckFailure of string
  | InferTrace of env * term
  | Inferred of term

type trace_step = trace_entry list

let rec check' trace_stack emit_trace ({ rules; _ } as env) (Typing (tm, ty) as typing) =
  let trace_entry = CheckTrace (env, typing) in
  let trace_stack' = trace_entry :: trace_stack in
  emit_trace trace_stack';
  let match_rule = function
    | { conclusion = _, InferenceRule _; _ } -> None
    (* XXX conclusion context *)
    | { name; hypotheses; conclusion = _, CheckingRule { tm = rule_tm; ty = rule_ty } } ->
      let match1 = match_schema_vars rule_tm tm in
      let match2 = match_schema_vars rule_ty ty in
      (match match1, match2 with
      | Some tm_assignments, Some ty_assignments ->
        Some (name, hypotheses, tm_assignments, ty_assignments)
      | _ -> None)
  in
  let name, hypotheses, tm_assignments, ty_assignments =
    get_or_raise "check': no matching rule found" (List.find_map rules ~f:match_rule)
  in
  (* TODO: check term / type assignments disjoint *)
  let schema_assignments =
    match String.Map.strict_union tm_assignments ty_assignments with
    | `Ok assignments -> assignments
    | `Duplicate_key k -> failwith ("TODO: check' error for duplicate key: " ^ k)
  in
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
  let ctx_state : scope String.Map.t ref = ref schema_assignments in
  (* let name' = match name with | None -> "infer" | Some name -> "infer " ^ name in *)
  try
    List.iter ~f:(check_hyp trace_stack' emit_trace name ctx_state env) hypotheses;
    emit_trace (CheckSuccess :: trace_stack')
  with
  | CheckError msg as err ->
    emit_trace (CheckFailure msg :: trace_stack');
    raise err
  | BadTermMerge (v1, v2) ->
    let msg =
      Printf.sprintf "bad merge: %s vs %s" (string_of_term v1) (string_of_term v2)
    in
    emit_trace (CheckFailure msg :: trace_stack');
    raise (BadTermMerge (v1, v2))
  | BadScopeMerge (v1, v2) ->
    let msg =
      Printf.sprintf "bad merge: %s vs %s" (string_of_scope v1) (string_of_scope v2)
    in
    emit_trace (CheckFailure msg :: trace_stack');
    raise (BadScopeMerge (v1, v2))

(* TODO: catch other exception? *)
and infer' trace_stack emit_trace ({ rules; var_types } as env) tm =
  let trace_stack' = InferTrace (env, tm) :: trace_stack in
  emit_trace trace_stack';
  let match_rule { name; hypotheses; conclusion } =
    match conclusion with
    | _, CheckingRule _ -> None
    (* XXX conclusion context *)
    | _, InferenceRule { tm = rule_tm; ty = rule_ty } ->
      match_schema_vars rule_tm tm
      |> Option.map ~f:(fun schema_assignments ->
             name, hypotheses, schema_assignments, rule_ty)
  in
  let ty =
    match List.find_map rules ~f:match_rule with
    | Some (name, hypotheses, schema_assignments, rule_ty) ->
      let ctx_state : scope String.Map.t ref = ref schema_assignments in
      let name' = match name with None -> "infer'" | Some name -> "infer' " ^ name in
      List.iter ~f:(check_hyp trace_stack' emit_trace name ctx_state env) hypotheses;
      instantiate !ctx_state rule_ty |> raise_if_not_ok name'
    | None -> ctx_infer var_types tm
  in
  emit_trace (Inferred ty :: trace_stack');
  ty

and check_hyp trace_stack emit_trace name ctx_state env (_pattern_ctx, rule) =
  let name' = match name with None -> "check_hyp" | Some name -> "check_hyp " ^ name in
  match rule with
  | CheckingRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = instantiate !ctx_state hyp_tm |> raise_if_not_ok name' in
    let ty = instantiate !ctx_state hyp_ty |> raise_if_not_ok name' in
    check' trace_stack emit_trace env (Typing (tm, ty))
  | InferenceRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = instantiate !ctx_state hyp_tm |> raise_if_not_ok name' in
    let ty = infer' trace_stack emit_trace env tm in
    let learned_tys =
      get_or_raise "check_hyp: failed to match schema vars" (match_schema_vars hyp_ty ty)
    in
    update_ctx ctx_state learned_tys
;;

let check_trace = check' []
let infer_trace = infer' []
let check = check_trace (fun _ -> ())
let infer = infer_trace (fun _ -> ())

let%test_module "bidirectional tests" =
  (module struct

    let statics_str =
      {|

  ----------------------- (infer_true)
  ctx >> true() => bool()

  ------------------------ (infer_false)
  ctx >> false() => bool()

  ctx >> tm1 => arr(ty1; ty2)   ctx >> tm2 <= ty1
  ----------------------------------------------- (infer_app)
            ctx >> app(tm1; tm2) => ty2

      ctx, x : ty1 >> body <= ty2
  ------------------------------------ (check_lam)
  ctx >> lam(x. body) <= arr(ty1; ty2)

       ctx >> tm <= ty
  -------------------------- (infer_annot)
  ctx >> annot(tm; ty) => ty

  ctx >> t1 <= bool()  ctx >> t2 => ty  ctx >> t3 => ty
  ----------------------------------------------------- (infer_ite)
             ctx >> ite(t1; t2; t3) => ty

  ctx >> tm => ty
  --------------- (reverse)
  ctx >> tm <= ty
  |}
    ;;

    module Parse = Statics.Parse(Lvca_util.Angstrom.NoComment);;
    module NominalParse = Binding.Nominal.Parse(Lvca_util.Angstrom.NoComment)

    let statics =
      match
        Angstrom.parse_string ~consume:All
          Angstrom.(Lvca_util.Angstrom.whitespace *> Parse.t)
          statics_str
      with
        | Ok statics -> statics
        | Error err -> failwith err
    ;;

    let parse_cvt : string -> term
      = fun str ->
      let tm = match Angstrom.parse_string ~consume:All NominalParse.t str with
        | Ok tm -> tm
        | Error err -> failwith err
      in
      let tm' =
        match Binding.DeBruijn.from_nominal tm with
        | Ok tm -> tm
        | Error msg -> failwith msg
      in
      Statics.of_de_bruijn tm'
    ;;

    let true_tm = parse_cvt "true()"
    let bool_ty = parse_cvt "bool()"
    let env = { rules = statics; var_types = String.Map.empty }
    let ite = parse_cvt "ite(true(); false(); true())"
    let annot_ite = parse_cvt "annot(ite(true(); false(); true()); bool())"
    let lam_tm = parse_cvt "lam(x. true())"
    let bool_to_bool = parse_cvt "arr(bool(); bool())"
    let annot_lam = parse_cvt "annot(lam(x. true()); arr(bool(); bool()))"

    let app_annot =
      Statics.(Operator ("app", [ Scope ([], [annot_lam]); Scope ([], [true_tm]) ]))
    ;;

    let ( = ) = Caml.( = )

    let%test "check true() : bool()" = check env (Typing (true_tm, bool_ty)) = ()
    let%test "infer true()" = infer env true_tm = bool_ty
    let%test "check ite" = check env (Typing (ite, bool_ty)) = ()
    let%test "infer ite" = infer env ite = bool_ty
    let%test "infer annotated ite" = infer env annot_ite = bool_ty
    let%test "check lam" = check env (Typing (lam_tm, bool_to_bool)) = ()
    let%test "infer annotated lam" = infer env annot_lam = bool_to_bool
    let%test "check annotated lam" = check env (Typing (annot_lam, bool_to_bool)) = ()
    let%test "infer annotated app" = infer env app_annot = bool_ty
  end)
;;
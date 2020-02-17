open Statics
open Core_kernel

type env = {
  rules     : rule list
  (** The (checking / inference) rules we can apply *)
; var_types : term String.Map.t
  (** The types of all known free variables *)
}

let enscope (binders : Pattern.t list) (Scope (binders', body))
  = Scope (binders' @ binders, body)

(** Only used internally to match_schema_vars  *)
exception NoMatch

let rec match_schema_vars' t1 t2 = match t1, t2 with
  | Free v, tm -> String.Map.of_alist [ v, Scope ([], tm) ]
  | Operator (tag1, args1), Operator (tag2, args2)
    -> if tag1 = tag2 && List.(length args1 = length args2)
    then Util.map_unions @@ List.map2 args1 args2 ~f:match_schema_vars_scope
    else raise NoMatch

and match_schema_vars_scope (Scope (names1, body1)) (Scope (names2, body2)) =
  if List.(length names1 = length names2)
  (* TODO: is it okay to use names1? what happens to names2? *)
  then match_schema_vars' body1 body2 |> String.Map.map ~f:(enscope names1)
  else raise NoMatch

let match_schema_vars : term -> term -> scope String.Map.t option
  = fun t1 t2 ->
    try
      Some (match_schema_vars' t1 t2)
    with
      NoMatch -> None

(** Open a scope, instantiating all of its bound variables *)
let open_scope (args : term list list) (Scope (names, body)) : term option
  = if List.(length args <> length names)
  then None
  else
    let rec open' offset tm = match tm with
      | Operator (tag, subtms) -> subtms
                                  |> List.map ~f:(fun (Scope (binders, subtm)) ->
                                    open' (offset + List.length binders) subtm
                                    |> Option.map ~f:(fun subtm' -> Scope (binders, subtm'))
                                  )
                                  |> Util.sequence_list_option
                                  |> Option.map ~f:(fun subtms' -> Operator (tag, subtms'))
      | Bound (i, j) -> if i >= offset
        then args
          |> List.nth (i - offset)
          |> Option.bind ~f:(fun args' -> List.nth args' j)
        else Some tm
      | Free _ -> Some tm
      | Sequence tms -> tms
                        |> List.map ~f:(open' offset)
                        |> Util.sequence_list_option
                        |> Option.map ~f:(fun tms' -> Sequence tms')
      | Primitive _ -> Some tm
    in open' 0 body

(** Create free variables from a pattern *)
let pat_to_free_vars : Pattern.t -> term list
  = fun pat -> Pattern.list_vars_of_pattern pat
               |> List.map ~f:(fun name -> Free name)

let rec instantiate (env : scope String.Map.t) (tm : term)
  : (string, term) Result.t
  = match tm with
  | Operator (tag, subtms) -> subtms
                              |> List.map ~f:(fun (Scope (binders, body)) ->
                                let new_var_names = binders
                                                    |. Array.of_list
                                                    |. Array.map
                                                         ~f:(fun pat -> Array.of_list (Pattern.list_vars_of_pattern pat))
                                                    |. Array.concatenate
                                in
                                instantiate (String.Map.remove_many env new_var_names) body
                                |> Result.map (fun body' -> Scope (binders, body'))
                              )
                              |> Util.sequence_list_result
                              |> Result.map (fun subtms' -> Operator (tag, subtms'))
  | Bound _ -> Ok tm
  | Free v -> (match String.Map.find env v with
    | None -> Error ("instantiate: couldn't find var " ^ v)
    | Some (Scope (pats, _) as sc)

      (* Open the scope, instantiating all variables it binds as free *)
      -> (match open_scope (List.map pats ~f:pat_to_free_vars) sc with
        | None -> Error "instantiate: failed to open scope"
        | Some tm -> Ok tm
      )
  )
  | Sequence tms -> tms
                    |> List.map ~f:(instantiate env)
                    |> Util.sequence_list_result
                    |> Result.map (fun tms' -> Sequence tms')
  | Primitive _ -> Ok tm

exception BadTermMerge of term * term
exception BadScopeMerge of scope * scope

(* TODO: remove? *)
let safe_union m1 m2 : 'a String.Map.t = String.Map.merge m1 m2 ~f:(fun _ mv1 mv2 ->
  match mv1, mv2 with
  | Some v, None
  | None, Some v
    -> Some v
  | Some v1, Some v2
    -> if v1 = v2 then Some v1 else raise (BadTermMerge(v1, v2))
  | _ -> assert false
)

exception CheckError of string

let update_ctx (ctx_state : scope String.Map.t ref) (learned_tys : scope String.Map.t) =
  let do_assignment = fun (k, v) -> match String.Map.find !ctx_state k with
    | None    -> ctx_state := String.Map.add !ctx_state ~key:k ~data:v
    | Some v' -> if v <> v' then raise (BadScopeMerge(v, v'))
  in
  List.iter do_assignment (String.Map.to_list learned_tys)

let get_or_raise msg = function
  | Some x -> x
  | None   -> raise (CheckError msg)

let raise_if_not_ok outer_msg : ('err, 'a) Result.t -> 'a
  = function
  | Ok x -> x
  | Error msg -> raise (CheckError (outer_msg ^ ":\n" ^ msg))

let ctx_infer (var_types : term String.Map.t) : term -> term = function
  | Free v -> (match String.Map.find var_types v with
    | None -> raise (CheckError ("ctx_infer: couldn't find variable " ^ v))
    | Some ty -> ty
  )
  | tm -> raise (CheckError "ctx_infer: called with non-free-variable")

type trace_entry =
  | CheckTrace of env * typing
  | CheckSuccess
  | CheckFailure of string
  | InferTrace of env * term
  | Inferred   of term

type trace_step = trace_entry list

let rec check' trace_stack emit_trace ({ rules } as env) (Typing (tm, ty) as typing)
  = let trace_entry = CheckTrace (env, typing) in
  let trace_stack' = trace_entry :: trace_stack in
  emit_trace trace_stack';
  let match_rule = function
    | { conclusion = _, InferenceRule _ } -> None
    (* XXX conclusion context *)
    | { name; hypotheses; conclusion = _, CheckingRule { tm = rule_tm; ty = rule_ty } } -> (
        let match1 = match_schema_vars rule_tm tm in
        let match2 = match_schema_vars rule_ty ty in
        match match1, match2 with
        | Some tm_assignments, Some ty_assignments
          -> Some (name, hypotheses, tm_assignments, ty_assignments)
        | _
          -> None
      )
  in let (name, hypotheses, tm_assignments, ty_assignments) =
       get_or_raise "check': no matching rule found" (Util.first_by rules match_rule) in
  (* TODO: check term / type assignments disjoint *)
  let schema_assignments = Util.map_union tm_assignments ty_assignments in
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
  let name' = match name with
    | None      -> "infer"
    | Some name -> "infer " ^ name
  in
  try
    List.iter (check_hyp trace_stack' emit_trace name ctx_state env) hypotheses;
    emit_trace (CheckSuccess :: trace_stack');
  with
  | (CheckError msg) as err ->
    emit_trace (CheckFailure msg :: trace_stack');
    raise err
  | BadTermMerge (v1, v2) ->
    let msg = Printf.sprintf
                "bad merge: %s vs %s"
                (string_of_term v1)
                (string_of_term v2)
    in
    emit_trace (CheckFailure msg :: trace_stack');
    raise (BadTermMerge (v1, v2))
  | BadScopeMerge (v1, v2) ->
    let msg = Printf.sprintf
                "bad merge: %s vs %s"
                (string_of_scope v1)
                (string_of_scope v2)
    in
    emit_trace (CheckFailure msg :: trace_stack');
    raise (BadScopeMerge (v1, v2))
(* TODO: catch other exception? *)

and infer' trace_stack emit_trace ({ rules; var_types } as env) tm
  = let trace_stack' = InferTrace (env, tm) :: trace_stack in
  emit_trace trace_stack';
  let match_rule = fun { name; hypotheses; conclusion } -> match conclusion with
    | _, CheckingRule _ -> None
    (* XXX conclusion context *)
    | _, InferenceRule { tm = rule_tm; ty = rule_ty } ->
      match_schema_vars rule_tm tm
      |> Option.map ~f:(fun schema_assignments ->
        (name, hypotheses, schema_assignments, rule_ty))
  in
  let ty = match Util.first_by rules match_rule with
    | Some (name, hypotheses, schema_assignments, rule_ty)
      -> let ctx_state : scope String.Map.t ref = ref schema_assignments in
      let name' = match name with
        | None      -> "infer'"
        | Some name -> "infer' " ^ name
      in
      List.iter
        (check_hyp trace_stack' emit_trace name ctx_state env)
        hypotheses;
      instantiate !ctx_state rule_ty
      |> raise_if_not_ok name'
    | None -> ctx_infer var_types tm
  in
  emit_trace (Inferred ty :: trace_stack');
  ty

and check_hyp = fun trace_stack emit_trace name ctx_state env (pattern_ctx, rule) ->
  let name' = match name with
    | None      -> "check_hyp"
    | Some name -> "check_hyp " ^ name
  in
  match rule with
  | CheckingRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = instantiate !ctx_state hyp_tm |> raise_if_not_ok name' in
    let ty = instantiate !ctx_state hyp_ty |> raise_if_not_ok name' in
    check' trace_stack emit_trace env (Typing (tm, ty))
  | InferenceRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = instantiate !ctx_state hyp_tm |> raise_if_not_ok name' in
    let ty = infer' trace_stack emit_trace env tm in
    let learned_tys = get_or_raise
                        "check_hyp: failed to match schema vars"
                        (match_schema_vars hyp_ty ty)
    in
    update_ctx ctx_state learned_tys

let check_trace = check' []
let infer_trace = infer' []
let check = check_trace (fun _ -> ())
let infer = infer_trace (fun _ -> ())

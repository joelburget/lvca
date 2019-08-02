open Types.Statics
module M = Belt.Map.String
module BL = Belt.List
module BO = Belt.Option

type env = {
    (** The (checking / inference) rules we can apply *)
    rules     : rule list;
    (** The types of all known free variables *)
    var_types : term M.t;
  }

let enscope (binders : string list) (Scope (binders', body))
  = Scope (binders' @ binders, body)

(** Only used internally to match_schema_vars  *)
exception NoMatch

let rec match_schema_vars' t1 t2 = match t1, t2 with
  | Free v, tm -> M.fromArray [| v, Scope ([], tm) |]
  | Term (tag1, args1), Term (tag2, args2)
  -> if tag1 = tag2 && BL.(length args1 = length args2)
     then Util.unions @@ BL.zipBy args1 args2 match_schema_vars_scope
     else raise NoMatch

and match_schema_vars_scope (Scope (names1, body1)) (Scope (names2, body2)) =
  if BL.(length names1 = length names2)
  (* TODO: is it okay to use names1? what happens to names2? *)
  then match_schema_vars' body1 body2 |. M.map (enscope names1)
  else raise NoMatch

let match_schema_vars : term -> term -> scope M.t option
  = fun t1 t2 ->
    try
      Some (match_schema_vars' t1 t2)
    with
      NoMatch -> None

(** Open a scope, instantiating all of its bound variables *)
let open_scope (args : term list) (Scope (names, body)) : term option
  = if BL.(length args <> length names)
    then None
    else
      let rec open' offset tm = match tm with
        | Term (tag, subtms) -> subtms
          |. BL.map (fun (Scope (binders, subtm)) ->
            open' (offset + BL.length binders) subtm
            |. BO.map (fun subtm' -> Scope (binders, subtm'))
          )
          |. Util.sequence_list_option
          |. BO.map (fun subtms' -> Term (tag, subtms'))
        | Bound i -> if i >= offset
          then BL.get args (i - offset)
          else Some tm
        | Free _ -> Some tm
        | Sequence tms -> tms
          |. BL.map (open' offset)
          |. Util.sequence_list_option
          |. BO.map (fun tms' -> Sequence tms')
        | Primitive _ -> Some tm
      in open' 0 body

let rec instantiate (env : scope M.t) (tm : term) : term option = match tm with
  | Term (tag, subtms) -> subtms
    |. BL.map (fun (Scope (binders, body)) ->
      instantiate (M.removeMany env (BL.toArray binders)) body
      |. BO.map (fun body' -> Scope (binders, body'))
    )
    |. Util.sequence_list_option
    |. BO.map (fun subtms' -> Term (tag, subtms'))
  | Bound _ -> Some tm
  | Free v -> (match M.get env v with
    | None -> None (* TODO: describe error *)
    | Some (Scope (names, _) as sc)
    -> open_scope (names |. BL.map (fun name -> Free name)) sc
  )
  | Sequence tms -> tms
    |. BL.map (instantiate env)
    |. Util.sequence_list_option
    |. BO.map (fun tms' -> Sequence tms')
  | Primitive _ -> Some tm

exception BadMerge

(* TODO: remove? *)
let safe_union m1 m2 : 'a M.t = M.merge m1 m2 (fun _ mv1 mv2 ->
  match mv1, mv2 with
  | Some v, None
  | None, Some v
  -> Some v
  | Some v1, Some v2
  -> if v1 = v2 then Some v1 else raise BadMerge
  | _ -> raise BadMerge
  )

exception CheckError of string

let update_ctx (ctx_state : scope M.t ref) (learned_tys : scope M.t) =
  let ctx = !ctx_state in
  let do_assignment = fun (k, v) -> match M.get ctx k with
    | None -> ctx_state := M.set ctx k v
    | Some v' -> if v <> v' then raise BadMerge
  in
  List.iter do_assignment (learned_tys |. M.toList)

let get_or_raise msg = function
  | Some x -> x
  | None   -> raise (CheckError msg)

let rec check ({ rules } as env : env) (Typing (tm, ty)) : unit
  = let match_rule = function
      | { conclusion = _, InferenceRule _ } -> None
      (* XXX conclusion context *)
      | { hypotheses; conclusion = _, CheckingRule { tm = rule_tm; ty = rule_ty } } -> (
        let match1 = match_schema_vars rule_tm tm in
        let match2 = match_schema_vars rule_ty ty in
        match match1, match2 with
          | Some tm_assignments, Some ty_assignments
          -> Some (hypotheses, tm_assignments, ty_assignments)
          | _
          -> None
      )
    in let (hypotheses, tm_assignments, ty_assignments) =
      get_or_raise "1" (Util.first_by rules match_rule) in
    (* ctx_state is a mapping of type variables we've learned *)
    let ctx_state : scope M.t ref = ref M.empty in
    (* TODO: check term / type assignments disjoint *)
    let assignments = Util.union tm_assignments ty_assignments in
    List.iter (check_hyp ctx_state assignments env) hypotheses

and infer ({ rules } as env : env) (tm : term) : term
  = let match_rule = fun { hypotheses; conclusion } -> match conclusion with
    | _, CheckingRule _ -> None
    (* XXX conclusion context *)
    | _, InferenceRule { tm = rule_tm; ty = rule_ty } ->
      match_schema_vars rule_tm tm
      |. BO.map (fun assignments -> (hypotheses, assignments, rule_ty))
    in let (hypotheses, assignments, rule_ty) =
      get_or_raise "2" (Util.first_by rules match_rule) in
    let ctx_state : scope M.t ref = ref M.empty in
    List.iter (check_hyp ctx_state assignments env) hypotheses;
    get_or_raise "3" (instantiate !ctx_state rule_ty)

and check_hyp = fun ctx_state assignments env (pattern_ctx, rule) -> match rule with
  | CheckingRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = get_or_raise "4" (instantiate assignments hyp_tm) in
    let ty = get_or_raise "5" (instantiate assignments hyp_ty) in
    check env (Typing (tm, ty))
  | InferenceRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = get_or_raise "6" (instantiate assignments hyp_tm) in
    let ty = infer env tm in
    let learned_tys = get_or_raise "7" (match_schema_vars hyp_ty ty) in
    update_ctx ctx_state learned_tys

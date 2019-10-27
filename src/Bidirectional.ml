open Statics
module M = Belt.Map.String
module BL = Belt.List
module BO = Belt.Option
module BR = Belt.Result

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
  | Operator (tag1, args1), Operator (tag2, args2)
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
        | Operator (tag, subtms) -> subtms
          |. BL.map (fun (Scope (binders, subtm)) ->
            open' (offset + BL.length binders) subtm
            |. BO.map (fun subtm' -> Scope (binders, subtm'))
          )
          |. Util.sequence_list_option
          |. BO.map (fun subtms' -> Operator (tag, subtms'))
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

let rec instantiate (env : scope M.t) (tm : term) : (term, string) BR.t = match tm with
  | Operator (tag, subtms) -> subtms
    |. BL.map (fun (Scope (binders, body)) ->
      instantiate (M.removeMany env (BL.toArray binders)) body
      |. BR.map (fun body' -> Scope (binders, body'))
    )
    |. Util.sequence_list_result
    |. BR.map (fun subtms' -> Operator (tag, subtms'))
  | Bound _ -> Ok tm
  | Free v -> (match M.get env v with
    | None -> Js.log3 env tm v; Error ("instantiate: couldn't find var " ^ v)
    | Some (Scope (names, _) as sc)
    -> (match open_scope (names |. BL.map (fun name -> Free name)) sc with
      | None -> Error "instantiate: failed to open scope"
      | Some tm -> Ok tm
    )
  )
  | Sequence tms -> tms
    |. BL.map (instantiate env)
    |. Util.sequence_list_result
    |. BR.map (fun tms' -> Sequence tms')
  | Primitive _ -> Ok tm

exception BadMerge

(* TODO: remove? *)
let safe_union m1 m2 : 'a M.t = M.merge m1 m2 (fun _ mv1 mv2 ->
  match mv1, mv2 with
  | Some v, None
  | None, Some v
  -> Some v
  | Some v1, Some v2
  -> if v1 = v2 then Some v1 else (Js.log3 "safe_union" v1 v2; raise BadMerge)
  | _ -> assert false
  )

exception CheckError of string

let update_ctx (ctx_state : scope M.t ref) (learned_tys : scope M.t) =
  let do_assignment = fun (k, v) -> match M.get !ctx_state k with
    | None    -> ctx_state := M.set !ctx_state k v
    | Some v' -> if v <> v' then (Js.log3 "update_ctx" v v'; raise BadMerge)
  in
  List.iter do_assignment (learned_tys |. M.toList)

let get_or_raise msg = function
  | Some x -> x
  | None   -> raise (CheckError msg)

let raise_if_not_ok outer_msg = function
  | BR.Ok x   -> x
  | Error msg -> raise (CheckError (outer_msg ^ ":\n" ^ msg))

let ctx_infer (var_types : term M.t) : term -> term = function
  | Free v -> (match M.get var_types v with
    | None -> raise (CheckError ("ctx_infer: couldn't find variable " ^ v))
    | Some ty -> ty
    )
  | tm -> raise (CheckError "ctx_infer: called with non-free-variable")

let rec check ({ rules } as env : env) (Typing (tm, ty)) : unit
  = let match_rule = function
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
      get_or_raise "check: no matching rule found" (Util.first_by rules match_rule) in
    (* TODO: check term / type assignments disjoint *)
    let schema_assignments = Util.union tm_assignments ty_assignments in
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
    let ctx_state : scope M.t ref = ref schema_assignments in
    let name' = match name with
      | None      -> "infer"
      | Some name -> "infer " ^ name
    in
    List.iter (check_hyp name ctx_state env) hypotheses

and infer ({ rules; var_types } as env : env) (tm : term) : term
  = let match_rule = fun { name; hypotheses; conclusion } -> match conclusion with
    | _, CheckingRule _ -> None
    (* XXX conclusion context *)
    | _, InferenceRule { tm = rule_tm; ty = rule_ty } ->
      match_schema_vars rule_tm tm
      |. BO.map (fun schema_assignments ->
          (name, hypotheses, schema_assignments, rule_ty))
    in
    match Util.first_by rules match_rule with
      | Some (name, hypotheses, schema_assignments, rule_ty)
      -> let ctx_state : scope M.t ref = ref schema_assignments in
         let name' = match name with
           | None      -> "infer"
           | Some name -> "infer " ^ name
         in
         List.iter (check_hyp name ctx_state env) hypotheses;
         instantiate !ctx_state rule_ty |> raise_if_not_ok name'
      | None -> ctx_infer var_types tm

and check_hyp = fun name ctx_state env (pattern_ctx, rule) ->
  let name' = match name with
    | None      -> "check_hyp"
    | Some name -> "check_hyp " ^ name
  in
  match rule with
  | CheckingRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = instantiate !ctx_state hyp_tm |> raise_if_not_ok name' in
    let ty = instantiate !ctx_state hyp_ty |> raise_if_not_ok name' in
    check env (Typing (tm, ty))
  | InferenceRule { tm = hyp_tm; ty = hyp_ty } ->
    let tm = instantiate !ctx_state hyp_tm |> raise_if_not_ok name' in
    let ty = infer env tm in
    let learned_tys = get_or_raise
      "check_hyp: failed to match schema vars"
      (match_schema_vars hyp_ty ty)
    in
    update_ctx ctx_state learned_tys
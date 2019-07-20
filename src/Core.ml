open Belt
open Types
open Binding
let (fold_right, get_first, traverse_list_result) =
  Util.(fold_right, get_first, traverse_list_result)

module M = Belt.Map.String
module O = Belt.Option

type denotation_scope_pat =
  | DenotationScopePat of string list * denotation_pat

and denotation_pat =
  | DPatternTm of string * denotation_scope_pat list
  | DVar       of string option

type core_pat =
  | PatternTerm of string * core_pat list
  | PatternVar  of string option
  | PatternLit  of primitive
  | PatternDefault

type core_val =
  | OperatorVal of string * core_val list
  | PrimVal     of primitive
  | LamVal      of string list * core

and core =
  | CoreVar of string
  (** core variables, introduced by lambda or case bindings *)
  | CoreVal of core_val
  | CoreApp of core * core list
  | Case    of core * sort * (core_pat * core) list
  | Metavar of string
  | Meaning of string

type denotation_chart =
  | DenotationChart of (denotation_pat * core) list

type located_err = string * DeBruijn.term option
type 'a translation_result = ('a, located_err) Result.t

(** Raised by val_to_ast when the presence of metavars makes the value invalid
 *)
exception AstConversionMetavar of string

let rec sort_to_ast (SortAp (name, args)) = Nominal.Operator ("sort",
  [ Scope ([], Primitive (PrimString name));
    Scope ([], Sequence (args |. List.fromArray |. List.map sort_to_ast))
  ])

(* val val_to_ast : core_val -> Nominal.term *)
let rec val_to_ast = function
  | OperatorVal (name, vals)
  -> Nominal.Operator
    ( name
    , List.map vals (fun value -> Nominal.Scope([], val_to_ast value))
    )
  | PrimVal prim
  -> Primitive prim
  | LamVal (args, body)
  (* TODO: is "lam" the right name? *)
  -> Operator ("lam", [ Scope (args, to_ast body) ])

and pat_to_ast : core_pat -> Nominal.term = function
  (* | PatternLit lit -> Nominal.Operator TODO *)
  | PatternDefault -> Nominal.Operator ("PatternDefault", [])

and branch_to_ast (pat, core) : Nominal.term
  = Sequence [ pat_to_ast pat; to_ast core ]

and to_ast (core : core) : Nominal.term = match core with
  | CoreVar name
  -> Nominal.Var name
  | CoreVal core_val
  -> Operator ("CoreVal", [Scope ([], val_to_ast core_val)])
  | CoreApp (f, args)
  -> Operator ("CoreApp",
    [ Scope ([], to_ast f);
      Scope ([], Sequence (args |. List.map to_ast));
    ])
  | Case (tm, ty, branches)
  -> Operator ("Case",
    [ Scope ([], to_ast tm);
      Scope ([], sort_to_ast ty);
      Scope ([], Sequence (branches |. List.map branch_to_ast));
    ])
  | Metavar v | Meaning v -> raise (AstConversionMetavar v)

(* val match_branch : core_val -> core_pat -> core_val M.t option *)
let rec match_branch v pat = match (v, pat) with

  | (OperatorVal (tag1, vals), PatternTerm (tag2, pats)) -> List.(
    let sub_results = zipBy vals pats match_branch in
    if tag1 = tag2 &&
       length vals = length pats && every sub_results O.isSome
    then Some (reduce (map sub_results O.getExn) M.empty union)
    else None
  )
  | (PrimVal l1, PatternLit l2)
  -> if prim_eq l1 l2 then Some M.empty else None
  | (tm, PatternVar (Some v))
  -> Some (M.fromArray [|v,tm|])
  | (_, PatternVar None)
  | (_, PatternDefault)
  -> Some M.empty
  | _
  -> None

(* val find_core_match : core_val -> (core_pat * core) list *)
let rec find_core_match v = function
  | []                 -> None
  | (pat, rhs) :: pats -> (match match_branch v pat with
    | None          -> find_core_match v pats
    | Some bindings -> Some (rhs, bindings))

(* val matches
  : DeBruijn.term
  -> denotation_pat
  -> ((string * string) list * DeBruijn.term M.t) option
*)
let rec matches tm pat = match (tm, pat) with
  | (DeBruijn.Operator(tag1, subtms), DPatternTm(tag2, subpats))
  -> if tag1 == tag2 && List.(length subtms == length subpats)
     then fold_right
       (fun ((scope, subpat), b_opt) ->
         match (matches_scope scope subpat, b_opt) with
         | (Some (assocs, bindings), Some (assocs', bindings'))
         -> Some (assocs @ assocs', union bindings bindings')
         | _ -> None)
       (List.zip subtms subpats)
       (Some ([], M.empty))
     else None
  | (_, DPatternTm _)   -> None
  | (_, DVar None)      -> Some ([], M.empty)
  | (tm, DVar (Some v)) -> Some ([], M.fromArray [|v,tm|])

(* val matches_scope
  : DeBruijn.scope
  -> denotation_scope_pat
  -> ((string * string) list * DeBruijn.term M.t) option
*)
and matches_scope (Scope (binders, tm)) (DenotationScopePat (patBinders, pat))
  = if List.(length patBinders == length binders)
    then O.map
      (matches tm pat)
      (fun (assocs, tmMatches)
        -> (List.zip patBinders binders @ assocs, tmMatches))
    else None

(* val find_match
  : denotation_chart
  -> DeBruijn.term
  -> ((string * string) list * DeBruijn.term M.t * core) option
*)
let find_match (DenotationChart denotations) term = get_first
  (fun (pat, core) -> O.map
    (matches term pat)
    (fun (assocs, bindings) -> (assocs, bindings, core)))
  denotations

(* val fill_in_core
  : denotation_chart
  -> (string * string) list * DeBruijn.term M.t
  -> core
  -> core translation_result
*)
let rec fill_in_core dynamics ((assocs, assignments) as mr) = function
  | Metavar name -> (match M.get assignments name with
    | Some tm -> term_to_core dynamics tm
    | None    -> Result.Error ("TODO 3", None)
    )
  (* XXX same as Metavar? *)
  | Meaning name -> (match M.get assignments name with
    | Some tm -> term_to_core dynamics tm
    | None    -> Result.Error ("TODO 3", None)
    )
  | CoreVar _ as c -> Ok c
  | CoreVal v -> Result.map
    (fill_in_val dynamics mr v)
    (fun v' -> CoreVal v')
  | CoreApp (f, args) -> (match
    ( fill_in_core dynamics mr f
    , mr
      |> fill_in_core dynamics
      |> List.map args
      |> sequence_list_result
    ) with
    | (Ok f', Ok args')               -> Ok (CoreApp (f', args'))
    | (Error msg, _) | (_, Error msg) -> Error msg
    )
  (* | Lam (binders, core) -> Result.map
    (fill_in_core dynamics mr core)
    (fun core' -> Lam (binders, core')) *)
  | Case (scrutinee, ty, branches) ->
      let mBranches : ((core_pat * core) list) translation_result
            = sequence_list_result (List.map branches
            (fun (pat, core) -> Result.map
              (fill_in_core dynamics mr core)
              (fun core' -> (pat, core'))
            )) in
      match (fill_in_core dynamics mr scrutinee, mBranches) with
        | (Ok scrutinee', Ok branches')
        -> Ok (Case (scrutinee', ty, branches'))
        | (Error msg, _) | (_, Error msg)
        -> Error msg

(* val fill_in_val
  : denotation_chart
  -> (string * string) list * DeBruijn.term M.t
  -> core_val
  -> core_val translation_result
*)
and fill_in_val dynamics mr = function
  | OperatorVal (tag, vals) -> Result.map
    (traverse_list_result (fill_in_val dynamics mr) vals)
    (fun vals' -> OperatorVal (tag, vals'))
  | PrimVal _  as v -> Ok v
  | LamVal (binders, core) -> Result.map
    (fill_in_core dynamics mr core)
    (fun core' -> LamVal (binders, core'))

(* val term_is_core_val : string list -> DeBruijn.term -> core_val translation_result *)
and term_is_core_val env = function
  | DeBruijn.Operator ("lam", [Scope (names, body)])
  -> let env' = List.concat names env in
     Result.map
       (term_is_core env' body)
       (fun body' -> LamVal (names, body'))
  | Operator (tag, subtms) -> Result.map
    (traverse_list_result (scope_is_core_val env) subtms)
    (fun subtms' -> OperatorVal (tag, subtms'))
  | Primitive prim -> Ok (PrimVal prim)
  | Var _ as tm -> Error ("TODO 4", Some tm)
  | tm          -> Error ("TODO 5", Some tm)

(* val scope_is_core_val : string list -> DeBruijn.scope -> core_val translation_result *)
and scope_is_core_val env (Scope (names, body)) = match names with
  | [] -> term_is_core_val env body
  | _  -> Error ("Unexpected binding TODO", None)

(* val term_is_core : string list -> DeBruijn.term -> core translation_result *)
and term_is_core env tm = match tm with
  | DeBruijn.Var ix -> (match List.get env ix with
    | None -> Error ("failed to look up variable", Some tm)
    | Some name -> Ok (CoreVar name)
    )
  (* TODO *)

(* val term_to_core : denotation_chart -> DeBruijn.term -> core translation_result *)
and term_to_core dynamics tm = match find_match dynamics tm with
  | None
  -> Error ("no match found", Some tm)
  | Some (assocs, bindings, protoCore)
  -> fill_in_core dynamics (assocs, bindings) protoCore

(* val eval : core -> (core_val, string) Result.t *)
let eval core =
  let open Belt.Result in

  let rec go ctx = function
        | CoreVar v -> (match M.get ctx v with
          | Some result -> Ok result
          | None        -> Error ("Unbound variable " ^ v)
        )
        | CoreVal v -> Ok v
        | CoreApp (CoreVal (LamVal (argNames, body)), args) ->
            if List.(length argNames != length args)
            then Error "mismatched application lengths"
            else Result.flatMap
              (sequence_list_result (List.map args (go ctx)))
              (fun arg_vals ->
                 let new_args = List.(arg_vals
                   |> zip argNames
                   |> toArray
                   |> M.fromArray
                 ) in
                 go (union ctx new_args) body
              )
        | Case (tm, _ty, branches) -> Result.flatMap (go ctx tm)
          (fun v -> (match find_core_match v branches with
            | None                    -> Error "no match found in case"
            | Some (branch, bindings) -> go (union ctx bindings) branch)
          )
        | Metavar _v | Meaning _v -> Error "Found a metavar!"

        | _ -> Error "TODO 7"

  in go M.empty core

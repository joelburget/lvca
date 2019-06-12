open Belt
open Util

type sort =
  | SortAp   of sort * sort
  | SortName of string

type valence =
  | FixedValence    of sort list * sort
  | VariableValence of string * sort
  (** A variable valence represents *)

type arity =
  | Arity of string list * valence list
  (** An arity is defined its arity indices and valences *)

type operatorDef =
  | OperatorDef of string * arity
  (** An operator is defined by its tag and arity *)

type sortDef =
  | SortDef of string list * operatorDef list
  (** A sort is defined by a set of variables and a set of operators *)

type language =
  | Language of sortDef Belt.Map.String.t

type primitive =
  | PrimInteger of Bigint.t
  | PrimString  of string
  | PrimBool    of bool

let prim_eq p1 p2 = match (p1, p2) with
  | (PrimInteger i1, PrimInteger i2) -> Bigint.(i1 = i2)
  | (PrimString  s1, PrimString  s2) -> s1 = s2
  | (PrimBool    b1, PrimBool    b2) -> b1 = b2
  | _                                -> false

module rec Abt : sig
  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of int
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive

  val from_ast
    : language
    -> string
    -> Ast.term
    -> (term, string) Result.t
  val from_ast_with_bindings
    :  language
    -> string
    -> int Belt.Map.String.t
    -> Ast.term
    -> (term, string) Result.t
  val scope_from_ast
    :  language
    -> string
    -> int Belt.Map.String.t
    -> Ast.scope
    -> (scope, string) Result.t
end = struct

  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of int
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive

  let rec find_operator (operators : operatorDef list) (tag : string)
    : operatorDef option
    = match operators with
      | [] -> None
      | (OperatorDef (hd, _) as od) :: tl ->
        if hd = tag
        then Some od
        else find_operator tl tag

  module M = Belt.Map.String
  let rec from_ast_with_bindings
        (Language sorts as lang : language)
        (current_sort : string)
        (env : int M.t)
    : Ast.term -> (term, string) Result.t
    = function
      | Ast.Term(tag, subtms) -> (match M.get sorts current_sort with
        | None -> Error
          ("from_ast_with_bindings: couldn't find sort " ^ current_sort)
        | Some (SortDef (_vars, operators)) -> (match find_operator operators tag with
          | None -> Error
            ("from_ast_with_bindings: couldn't find operator " ^ tag ^
            " (in sort " ^ current_sort ^ ")")
          | Some (OperatorDef (_tag, Arity (_binds, valences))) ->
            if List.(length valences != length subtms)
            then Error "TODO"
            else Result.map
                   (traverse_list_result
                     (List.zipBy valences subtms
                     (fun valence subtm -> match valence with
                       | FixedValence (_binds, SortName result_sort)
                         -> scope_from_ast lang result_sort env subtm
                       | _ -> Result.Error "TODO")))
                   (fun subtms' -> Term (tag, subtms'))))
      | Ast.Var name -> (match M.get env name with
        | None    -> Error ("couldn't find variable " ^ name)
        | Some ix -> Ok (Var ix))
      | Ast.Sequence tms -> Result.map
        (traverse_list_result
          (List.map tms (from_ast_with_bindings lang current_sort env)))
        (fun x' -> Sequence x')
      | Primitive prim -> Ok (Abt.Primitive prim)

  and scope_from_ast lang (current_sort : string) env (Ast.Scope (names, body))
    = Result.map
      (from_ast_with_bindings lang current_sort env body)
      (fun body' -> (Scope (names, body')))

  let from_ast lang current_sort = from_ast_with_bindings lang current_sort M.empty

end

and Ast : sig
  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of string
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive

end = struct
  type scope =
    | Scope of string list * term

  and term =
    | Term      of string * scope list
    | Var       of string
    | Sequence  of term list (* TODO: list vs array? *)
    | Primitive of primitive
end

module Core = struct

  type scope_pat =
    | DenotationScopePat of string list * denotation_pat

  and denotation_pat =
    | DPatternTm of string * scope_pat list
    | DVar       of string option

  type ty = | Ty (* TODO *)

  type core_pat =
    | PatternTerm of string * core_pat list
    | PatternVar  of string option
    | PatternLit  of primitive
    | PatternDefault

  type core_val =
    | ValTm  of string * core_val list
    | ValLit of primitive
    | ValPrimop of string
    | ValLam of string list * core

  and core =
    | CoreVar of string
    | CoreVal of core_val
    | CoreApp of core * core list
    | Lam     of string list * core
    | Case    of core * ty * (core_pat * core) list
    | Meaning of string

  type denotation_chart =
    | DenotationChart of (denotation_pat * core) list

  module M = Belt.Map.String
  module O = Belt.Option

  let rec match_branch (v : core_val) (pat : core_pat)
    : core_val M.t option = match (v, pat) with

    | (ValTm (tag1, vals), PatternTerm (tag2, pats)) ->
        let sub_results = List.map
          (List.zip vals pats)
          (fun (v', pat') -> match_branch v' pat') in
        if tag1 = tag2 &&
           List.length vals = List.length pats &&
           List.every sub_results O.isSome
        then Some (List.reduce (List.map sub_results O.getExn) M.empty union)
        else None
    | (ValLit l1, PatternLit l2)
    -> if prim_eq l1 l2 then Some M.empty else None
    | (tm, PatternVar (Some v)) -> Some (M.fromArray [|v,tm|])
    | (tm, PatternVar None)     -> Some M.empty
    | (_val, PatternDefault)    -> Some M.empty
    | _ -> None

  let rec matches (tm : Abt.term) (pat : denotation_pat)
    : ((string * string) list * Abt.term M.t) option
    = match (tm, pat) with
      | (Term(tag1, subtms), DPatternTm(tag2, subpats))
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

  and matches_scope
    (Scope (binders, tm) : Abt.scope)
    (DenotationScopePat (patBinders, pat) : scope_pat)
    : ((string * string) list * Abt.term M.t) option
    = if List.(length patBinders == length binders)
      then O.map
        (matches tm pat)
        (fun (assocs, tmMatches)
          -> (List.zip patBinders binders @ assocs, tmMatches))
      else None

  let find_match
    (DenotationChart denotations : denotation_chart)
    (term : Abt.term)
    : ((string * string) list * Abt.term M.t * core) option
    = get_first
        (fun (pat, core) -> O.map
          (matches term pat)
          (fun (assocs, bindings) -> (assocs, bindings, core)))
        denotations

  type located_err = (string * Abt.term option)
  type 'a translation_result = ('a, located_err) Result.t

  let rec fill_in_core
    (dynamics : denotation_chart)
    ((assocs, assignments) as mr : (string * string) list * Abt.term M.t)
    (c : core)
    : core translation_result
    = match c with
      | Meaning name -> (match M.get assignments name with
        | Some tm -> term_to_core dynamics tm
        | None    -> Error ("TODO 4", None)
        )
      (* XXX same as Meaning *)
      | CoreVar name -> (match M.get assignments name with
        | Some tm -> Result.map (term_is_core_val tm) (fun cv -> CoreVal cv)
        | None    -> Ok c
        )
      | CoreVal v -> Result.map
        (fill_in_val dynamics mr v)
        (fun v' -> CoreVal v')
      | CoreApp(f, args) -> (match
        ( fill_in_core dynamics mr f
        , traverse_list_result (List.map args (fill_in_core dynamics mr))
        ) with
        | (Ok f', Ok args')               -> Ok (CoreApp (f', args'))
        | (Error msg, _) | (_, Error msg) -> Error msg
        )
      | Lam (binders, core) -> Result.map
        (fill_in_core dynamics mr core)
        (fun core' -> Lam (binders, core'))
      | Case (scrutinee, ty, branches) ->
          let x : ((core_pat * core) list) translation_result
                = traverse_list_result (List.map branches
                (fun (pat, core) -> Result.map
                  (fill_in_core dynamics mr core)
                  (fun core' -> (pat, core'))
                )) in
          match (fill_in_core dynamics mr scrutinee, x) with
            | (Ok scrutinee', Ok branches')
            -> Ok (Case (scrutinee', ty, branches'))
            | (Error msg, _) | (_, Error msg)
            -> Error msg

  and fill_in_val
      (dynamics : denotation_chart)
      (mr : (string * string) list * Abt.term M.t)
      (v : core_val)
    : core_val translation_result
    = match v with
      | ValTm (tag, vals) -> Result.map
        (traverse_list_result (List.map vals (fill_in_val dynamics mr)))
        (fun vals' -> ValTm (tag, vals'))
      | ValLit _    -> Ok v
      | ValPrimop _ -> Ok v
      | ValLam (binders, core) -> Result.map
        (fill_in_core dynamics mr core)
        (fun core' -> ValLam (binders, core'))

  and term_is_core_val (tm : Abt.term) : core_val translation_result
    = match tm with
    | Term (tag, subtms) -> Result.map
      (traverse_list_result (List.map subtms scope_is_core_val))
      (fun subtms' -> ValTm (tag, subtms'))
    | Abt.Primitive prim -> Ok (ValLit prim)
    | Abt.Var _ -> Error ("TODO", Some tm)
    | _ -> Error ("TODO", Some tm)

  and scope_is_core_val (scope : Abt.scope) : core_val translation_result
    = failwith "TODO"

  and term_to_core (dynamics : denotation_chart) (tm : Abt.term)
    : core translation_result
    = match find_match dynamics tm with
      | None
      -> Error ("no match found", Some tm)
      | Some (assocs, bindings, protoCore)
      -> fill_in_core dynamics (assocs, bindings) protoCore

  let eval (core : core) : (core_val, string) Result.t =

    let open Belt.Result in

    let rec go ctx core = match core with
          | CoreVar v -> (match M.get ctx v with
            | Some result -> Ok result
            | None        -> Error ("Unbound variable " ^ v))
          | CoreVal v -> Ok v
          | CoreApp (Lam (argNames, body), args) ->
              if List.(length argNames != length args)
              then Error "mismatched application lengths"
              else let args' = [] (* List.map (go ctx) args *) in
                   let newArgs = M.fromArray
                     (List.toArray (List.zip argNames args')) in
                   go (union ctx newArgs) body
          (* | Case tm _ty branches ->
            let v = go ctx tm *)
          | Meaning _v -> Error "Found a metavar!"

          | _ -> Error "TODO 5"

    in go M.empty core

end

module Statics = struct

  module M = Belt.Map.String

  type scope = Scope of string list * term

  and term =
          | Term      of string * scope list
          | Bound     of int
          | Free      of string
          | Sequence  of term list
          | Primitive of primitive

  type inferenceRule = InferenceRule of term * term
  type checkingRule  = CheckingRule  of term * term

  type typingClause =
    | InferenceRule of inferenceRule
    | CheckingRule  of checkingRule

  type hypothesis = term M.t * typingClause

  (* TODO: the conclusion type differs from LVCA *)
  type rule = Rule of hypothesis list * string option * hypothesis

end

open Types
open Binding

let fold_right, get_first, traverse_list_result, union =
  Util.(fold_right, get_first, traverse_list_result, union)
;;

(** Represents the LHS of a denotation rule *)
type denotation_pat =
  | Operator of string * denotation_pat_scope list
  | Sequence of denotation_pat list
  | Primitive of primitive
  | Var of string

and denotation_pat_scope = Scope of string list * denotation_pat

(** Represents the RHS of a denotation rule *)
type denotation_term =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * denotation_scope list
  | Var of string
  | Sequence of denotation_term list
  | Primitive of primitive
  (* Also, oxford bracketed var *)
  | Meaning of string

and denotation_scope = Scope of denotation_scope_pat list * denotation_term

and denotation_scope_pat =
  | PatOperator of string * denotation_scope_pat list
  | PatVar of string
  | Sequence of denotation_scope_pat list
  | Primitive of primitive

type core =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * core_scope list
  | Var of string
  | Sequence of core list
  | Primitive of primitive
  (* plus, core-specific ctors *)
  | Lambda of sort list * core_scope
  | CoreApp of core * core list
  | Case of core * core_scope list
  (** A metavariable refers to a term captured directly from the left-hand-side
  *)
  | Metavar of string
  (** Meaning is very similar to a metavar in that it refers to a capture from
      the left-hand-side. However, a meaning variable is interpreted. *)
  | Meaning of string

and core_scope = CoreScope of Pattern.t list * core

let rec term_to_core' vars : denotation_term -> core = function
  | Var s -> if Belt.Set.String.has vars s then Var s else Metavar s
  | Sequence tms -> Sequence (Belt.List.map tms @@ term_to_core' vars)
  | Primitive p -> Primitive p
  | Meaning v -> Meaning v
  | _ -> failwith "TODO"
;;

(*
   | Operator ("annot", [t1; t2]) ->
   | Operator ("fun", ...)
   | Operator ("app", ...)
   | Operator ("case", ...)
*)

let term_to_core : denotation_term -> core = term_to_core' Belt.Set.String.empty

let rec sort_from_ast (term : Nominal.term) : sort =
  match term with
  | Operator
      ( "sort_ap"
      , [ Scope ([], Primitive (PrimString name)); Scope ([], Sequence subtms) ] ) ->
    SortAp (name, subtms |. Belt.List.toArray |. Belt.Array.map sort_from_ast)
  | _ -> failwith "TODO: throw"
;;

let rec from_ast' (vars : Belt.Set.String.t) (term : Nominal.term) : core =
  match term with
  (* Sequence, and Primitive are translated directly: *)
  | Sequence tms -> Sequence (tms |. Belt.List.map (from_ast' vars))
  | Primitive p -> Primitive p
  (* A variable is just a variable if we've encountered a binder for it.
   * Otherwise it's a metavar. *)
  | Var name -> if Belt.Set.String.has vars name then Var name else Metavar name
  (* The other operators are more involved: *)
  | Operator ("lam", [ Scope ([], Sequence tys); body ]) ->
    Lambda (tys |. Belt.List.map sort_from_ast, from_ast_scope vars body)
  | Operator ("app", Scope ([], f) :: args) ->
    CoreApp (from_ast' vars f, args |. Belt.List.map (from_ast_no_scope vars))
  | Operator ("case", Scope ([], tm) :: branches)
    (* XXX how do variable arity terms appear? *) ->
    Case (from_ast' vars tm, branches |. Belt.List.map (failwith "TODO"))
  | Operator ("[[]]", [ Scope ([], Var name) ]) -> Meaning name
  | _ -> failwith "TODO: throw"

(* and branch_from_ast ( *)
and from_ast_no_scope vars (Scope (pats, tm) : Nominal.scope) : core =
  match pats with
  | [] -> from_ast' vars tm
  | _ -> failwith "TODO: throw"

and from_ast_scope vars (Scope (pats, tm) : Nominal.scope) : core_scope =
  CoreScope
    ( pats
      , (* from_ast' (Belt.Set.String.union vars (Belt.Set.String.fromArray (Belt.List.toArray pats))) tm *)
      from_ast' (failwith "TODO") tm )
;;

let from_ast : Nominal.term -> core = from_ast' Belt.Set.String.empty

(** An association of variable names between pattern and term. IE the named
 * pattern variable and term variable are the same. *)
type assoc =
  { pattern_name : string
  ; term_name : string
  }

type pre_denotation_chart = DenotationChart of (denotation_pat * denotation_term) list
type denotation_chart = DenotationChart of (denotation_pat * core) list

let produce_denotation_chart : pre_denotation_chart -> denotation_chart =
  fun (DenotationChart lines) ->
  DenotationChart (lines |. Belt.List.map (fun (pat, tm) -> pat, term_to_core tm))
;;

type located_err = string * DeBruijn.term option
type 'a translation_result = ('a, located_err) Belt.Result.t

(** Raised by to_ast when the presence of metavars, lambdas, and cases make
    the value invalid
*)
exception ToAstConversionErr of core

let rec to_ast (core : core) : Nominal.term =
  match core with
  | Var name -> Var name
  | Operator (tag, vals) -> Operator (tag, Belt.List.map vals scope_to_ast)
  | Primitive prim -> Primitive prim
  | Sequence tms -> Sequence (Belt.List.map tms to_ast)
  | Lambda _ as v -> raise @@ ToAstConversionErr v
  | CoreApp _ | Case _ | Metavar _ | Meaning _ -> raise @@ ToAstConversionErr core

and scope_to_ast (CoreScope (pats, body)) = Nominal.Scope (pats, to_ast body)

(* val match_branch : core -> Pattern.t -> core Belt.Map.String.t option *)
let rec match_branch v pat =
  match v, pat with
  | Operator (tag1, vals), Pattern.Operator (tag2, pats) ->
    Belt.List.(
      (* let sub_results = zipBy vals pats match_binding_branch in *)
      let sub_results = failwith "TODO" in
      if tag1 = tag2 && length vals = length pats && every sub_results Belt.Option.isSome
      then Some (reduce (map sub_results Belt.Option.getExn) Belt.Map.String.empty union)
      else None)
  | Sequence s1, Sequence s2 ->
    Belt.List.(
      let sub_results = zipBy s1 s2 match_branch in
      if length s1 = length s2 && every sub_results Belt.Option.isSome
      then Some (reduce (map sub_results Belt.Option.getExn) Belt.Map.String.empty union)
      else None)
  | Primitive l1, Primitive l2 -> if prim_eq l1 l2 then Some Belt.Map.String.empty else None
  | _, Var "_" -> Some Belt.Map.String.empty
  | tm, Var v -> Some (Belt.Map.String.fromArray [| v, tm |])
  | _ -> None
;;

let rec find_core_match v : core_scope list -> (core * core Belt.Map.String.t) option = function
  | [] -> None
  | CoreScope ([ pat ], rhs) :: scopes ->
    (match match_branch v pat with
     | None -> find_core_match v scopes
     | Some bindings -> Some (rhs, bindings))
;;

(* val matches
   : DeBruijn.term
   -> denotation_pat
   -> (assoc list * DeBruijn.term Belt.Map.String.t) option
*)
let rec matches tm (pat : denotation_pat) =
  match tm, pat with
  | DeBruijn.Operator (tag1, subtms), Operator (tag2, subpats) ->
    if tag1 == tag2 && Belt.List.(length subtms == length subpats)
    then
      fold_right
        (fun ((scope, subpat), b_opt) ->
           match matches_scope scope subpat, b_opt with
           | Some (assocs, bindings), Some (assocs', bindings') ->
             Some (assocs @ assocs', union bindings bindings')
           | _ -> None)
        (Belt.List.zip subtms subpats)
        (Some ([], Belt.Map.String.empty))
    else None
  | _, Operator _ -> None
  | _, Var "_" -> Some ([], Belt.Map.String.empty)
  | tm, Var v -> Some ([], Belt.Map.String.fromArray [| v, tm |])

(* val matches_scope
   : DeBruijn.scope
   -> denotation_scope_pat
   -> (assoc list * DeBruijn.term Belt.Map.String.t) option
*)
and matches_scope (Scope (binders, tm)) (Scope (patBinders, pat)) =
  if Belt.List.(length patBinders == length binders)
  then
    Belt.Option.map (matches tm pat) (fun (assocs, tmMatches) ->
      ( Belt.List.zipBy patBinders binders (failwith "TODO")
        (* (fun pattern_name term_name -> {pattern_name; term_name}) *)
        @ assocs
      , tmMatches ))
  else None
;;

(* val find_match
   : denotation_chart
   -> DeBruijn.term
   -> (assoc list * DeBruijn.term Belt.Map.String.t * core) option
*)
let find_match (DenotationChart denotations) term =
  get_first
    (fun (pat, core) ->
       Belt.Option.map (matches term pat) (fun (assocs, bindings) -> assocs, bindings, core))
    denotations
;;

exception TranslationError of located_err

type fill_in_args =
  { dynamics : denotation_chart
  ; vars : string list
  ; assocs : assoc list
  ; assignments : DeBruijn.term Belt.Map.String.t
  }

let associate_name (assocs : assoc list) (pat_name : string) =
  match Util.find (fun { pattern_name } -> pattern_name = pat_name) assocs with
  | None -> raise (TranslationError ("Pattern name " ^ pat_name ^ " not found", None))
  | Some { term_name } -> term_name
;;

(* val fill_in_core
   : denotation_chart
   -> assoc list * DeBruijn.term Belt.Map.String.t
   -> core
   -> core translation_result
*)

(** Fill in a core representation of a value with metavars (the raw
    right-hand-side of a denotation chart) with the core terms that go there.
*)
let rec fill_in_core ({ dynamics; vars; assignments } as args) = function
  | Metavar name ->
    (match Belt.Map.String.get assignments name with
     | Some tm -> term_to_core [] tm
     | None -> raise (TranslationError ("Metavariable " ^ name ^ " not found", None)))
  | Meaning name ->
    (match Belt.Map.String.get assignments name with
     | Some tm ->
       (match term_denotation dynamics vars tm with
        | Belt.Result.Ok tm' -> tm'
        | Error err -> raise (TranslationError err))
     | None -> raise (TranslationError ("Metavariable " ^ name ^ " not found", None)))
  | Var _ as v -> v
  | Operator (tag, vals) -> Operator (tag, vals |. Belt.List.map (fill_in_core_scope args))
  | Primitive _ as v -> v
  | Sequence tms -> Sequence (tms |. Belt.List.map (fill_in_core args))
  | Lambda (tys, body) -> Lambda (tys, fill_in_core_scope args body)
  | CoreApp (f, app_args) ->
    CoreApp (fill_in_core args f, app_args |. Belt.List.map (fill_in_core args))
  | Case (scrutinee, branches) ->
    Case
      ( fill_in_core args scrutinee
      , failwith "TODO"
        (*
           , branches |. Belt.List.map (fun (pat, scope) ->
           (pat, fill_in_core_scope args scope))
        *)
      )

and fill_in_core_scope ({ assocs; vars } as args)
      (CoreScope (names, body)) =
  let names' = names |. Belt.List.map (failwith "TODO") (* associate_name assocs *) in
  CoreScope (names', fill_in_core { args with vars = names' @ vars } body)

(** Translate a term directly to core, with no interpretation. In other words,
    this term is supposed to directly represent a term in the codomain. *)
and term_to_core env tm =
  match tm with
  | Operator (tag, subtms) -> Operator (tag, subtms |. Belt.List.map (scope_to_core env))
  | Var (i, j) -> failwith "TODO"
  (*match Belt.List.get env ix with
    | None -> raise (TranslationError ("failed to look up variable", Some tm))
    | Some name -> Var name
  *)
  | Sequence tms -> Sequence (tms |. Belt.List.map (term_to_core env))
  | Primitive prim -> Primitive prim

(* XXX change names (assocs)? *)
and scope_to_core env (Scope (names, body)) =
  CoreScope (names, term_to_core env body)
(* val term_denotation
   : denotation_chart -> DeBruijn.term -> core translation_result
*)

(** Match a term in the denotation chart, and return its core term. *)
and term_denotation dynamics vars tm : core translation_result =
  match tm with
  | DeBruijn.Var (i, j) -> failwith "TODO"
  (*match Belt.List.get vars i with
    | Some var_name -> Ok (Var var_name)
    | None -> Error ("couldn't find variable " ^ string_of_int i ^
    " in variable context of size " ^ string_of_int (Belt.List.length vars),
    Some tm)
  *)
  | _ ->
    (match find_match dynamics tm with
     | None -> Error ("no match found", Some tm)
     | Some (assocs, assignments, protoCore) ->
       (try Ok (fill_in_core { dynamics; vars; assocs; assignments } protoCore) with
        | TranslationError err -> Error err))
;;

(* val eval : core -> (core, string) Belt.Result.t *)
let eval core =
  let rec go ctx tm =
    match tm with
    | Var v ->
      (match Belt.Map.String.get ctx v with
       | Some result -> Belt.Result.Ok result
       | None -> Error ("Unbound variable " ^ v))
    | CoreApp (Lambda (_tys, CoreScope (argNames, body)), args) ->
      if Belt.List.(length argNames != length args)
      then Error "mismatched application lengths"
      else
        Belt.Result.flatMap
          (traverse_list_result (go ctx) args)
          (fun arg_vals ->
             let new_args = failwith "TODO" in
            (*
                let new_args = Belt.List.(arg_vals
                |> zip argNames
                |> toArray
                |> Belt.Map.String.fromArray
                ) in
             *)
             go (union ctx new_args) body)
    | Case (tm, branches) ->
      Belt.Result.flatMap (go ctx tm) (fun v ->
        match find_core_match v branches with
        | None -> Error "no match found in case"
        | Some (branch, bindings) -> go (union ctx bindings) branch)
    | Metavar _v | Meaning _v -> Error "Found a metavar!"
    (* TODO: or should this be an app? *)
    | Operator ("#add", [ CoreScope ([], a); CoreScope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Ok (Primitive (PrimInteger a')), Ok (Primitive (PrimInteger b')) ->
         Ok (Primitive (PrimInteger (Bigint.add a' b')))
       | Error err, _ | _, Error err -> Error err)
    | Operator ("#sub", [ CoreScope ([], a); CoreScope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Ok (Primitive (PrimInteger a')), Ok (Primitive (PrimInteger b')) ->
         Ok (Primitive (PrimInteger (Bigint.sub a' b')))
       | Error err, _ | _, Error err -> Error err)
    | Operator _ | Sequence _ | Primitive _ -> Ok tm
    (* TODO: include the term in error *)
    | _ -> Error "Found a term we can't evaluate"
  in
  go Belt.Map.String.empty core
;;

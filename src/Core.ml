open Types
open Binding

let (vars_of_pattern, vars_of_patterns) = Pattern.(vars_of_pattern, vars_of_patterns)
let (empty_set, set_union) = Belt.Set.String.(empty, union)
let (empty_map, fromArray) = Belt.Map.String.(empty, fromArray)
let (every, reduce, length, map, zip, zipBy) =
  Belt.List.(every, reduce, length, map, zip, zipBy)
let fold_right, get_first, traverse_list_result, map_union =
  Util.(fold_right, get_first, traverse_list_result, map_union)
;;

(** This type represents the RHS of a denotation rule as parsed. We use
 * denotation_term_to_core to translate it to a core term which can actually be
 * executed.
 * (See produce_denotation_chart : pre_denotation_chart -> denotation_chart).
 * *)
type denotation_term =
  (* first four constructors correspond to regular term constructors *)
  | Operator of string * denotation_scope list
  | Var of string
  | Sequence of denotation_term list
  | Primitive of primitive
  (* Also, oxford bracketed var *)
  | Meaning of string

and denotation_scope = Scope of Pattern.t list * denotation_term

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
  | Let of core * core_scope
  (** A metavariable refers to a term captured directly from the left-hand-side
  *)
  | Metavar of string
  (** Meaning is very similar to a metavar in that it refers to a capture from
      the left-hand-side. However, a meaning variable is interpreted. *)
  | Meaning of string

and core_scope = Scope of Pattern.t list * core

(* vars is the set of all variables that have been bound by object-language
 * scopes. *)
let rec denotation_term_to_core' vars : denotation_term -> core = function
  | Var s -> if Belt.Set.String.has vars s then Var s else Metavar s
  | Sequence tms -> Sequence (map tms @@ denotation_term_to_core' vars)
  | Primitive p -> Primitive p
  | Meaning v -> Meaning v
  | Operator ("lambda", [Scope (pats, body)])
  ->
    let vars' = set_union vars (vars_of_patterns pats)
    in Lambda ([(* XXX *)], Scope (pats, denotation_term_to_core' vars' body))
  | Operator ("app", [Scope (scope_pats, t); Scope ([], Sequence ts)])
  -> let vars' = set_union vars (vars_of_patterns scope_pats)
     in CoreApp
    (denotation_term_to_core' vars' t, (* XXX scope_pats *)
     map ts @@ denotation_term_to_core' vars)
  | Operator ("case", Scope ([], tm) :: branches) ->
    Case (denotation_term_to_core' vars tm,
      map branches (denotation_scope_to_core vars))
  | Operator ("let", [Scope ([], tm); Scope ([pat], body)]) ->
      Let (denotation_term_to_core' vars tm,
        Scope ([pat], denotation_term_to_core'
          (set_union vars @@ vars_of_pattern pat) body)
        )
  | _ -> failwith "error: unexpected term in denotation_term_to_core'"

and denotation_scope_to_core vars = fun (Scope (pats, body)) ->
  Scope (pats, denotation_term_to_core' vars body)
;;

let denotation_term_to_core : denotation_term -> core
  = denotation_term_to_core' empty_set

(*
let rec sort_from_ast (term : Nominal.term) : sort =
  match term with
  | Operator
      ( "sort_ap"
      , [ Scope ([], Primitive (PrimString name)); Scope ([], Sequence subtms) ] ) ->
    SortAp (name, subtms |. Belt.List.toArray |. map sort_from_ast)
  | _ -> failwith "TODO: throw"
;;

let rec from_ast' (vars : Belt.Set.String.t) (term : Nominal.term) : core =
  match term with
  (* Sequence, and Primitive are translated directly: *)
  | Sequence tms -> Sequence (tms |. map (from_ast' vars))
  | Primitive p -> Primitive p
  (* A variable is just a variable if we've encountered a binder for it.
   * Otherwise it's a metavar. *)
  | Var name -> if Belt.Set.String.has vars name then Var name else Metavar name
  (* The other operators are more involved: *)
  | Operator ("lam", [ Scope ([], Sequence tys); body ]) ->
    Lambda (tys |. map sort_from_ast, from_ast_scope vars body)
  | Operator ("app", Scope ([], f) :: args) ->
    CoreApp (from_ast' vars f, args |. map (from_ast_no_scope vars))
  | Operator ("case", Scope ([], tm) :: branches)
    (* XXX how do variable arity terms appear? *) ->
    Case (from_ast' vars tm, branches |. map (failwith "TODO"))
  | Operator ("[[]]", [ Scope ([], Var name) ]) -> Meaning name
  | _ -> failwith "TODO: throw"

(* and branch_from_ast ( *)
and from_ast_no_scope vars (Scope (pats, tm) : Nominal.scope) : core =
  match pats with
  | [] -> from_ast' vars tm
  | _ -> failwith "TODO: throw"

and from_ast_scope vars (Scope (pats, tm) : Nominal.scope) : core_scope =
  Scope
    ( pats
      , (* from_ast' (set_union vars (fromArray (Belt.List.toArray pats))) tm *)
      from_ast' (failwith "TODO") tm )
;;

let from_ast : Nominal.term -> core = from_ast' empty_set
*)

(** An association from a scope variable (in the LHS of a denotation) to the
 * scope binder in the matched term
 *)
type assoc =
  { pattern_name : string
  ; term_pattern : Pattern.t
  }

type pre_denotation_chart =
  DenotationChart of (BindingAwarePattern.t * denotation_term) list
type denotation_chart =
  DenotationChart of (BindingAwarePattern.t * core) list

let produce_denotation_chart : pre_denotation_chart -> denotation_chart =
  fun (DenotationChart lines) ->
  DenotationChart (lines |. map (fun (pat, tm) -> pat, denotation_term_to_core tm))
;;

type located_err = string * DeBruijn.term option
type 'a translation_result = ('a, located_err) Belt.Result.t

(** Raised by to_ast when the presence of metavars, lambdas, and cases make
    the value invalid
*)
exception ToAstConversionErr of core

let rec to_ast : core -> Nominal.term = fun core -> match core with
  | Var name -> Var name
  | Operator (tag, vals) -> Operator (tag, map vals scope_to_ast)
  | Primitive prim -> Primitive prim
  | Sequence tms -> Sequence (map tms to_ast)
  | Lambda _ | Let _ | CoreApp _ | Case _ | Metavar _ | Meaning _
  -> raise @@ ToAstConversionErr core

and scope_to_ast (Scope (pats, body)) = Nominal.Scope (pats, to_ast body)

let rec match_branch
  : core -> Pattern.t -> core Belt.Map.String.t option
  = fun v pat ->
  let (isSome, getExn) = Belt.Option.(isSome, getExn) in
  match v, pat with
  | Operator (tag1, vals), Operator (tag2, pats) ->
    let sub_results = zipBy vals pats (fun (x : core_scope) (y : Pattern.t) ->
      Some empty_map (* XXX *))
    in
    if tag1 = tag2 && length vals = length pats && every sub_results isSome
    then Some (reduce (map sub_results getExn) empty_map map_union)
    else None
  | Sequence s1, Sequence s2 ->
    let sub_results = zipBy s1 s2 match_branch in
    if length s1 = length s2 && every sub_results isSome
    then Some (reduce (map sub_results getExn) empty_map map_union)
    else None
  | Primitive l1, Primitive l2 -> if prim_eq l1 l2 then Some empty_map else None
  | _, Var "_" -> Some empty_map
  | tm, Var v -> Some (fromArray [| v, tm |])
  | _ -> None
;;

let rec find_core_match
  : core -> core_scope list -> (core * core Belt.Map.String.t) option
  = fun v -> function
  | [] -> None
  | Scope ([ pat ], rhs) :: scopes ->
    (match match_branch v pat with
     | None -> find_core_match v scopes
     | Some bindings -> Some (rhs, bindings))
  | _ -> failwith "invariant violation: match binding more than one pattern"
;;

(* Helper to associate a sequence of subterms (or scopes) / patterns *)
let associate_all associate_one tm_seq pat_seq =
  if length tm_seq = length pat_seq
  then fold_right
    (fun ((tm, pat), b_opt) ->
      match associate_one tm pat, b_opt with
        | Some (assocs, bindings), Some (assocs', bindings') ->
          Some (assocs @ assocs', map_union bindings bindings')
        | _ -> None
    )
    (zip tm_seq pat_seq)
    (Some ([], empty_map))
  else None
;;

let rec matches
  : DeBruijn.term
  -> BindingAwarePattern.t
  -> (assoc list * DeBruijn.term Belt.Map.String.t) option
  = fun tm pat -> match tm, pat with
  | Operator (tag1, subtms), Operator (tag2, subpats)
  -> if tag1 == tag2 then associate_all matches_scope subtms subpats else None
  | Primitive p1, Primitive p2
  -> if p1 = p2 then Some ([], empty_map) else None
  | Sequence tm_seq, Sequence pat_seq
  -> associate_all matches tm_seq pat_seq
  | _, Operator _ -> None
  | _, Var "_" -> Some ([], empty_map)
  | tm, Var v -> Some ([], fromArray [| v, tm |])
  | _, _ -> None

and matches_scope
  : DeBruijn.scope
  -> BindingAwarePattern.scope
  -> (assoc list * DeBruijn.term Belt.Map.String.t) option
  = fun (Scope (binders, tm)) (Scope (patBinders, pat)) ->
  if length patBinders == length binders
  then
    Belt.Option.map (matches tm pat) (fun (assocs, tmMatches) ->
      ( zipBy patBinders binders
        (fun pattern_name term_pattern -> {pattern_name; term_pattern})
        @ assocs
      , tmMatches ))
  else None
;;

let find_match
 : denotation_chart
 -> DeBruijn.term
 -> (assoc list * DeBruijn.term Belt.Map.String.t * core) option
  = fun (DenotationChart denotations) term ->
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

let associate_pattern
  : assoc list -> string -> Pattern.t
  = fun assocs pat_name ->
  match Util.find (fun { pattern_name } -> pattern_name = pat_name) assocs with
  | None -> raise (TranslationError ("Pattern name " ^ pat_name ^ " not found", None))
  | Some { term_pattern } -> term_pattern
;;

(** Fill in a core representation of a value with metavars (the raw
    right-hand-side of a denotation chart) with the core terms that go there.
*)
let rec fill_in_core
  : fill_in_args -> core -> core
  = fun ({ dynamics; vars; assignments } as args) core -> match core with
  | Metavar name ->
    (match Belt.Map.String.get assignments name with
     | Some tm -> debruijn_to_core [] tm
     | None -> raise (TranslationError ("Metavariable " ^ name ^ " not found", None)))
  | Meaning name ->
    (match Belt.Map.String.get assignments name with
     | Some tm ->
       (match term_denotation dynamics vars tm with
        | Belt.Result.Ok tm' -> tm'
        | Error err -> raise (TranslationError err))
     | None -> raise (TranslationError ("Metavariable " ^ name ^ " not found", None)))
  | Var _ as v -> v
  | Operator (tag, vals) -> Operator (tag, vals |. map (fill_in_core_scope args))
  | Primitive _ as v -> v
  | Sequence tms -> Sequence (tms |. map (fill_in_core args))
  | Lambda (tys, body) -> Lambda (tys, fill_in_core_scope args body)
  | CoreApp (f, app_args) ->
    CoreApp (fill_in_core args f, app_args |. map (fill_in_core args))
  | Case (scrutinee, branches) ->
    Case
      ( fill_in_core args scrutinee
      , failwith "TODO"
        (*
           , branches |. map (fun (pat, scope) ->
           (pat, fill_in_core_scope args scope))
        *)
      )

and fill_in_core_scope
  : fill_in_args -> core_scope -> core_scope
  = fun ({ assocs; vars } as args) (Scope (patterns, body)) ->
  let patterns' : Pattern.t list =
    failwith "TODO"
    (* map patterns (associate_pattern assocs) *)
  in
  let vars' : string list = failwith "TODO" patterns' in
  Scope (patterns', fill_in_core { args with vars = vars' @ vars } body)

(** Translate a term directly to core, with no interpretation. In other words,
    this term is supposed to directly represent a term in the codomain. *)
and debruijn_to_core
  : int list -> DeBruijn.term -> core
  = fun env tm ->
  match tm with
  | Operator (tag, subtms) -> Operator (tag, subtms |. map (scope_to_core env))
  | Var (i, j) -> failwith "TODO"
  (*match Belt.List.get env ix with
    | None -> raise (TranslationError ("failed to look up variable", Some tm))
    | Some name -> Var name
  *)
  | Sequence tms -> Sequence (tms |. map (debruijn_to_core env))
  | Primitive prim -> Primitive prim

(* XXX change names (assocs)? *)
and scope_to_core env (Scope (names, body)) =
  Scope (names, debruijn_to_core env body)

(** Match a term in the denotation chart, and return its core term. *)
and term_denotation
   : denotation_chart
  -> string list
  -> DeBruijn.term
  -> core translation_result
  = fun dynamics vars tm -> match tm with
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
    | CoreApp (Lambda (_tys, Scope (argNames, body)), args) ->
      if length argNames != length args
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
                |> fromArray
                ) in
             *)
             go (map_union ctx new_args) body)
    | Case (tm, branches) ->
      Belt.Result.flatMap (go ctx tm) (fun v ->
        match find_core_match v branches with
        | None -> Error "no match found in case"
        | Some (branch, bindings) -> go (map_union ctx bindings) branch)
    | Metavar _v | Meaning _v -> Error "Found a metavar!"
    (* TODO: or should this be an app? *)
    | Operator ("#add", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Ok (Primitive (PrimInteger a')), Ok (Primitive (PrimInteger b')) ->
         Ok (Primitive (PrimInteger (Bigint.add a' b')))
       | Error err, _ | _, Error err -> Error err)
    | Operator ("#sub", [ Scope ([], a); Scope ([], b) ]) ->
      (match go ctx a, go ctx b with
       | Ok (Primitive (PrimInteger a')), Ok (Primitive (PrimInteger b')) ->
         Ok (Primitive (PrimInteger (Bigint.sub a' b')))
       | Error err, _ | _, Error err -> Error err)
    | Operator _ | Sequence _ | Primitive _ -> Ok tm
    (* TODO: include the term in error *)
    | _ -> Error "Found a term we can't evaluate"
  in
  go empty_map core
;;

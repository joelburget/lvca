open Base
open AbstractSyntax
open Binding
module Format = Caml.Format

type is_rec = Rec | NoRec

type term =
  | Term of Nominal.term
  (* plus, core-specific ctors *)
  | CoreApp of term * term
  | Case of term * core_case_scope list
  | Lambda of sort * core_scope
  | Let of is_rec * term * core_scope
  (** Lets bind only a single variable *)

and core_scope = Scope of string * term

and core_case_scope = CaseScope of Pattern.t * term

module PP = struct
  let list, any, pf, sp = Fmt.(list, any, pf, sp)

  (* TODO: add parse <-> pretty tests *)

  (** @raise InvariantViolation *)
  let rec pp_core
    = fun ppf -> function
      | Term tm -> Nominal.pp_term ppf tm
      | Lambda (sort, Scope (name, body)) ->
        pf ppf "\\(%s : %a) ->@ %a"
        name
        pp_sort sort
        pp_core body
      (* TODO: parens if necessary *)
      | (CoreApp _ as app) -> pp_app ppf app
      | Case (arg, case_scopes)
      -> pf ppf
        "@[<hv>match %a with {%t%a@ }@]"
        pp_core arg
        (* Before `|`, emit a single space if on the same line, or two when broken *)
        (Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:("", 2, "| "))
        (list ~sep:(any "@;<1 2>| ") pp_core_case_scope) case_scopes
      | Let (is_rec, tm, Scope (name, body))
      -> pf ppf "@[let %s%s =@ %a in@ @[%a@]@]"
         (match is_rec with Rec -> "rec " | NoRec -> "")
         name pp_core tm pp_core body

  and pp_core_case_scope : Format.formatter -> core_case_scope -> unit
    = fun ppf (CaseScope (pat, body))
    -> pf ppf "@[%a@ -> %a@]" Pattern.pp pat pp_core body

  (* Flatten all arguments into one box *)
  and pp_app = fun ppf app ->
    let rec go = function
      | CoreApp (f_args, final_arg) -> go f_args @ [final_arg]
      | tm -> [tm]
    in
    match go app with
      | [] -> Util.invariant_violation "pp_app: must be at least one argument"
      | f :: args ->
        pf ppf "@[<h>%a@ @[<hov>%a@]@]"
        pp_core f
        (list ~sep:sp pp_core) args
end

let pp_core : Format.formatter -> term -> unit
  = PP.pp_core

let pp_core_str : term -> string
  = Format.asprintf "%a" pp_core

type core_defn =
  { name : string
  ; ty : sort
  ; defn : term
  }

type import = AbstractSyntax.import

type core_module = CoreModule of import list * core_defn list

let pp_module : Format.formatter -> core_module -> unit
  = fun ppf (CoreModule (imports, definitions)) ->
    List.iter imports ~f:(fun import ->
      AbstractSyntax.pp_import ppf import;
      Format.pp_force_newline ppf ()
    );
    List.iter definitions
    ~f:(fun { name; ty; defn } -> Fmt.pf ppf "@[<hv>%s@ : %a =@ %a@]"
      name
      pp_sort ty
      pp_core defn)

let pp_module_str : core_module -> string
  = Format.asprintf "%a" pp_module

let rec match_pattern
  : Nominal.term -> Pattern.t -> Nominal.term Util.String.Map.t option
  = fun v pat -> match v, pat with
  | Operator (tag1, vals), Operator (tag2, pats) ->
    if String.(tag1 = tag2) && List.(Int.(length vals = length pats))
    then (
      let sub_results =
        List.map2_exn vals pats ~f:(fun core_scope (pat : Pattern.t) ->
            match core_scope, pat with
            | Scope ([], body), pat' -> match_pattern body pat'
            | _ -> None)
      in
      if List.for_all sub_results ~f:Option.is_some
      then
        Some
          (sub_results
          |> List.map ~f:(Util.Option.get_invariant
            (fun () -> "we just checked all is_some"))
          |> Util.String.Map.strict_unions
          |> function
            | `Duplicate_key k -> Util.invariant_violation (Printf.sprintf
                "multiple variables with the same name (%s) in one pattern"
                k
            )
            | `Ok m -> m
          )
      else None)
    else None
  | Primitive l1, Primitive l2
  -> if Primitive.(l1 = l2) then Some Util.String.Map.empty else None
  | _, Var "_" -> Some Util.String.Map.empty
  | tm, Var v -> Some (Util.String.Map.of_alist_exn [ v, tm ])
  | _ -> None
;;

let find_core_match
  : Nominal.term -> core_case_scope list -> (term * Nominal.term Util.String.Map.t) option
  = fun v branches ->
  branches
  |> List.find_map ~f:(function
         | CaseScope (pat, rhs) ->
           (match match_pattern v pat with
           | None -> None
           | Some bindings -> Some (rhs, bindings)))
;;

type eval_error = string * term

exception EvalExn of string * term

let eval_exn : term -> Nominal.term
  = fun core ->
    let rec go : Nominal.term Util.String.Map.t -> term -> Nominal.term =
     fun ctx tm -> match tm with
      | Term (Var v) -> (match Map.find ctx v with
        | Some result -> result
        | None -> raise @@ EvalExn ("Unbound variable " ^ v, tm))
      | CoreApp (Lambda (_ty, Scope (name, body)), arg) ->
        let arg_val = go ctx arg in
        go (Map.set ctx ~key:name ~data:arg_val) body
      | Case (tm, branches) ->
        (match find_core_match (go ctx tm) branches with
        | None -> raise @@ EvalExn ("no match found in case", tm)
        | Some (branch, bindings) -> go (Util.Map.union_right_biased ctx bindings) branch)

      (* primitives *)
      (* TODO: or should this be an app? *)
      | Term (Operator ("#add", [ Scope ([], a); Scope ([], b) ])) ->
        (match go' ctx a, go' ctx b with
        | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
          Primitive (PrimInteger Bigint.(a' + b'))
        | _ -> raise @@ EvalExn ("Invalid arguments to #add", tm))
      | Term (Operator ("#sub", [ Scope ([], a); Scope ([], b) ])) ->
        (match go' ctx a, go' ctx b with
        | Primitive (PrimInteger a'), Primitive (PrimInteger b') ->
          Primitive (PrimInteger Bigint.(a' - b'))
        | _ -> raise @@ EvalExn ("Invalid arguments to #sub", tm))

      | Term tm -> tm
      | _ -> raise @@ EvalExn ("Found a term we can't evaluate", tm)
    and go' : Nominal.term Util.String.Map.t -> Nominal.term -> Nominal.term
      = fun ctx tm -> match tm with
        | Var v -> (match Map.find ctx v with
          | Some result -> result
          | None -> raise @@ EvalExn ("Unbound variable " ^ v, Term tm))
        | _ -> tm

    in go Util.String.Map.empty core

let eval : term -> (Nominal.term, eval_error) Result.t =
  fun core -> try Ok (eval_exn core) with EvalExn (msg, tm) -> Error (msg, tm)
;;

(* module_to_term *)

(*
let rec term_of_core : term -> Nominal.term
  = function
  | Term tm -> Operator ("term", [Scope ([], tm)])
  | Lambda (sort, scope) -> Operator ("lambda",
    [ Scope ([], sort
        |> term_of_sort
        |> NonBinding.to_nominal)
    ; scope_of_core_scope scope
    ])
  | CoreApp (f, arg) -> Operator ("core_app",
    [ Scope ([], term_of_core f)
    ; Scope ([], term_of_core arg)
    ])
  | Case (tm, _branches) -> Operator ("case",
    [ Scope ([], term_of_core tm)
    (* TODO *)
    (* ; Scope ([], Sequence (List.map branches ~f:scope_of_core_case_scope)) *)
    ])
  | Let (is_rec, tm, body) ->
     let rec_tm = match is_rec with Rec -> "rec" | NoRec -> "norec" in
     Operator ("let",
    [ Scope ([], Operator (rec_tm, []))
    ; Scope ([], term_of_core tm)
    ; scope_of_core_scope body
    ])

and scope_of_core_scope : core_scope -> Nominal.scope
  = fun (Scope (name, body)) -> Scope ([Pattern.Var name], term_of_core body)

  (*
and scope_of_core_case_scope : core_case_scope -> Nominal.scope
  = fun (CaseScope (baw_pat, body)) -> failwith "TODO"
  *)

let module_to_term : core_module -> Nominal.term
  = fun (CoreModule (imports, defns)) -> Operator ("core_module",
    [ Scope ([], Sequence (imports
        |> List.map ~f:AbstractSyntax.term_of_import
        |> List.map ~f:NonBinding.to_nominal
      ))
    ; Scope ([], Sequence (List.map defns
      ~f:(fun { name; ty; defn } -> Nominal.Operator ("core_defn",
        [ Scope ([], Primitive (PrimString name))
        ; Scope ([], ty |> term_of_sort |> NonBinding.to_nominal)
        ; Scope ([], term_of_core defn)
        ]))))
    ])
    *)

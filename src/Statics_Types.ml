module List = Base.List
module String = Util.String

type scope = Scope of Pattern.t list * term

and term =
  | Operator of string * scope list
  | Bound of int * int
  (** Bound vars come via conversion of de Bruijn terms. *)
  | Free of string
  (** Free vars are used during typechecking. *)
  | Primitive of Primitive.t

let rec string_of_term = function
  | Operator (name, scopes) ->
    Printf.sprintf "%s(%s)" name
    (scopes |> List.map ~f:string_of_scope |> String.concat ~sep:"; ")
  | Bound (i, j) -> Printf.sprintf "%d, %d" i j
  | Free str -> str
  | Primitive prim -> Primitive.to_string prim

and string_of_scope (Scope (pats, tm)) =
  match pats with
  | [] -> string_of_term tm
  | _ ->
    let pats' = pats |> List.map ~f:Pattern.to_string |> String.concat ~sep:". " in
    Printf.sprintf "%s. %s" pats' (string_of_term tm)
;;

type typing_rule =
  { tm : term
  ; ty : term
  }

type inference_rule = typing_rule
type checking_rule = typing_rule

type typing_clause =
  | InferenceRule of inference_rule
  | CheckingRule of checking_rule

type hypothesis = term String.Map.t * typing_clause

type rule =
  { hypotheses : hypothesis list
  ; name : string option
  ; conclusion : hypothesis
  }

type typing = Typing of term * term

(** Convert a de Bruijn term to a [term]. See also [to_de_bruijn_exn]. *)
let rec of_de_bruijn : Binding.DeBruijn.term -> term = function
  | Operator (tag, scopes) -> Operator (tag, List.map scopes ~f:scope_of_de_bruijn)
  | Var (i, j) -> Bound (i, j)
  | Primitive p -> Primitive p

and scope_of_de_bruijn : Binding.DeBruijn.scope -> scope =
  fun (Scope (pats, body)) -> Scope (pats, of_de_bruijn body)
;;

exception FreeVar of string

(** Convert a [term] to a de Bruijn representation. See also [of_de_bruijn].
 @raise [FreeVar] *)
let rec to_de_bruijn_exn : term -> Binding.DeBruijn.term
  = function
    | Operator (name, scopes) -> Operator (name, List.map scopes ~f:to_scope)
    | Bound (i, j) -> Var (i, j)
    | Free name -> raise (FreeVar name)
    | Primitive prim -> Primitive prim

and to_scope : scope -> Binding.DeBruijn.scope
  = fun (Scope (pats, tm)) -> Scope (pats, to_de_bruijn_exn tm)

(* to_term: *)

open Binding

let rec term_to_term : term -> Nominal.term
  = fun tm -> match tm with
    | Operator (name, subtms)
    (* We can't translate directly to [Operator (name, ...)] because we need to
       use [Operator] for free and bound vars. *)
    -> Operator (name, List.map subtms ~f:scope_to_term)
    | Bound _ -> Util.invariant_violation (Printf.sprintf
      "term_to_term: Bound variables are not allowed in typing rules: %s"
      (string_of_term tm)
      )
    | Free name -> Var name
    | Primitive p -> Primitive p

and scope_to_term : scope -> Nominal.scope
  = fun (Scope (pats, tm)) -> Scope (pats, term_to_term tm)

let typing_rule_to_term : typing_rule -> Nominal.term
  = fun { tm; ty } -> Operator ("typing_rule",
    [ Scope ([], term_to_term tm)
    ; Scope ([], term_to_term ty)
    ])

let typing_clause_to_term : typing_clause -> Nominal.term
  = function
    | InferenceRule rule
    -> Operator ("inference_rule", [Scope ([], typing_rule_to_term rule)])
    | CheckingRule rule
    -> Operator ("checking_rule", [Scope ([], typing_rule_to_term rule)])

    (*
let hypothesis_to_term : hypothesis -> Nominal.term
  = fun (ctx, typing_clause) -> Operator ("hypothesis",
    [ Scope ([], Sequence (ctx
        |> Map.to_alist
        |> List.map ~f:(fun (name, tm) -> Nominal.Operator ("pair",
          [ Scope ([], Primitive (PrimString name))
          ; Scope ([], term_to_term tm)
          ]))
      ))
    ; Scope ([], typing_clause_to_term typing_clause)
    ])

let term_of_option : ('a -> Nominal.term) -> 'a option -> Nominal.term
  = fun f -> function
    | None -> Operator ("none", [])
    | Some a -> Operator ("some", [Scope ([], f a)])

let rule_to_term : rule -> Nominal.term
  = fun { hypotheses; name; conclusion } -> Operator ("rule",
    [ Scope ([], Sequence (List.map hypotheses ~f:hypothesis_to_term))
    ; Scope ([],
        term_of_option (fun name' -> Primitive (PrimString name')) name)
    ; Scope ([], hypothesis_to_term conclusion)
    ])

let to_term : rule list -> Nominal.term
  = fun rules -> Sequence (List.map rules ~f:rule_to_term)
  *)

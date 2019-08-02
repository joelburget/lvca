open Types

module M = Belt.Map.String
module BL = Belt.List

type scope = Scope of string list * term

and term =
  | Operator  of string * scope list
  | Bound     of int
  | Free      of string
  | Sequence  of term list
  | Primitive of primitive

type inference_rule = {
  tm : term;
  ty : term;
}

type checking_rule = inference_rule

type typing_clause =
  | InferenceRule of inference_rule
  | CheckingRule  of checking_rule

type hypothesis = term M.t * typing_clause

(* TODO: the conclusion type differs from LVCA *)
(* type rule = Rule of hypothesis list * string option * hypothesis *)

type rule = {
    hypotheses : hypothesis list;
    name       : string option;
    conclusion : hypothesis;
  }

type typing = Typing of term * term

let rec of_de_bruijn : Binding.DeBruijn.term -> term
  = function
    | Operator (tag, scopes)
    -> Operator (tag, scopes |. BL.map scope_of_de_bruijn)
    | Var i
    -> Bound i
    | Sequence tms
    -> Sequence (tms |. BL.map of_de_bruijn)
    | Primitive p
    -> Primitive p

and scope_of_de_bruijn : Binding.DeBruijn.scope -> scope
  = fun (Scope (binders, body)) -> Scope (binders, of_de_bruijn body)

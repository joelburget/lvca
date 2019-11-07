open Types

module M = Belt.Map.String
module BL = Belt.List

(* TODO: untangle Statics_Lexer -> Statics_Parser -> Statics *)
(* module Lexer = Statics_Lexer *)
(* module Parser = Statics_Parser *)
module ParseErrors = Statics_ParseErrors

type scope = Scope of string list * term

and term =
  | Operator  of string * scope list
  | Bound     of int
  | Free      of string
  | Sequence  of term list
  | Primitive of primitive

let rec string_of_term = function
  | Operator (name, scopes) ->
    let scopes' = scopes
      |. Belt.List.toArray
      |. Belt.Array.map string_of_scope
      |. Js.Array2.joinWith "; "
    in Printf.sprintf "%s(%s)" name scopes'
  | Bound i -> string_of_int i
  | Free str -> str
  | Sequence tms ->
    let tms' = tms
      |. Belt.List.toArray
      |. Belt.Array.map string_of_term
      |. Js.Array2.joinWith ", "
    in
    "[" ^ tms' ^ "]"
  | Primitive prim -> string_of_primitive prim

and string_of_scope = fun (Scope (names, tm)) -> match names with
  | [] -> string_of_term tm
  | _ ->
    let names' = names
      |. Belt.List.toArray
      |. Js.Array2.joinWith ". "
    in
    Printf.sprintf "%s. %s" names' (string_of_term tm)

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

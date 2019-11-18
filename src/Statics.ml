open Types

module M = Belt.Map.String
module BL = Belt.List

(* TODO: untangle Statics_Lexer -> Statics_Parser -> Statics *)
(* module Lexer = Statics_Lexer *)
(* module Parser = Statics_Parser *)
module ParseErrors = Statics_ParseErrors

type scope = Scope of Pattern.t list * term

and term =
  | Operator  of string * scope list
  | Bound     of int * int
  | Free      of string
  | Sequence  of term list
  | Primitive of primitive

let rec string_of_term = function
  | Operator (name, scopes) -> Printf.sprintf "%s(%s)" name
    (Util.stringify_list string_of_scope "; " scopes)
  | Bound (i, j) -> Printf.sprintf "%d, %d" i j
  | Free str -> str
  | Sequence tms -> "[" ^ Util.stringify_list string_of_term ", "tms ^ "]"
  | Primitive prim -> string_of_primitive prim

and string_of_scope = fun (Scope (pats, tm)) -> match pats with
  | [] -> string_of_term tm
  | _ ->
    let pats' = Util.stringify_list Pattern.string_of_pattern ". " pats in
    Printf.sprintf "%s. %s" pats' (string_of_term tm)

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
    | Var (i, j)
    -> Bound (i, j)
    | Sequence tms
    -> Sequence (tms |. BL.map of_de_bruijn)
    | Primitive p
    -> Primitive p

and scope_of_de_bruijn : Binding.DeBruijn.scope -> scope
  = fun (Scope (pats, body)) -> Scope (pats, of_de_bruijn body)

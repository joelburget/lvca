open Core_kernel
open Lvca

type parseable' =
  { abstract_syntax : AbstractSyntax.abstract_syntax
  ; concrete_syntax : ConcreteSyntaxDescription.t option
  }

type parseable =
  | ParseableTerm
  | ParseableAbstractSyntax
  | ParseableConcreteSyntax
  | ParseableDynamics
  | ParseableStatics
  | Parseable of parseable'

type term_store = Binding.Nominal.term String.Table.t
type name_store = string String.Table.t

type store =
  { term_store : term_store
  ; name_store : name_store
  }

let make_lang : Binding.Nominal.term -> parseable
  = fun tm -> failwith (Printf.sprintf "TODO: make_lang %s" (Binding.Nominal.pp_term' tm))

let lookup_sha : term_store -> string -> parseable
  = fun term_store -> function
  | "0000000000000000000000000000000000000000000000000000000000000001"
  -> ParseableTerm
  | "0000000000000000000000000000000000000000000000000000000000000002"
  -> ParseableAbstractSyntax
  | "0000000000000000000000000000000000000000000000000000000000000003"
  -> ParseableConcreteSyntax
  | "0000000000000000000000000000000000000000000000000000000000000004"
  -> ParseableDynamics
  | "0000000000000000000000000000000000000000000000000000000000000005"
  -> ParseableStatics
  | sha_str -> make_lang @@ Hashtbl.find_exn term_store sha_str

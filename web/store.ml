open Core_kernel
open Lvca

type store_value =
  (* abstract syntaxes *)
  | GenesisTermLanguage
  | GenesisAbstractSyntaxLanguage
  | GenesisConcreteSyntaxLanguage
  | GenesisDynamicsLanguage
  | GenesisStaticsLanguage
  (* concrete syntaxes *)
  | GenesisTermConcrete
  | GenesisAbstractSyntaxConcrete
  | GenesisConcreteSyntaxConcrete
  | GenesisDynamicsConcrete
  | GenesisStaticsConcrete
  | Term of Binding.Nominal.term

type term_store = Binding.Nominal.term String.Table.t
type name_store = string String.Table.t

type store =
  { term_store : term_store
  ; name_store : name_store
  }

let lookup_sha : term_store -> string -> store_value
  = fun term_store -> function
  (* abstract syntaxes *)
  | "0000000000000000000000000000000000000000000000000000000000000001"
  -> GenesisTermLanguage
  | "0000000000000000000000000000000000000000000000000000000000000002"
  -> GenesisAbstractSyntaxLanguage
  | "0000000000000000000000000000000000000000000000000000000000000003"
  -> GenesisConcreteSyntaxLanguage
  | "0000000000000000000000000000000000000000000000000000000000000004"
  -> GenesisDynamicsLanguage
  | "0000000000000000000000000000000000000000000000000000000000000005"
  -> GenesisStaticsLanguage

  (* concrete syntaxes *)
  | "0000000000000000000000000000000000000000000000000000000000000006"
  -> GenesisTermConcrete
  | "0000000000000000000000000000000000000000000000000000000000000007"
  -> GenesisAbstractSyntaxConcrete
  | "0000000000000000000000000000000000000000000000000000000000000008"
  -> GenesisConcreteSyntaxConcrete
  | "0000000000000000000000000000000000000000000000000000000000000009"
  -> GenesisDynamicsConcrete
  | "000000000000000000000000000000000000000000000000000000000000000a"
  -> GenesisStaticsConcrete

  | sha_str
  -> Term (Hashtbl.find_exn term_store sha_str)

let initial_name_store : name_store
  = String.Table.of_alist_exn
    (* abstract syntaxes *)
    [ "term",
      "0000000000000000000000000000000000000000000000000000000000000001"
    ; "abstract_syntax",
      "0000000000000000000000000000000000000000000000000000000000000002"
    ; "concrete_syntax",
      "0000000000000000000000000000000000000000000000000000000000000003"
    ; "dynamics",
      "0000000000000000000000000000000000000000000000000000000000000004"
    ; "statics",
      "0000000000000000000000000000000000000000000000000000000000000005"

    (* concrete syntaxes *)
    ; "term_concrete",
      "0000000000000000000000000000000000000000000000000000000000000006"
    ; "abstract_syntax_concrete",
      "0000000000000000000000000000000000000000000000000000000000000007"
    ; "concrete_syntax_concrete",
      "0000000000000000000000000000000000000000000000000000000000000008"
    ; "dynamics_concrete",
      "0000000000000000000000000000000000000000000000000000000000000009"
    ; "statics_concrete",
      "000000000000000000000000000000000000000000000000000000000000000a"
    ]

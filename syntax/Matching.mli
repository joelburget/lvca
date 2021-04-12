(** Pattern matching. See Maranget's "Compiling pattern matching to good decision trees". *)

module Make (Prim : LanguageObject_intf.S) : Matching_intf.S with module Prim = Prim

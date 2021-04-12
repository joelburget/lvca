(** Representation of terms that simply uses variable names to represent scope. *)

module Make (Prim : LanguageObject_intf.S) : Nominal_intf.S with module Prim = Prim
(* and module Pattern = Pattern.Make(Prim) *)

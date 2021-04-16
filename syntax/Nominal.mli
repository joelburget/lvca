(** Representation of terms that simply uses variable names to represent scope. *)

module Make2 (Prim : LanguageObject_intf.S) (Pat : Pattern_intf.S with module Prim = Prim) :
  Nominal_intf.S with module Prim = Prim and module Pat = Pat

module Make (Prim : LanguageObject_intf.S) :
  Nominal_intf.S with module Prim = Prim and module Pat = Pattern.Make(Prim)

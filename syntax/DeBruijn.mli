(** Representation of terms that uses de Bruijn indices to represent scope. *)

module Make (Prim : LanguageObject_intf.S) : DeBruijn_intf.S with module Prim = Prim

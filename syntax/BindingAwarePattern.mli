(** Patterns for matching binding terms. *)

(** {1 Types} *)

module Make (Prim : LanguageObject_intf.S) :
  BindingAwarePattern_intf.S with module Prim = Prim

(** Lots of interesting domains have no binding. At that point they're not really
    languages, just data types. This module gives a tighter representation for such types
    and allows conversion to / from binding types. *)

module Make (Prim : LanguageObject_intf.S) : NonBinding_intf.S with module Prim = Prim

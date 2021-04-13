module Make (Prim : LanguageObject_intf.S) : Nominal_intf.S with module OuterPrim = Prim =
struct
  module OuterPrim = Prim
  module OuterPat : Pattern_intf.S with module Prim = Prim = Pattern.Make (Prim)

  type 'info term =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info OuterPrim.t

  and 'info scope = Scope of 'info OuterPat.t list * 'info term

  module rec Term :
    (Nominal_intf.TermS
      with module Prim = OuterPrim
       and module Pat = OuterPat
       and type 'info scope = 'info scope
       and type 'info t = 'info term) = struct
    module Prim = OuterPrim
    module Pat = OuterPat

    type 'info scope_outer = 'info scope
    type 'info scope = 'info scope_outer

    type 'info t = 'info term =
      | Operator of 'info * string * 'info scope list
      | Var of 'info * string
      | Primitive of 'info Prim.t
  end

  and Scope :
    (Nominal_intf.ScopeS
      with module Prim = OuterPrim
       and module Pat = OuterPat
       and type 'info term = 'info term
       and type 'info t = 'info scope) = struct
    module Prim = OuterPrim
    module Pat = OuterPat

    type 'info term_outer = 'info term
    type 'info term = 'info term_outer
    type 'info t = 'info scope = Scope of 'info Pat.t list * 'info term
  end
end

module Make (Prim : LanguageObject_intf.S) : Nominal_intf.S with module Prim = Prim =
struct
  module Prim = Prim
  module Pat : Pattern_intf.S with module Prim = Prim = Pattern.Make (Prim)

  type 'info term =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Prim.t

  and 'info scope = Scope of 'info Pat.t list * 'info term

  module rec Term :
    (Nominal_intf.TermS
      with module Prim = Prim
       and module Pat = Pat
       and type 'info scope = 'info scope
       and type 'info t = 'info term) = struct
    module Prim = Prim
    module Pat = Pat

    type 'info scope_outer = 'info scope
    type 'info scope = 'info scope_outer

    type 'info t = 'info term =
      | Operator of 'info * string * 'info scope list
      | Var of 'info * string
      | Primitive of 'info Prim.t
  end

  and Scope :
    (Nominal_intf.ScopeS
      with module Prim = Prim
       and module Pat = Pat
       and type 'info term = 'info term
       and type 'info t = 'info scope) = struct
    module Prim = Prim
    module Pat = Pat

    type 'info term_outer = 'info term
    type 'info term = 'info term_outer
    type 'info t = 'info scope = Scope of 'info Pat.t list * 'info term
  end
end

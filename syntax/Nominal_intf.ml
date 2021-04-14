module type TermS = sig
  module Prim : LanguageObject_intf.S
  module Pat : Pattern_intf.S

  type 'info scope

  type 'info t =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Prim.t
end

module type ScopeS = sig
  module Prim : LanguageObject_intf.S
  module Pat : Pattern_intf.S

  type 'info term
  type 'info t = Scope of 'info Pat.t list * 'info term
end

module type S = sig
  module Prim : LanguageObject_intf.S
  module Pat : Pattern_intf.S (* with module Prim = Prim *)

  type 'info term =
    | Operator of 'info * string * 'info scope list
    | Var of 'info * string
    | Primitive of 'info Prim.t

  and 'info scope = Scope of 'info Pat.t list * 'info term

  module Term :
    TermS
      with module Prim = Prim
       and module Pat = Pat
       and type 'info scope = 'info scope
       and type 'info t = 'info term

  module Scope :
    ScopeS
      with module Prim = Prim
       and module Pat = Pat
       and type 'info term = 'info term
       and type 'info t = 'info scope
end

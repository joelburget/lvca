open Brr
open Note

module Model = struct
  type t = unit

  let initial_model = ()
  let ( = ) _ _ = true
end

module Action = struct
  type t = Nop
end

module Controller = struct
  let update (action : Action.t) model = match action with Nop -> model
end

module View = struct
  open El

  let view _model_s = E.select [], div [ txt' "TODO: IDE" ]
end

module Stateless_view = Stateless_view.Mk (Action) (Model) (View) (Controller)

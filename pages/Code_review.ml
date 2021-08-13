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

  let view _model_s = E.select [], div [ txt' "TODO: Code review" ]
end

let stateless_view () =
  let wrapper model_s =
    let evts, elem = View.view model_s in
    let do_action = E.map Controller.update evts in
    let model_s' = S.accum ~eq:Model.( = ) (S.value model_s) do_action in
    model_s', (model_s', elem)
  in
  let model_s, elem = S.fix ~eq:Model.( = ) Model.initial_model wrapper in
  Logr.hold (S.log model_s (fun _ -> ()));
  elem
;;

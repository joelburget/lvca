open Brr
open Note

module type Action_sig = sig
  type t
end

module type Model_sig = sig
  type t

  val ( = ) : t -> t -> bool
  val initial_model : t
end

module type View_sig = sig
  type model_t
  type action_t

  val view : model_t signal -> action_t event * El.t
end

module type Controller_sig = sig
  type model_t
  type action_t

  val update : action_t -> model_t -> model_t
end

module Mk
    (Action : Action_sig)
    (Model : Model_sig)
    (View : View_sig with type model_t := Model.t and type action_t := Action.t)
    (Controller : Controller_sig
                    with type model_t := Model.t
                     and type action_t := Action.t) =
struct
  let view () =
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
end

open Brr
open Prelude

module Model = struct
  let initial_model = ()
end

module View = struct
  let view _model = El.div [ txt "TODO" ]
end

let stateless_view () = View.view Model.initial_model

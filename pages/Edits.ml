module Model = struct
  let initial_model = ()
end

module View = struct
  open Brr.El

  let view _model = div [ txt' "TODO" ]
end

let stateless_view () = View.view Model.initial_model

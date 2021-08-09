module Model = struct
  let initial_model = ()
end

module View = struct
  open Brr.El

  let view _model =
    div
      [ txt' "substitution"
      ; txt' "opening"
      ; txt' "closing"
      ; txt' "structural induction"
      ; txt' "folding"
      ; txt' "is open? (free vars)"
      ; txt' "renaming"
      ; txt' "(alpha) equivalence checking"
      ]
  ;;
end

let stateless_view () = View.view Model.initial_model

open Bonsai_web
open Core_kernel
open Lvca_web2

let test_md = [%blob "test.md"]

let (_ : _ Start.Handle.t) =

  (* Points from sha256 to term *)
  let term_store = String.Table.create () in

  let component = test_md
    |> LanguageDocument.evaluate_and_produce_vdom { term_store; name_store =
      Store.initial_name_store }
    |> Bonsai.const
  in

  Start.start_standalone
    ~initial_input:()
    ~initial_model:()
    ~bind_to_element_with_id:"app"
    component
;;

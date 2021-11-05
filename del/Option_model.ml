open Lvca_syntax
include [%lvca.abstract_syntax_module "option a := None() | Some(a)"]

let of_option x =
  let info = Provenance.of_here [%here] in
  match x with None -> Option.None info | Some a -> Some (info, a)
;;

let to_option = function Option.None _ -> None | Some (_, a) -> Some a
let map ~f = function Option.None i -> Option.None i | Some (i, a) -> Some (i, f a)

open Lvca_syntax

module Kernel =
[%lvca.abstract_syntax_module
{|
string : *

sort :=
  | Ap(string; ap_list)
  | Name(string)

ap_list :=
  | Nil()
  | Cons(sort; ap_list)
|}
, { string = "Primitive.String" }]

module Into = struct
  let rec sort tm =
    let info = Provenance.calculated_here [%here] [ Lvca_syntax.Sort.info tm ] in
    match tm with
    | Lvca_syntax.Sort.Ap (_, name, lst) ->
      Kernel.Sort.Ap (info, (info, name), ap_list lst)
    | Name (_, name) -> Name (info, (info, name))

  and ap_list tm =
    let info = Provenance.calculated_here [%here] [ Lvca_syntax.Sort.Ap_list.info tm ] in
    match tm with
    | Lvca_syntax.Sort.Nil _ -> Kernel.Ap_list.Nil info
    | Cons (_, s, lst) -> Cons (info, sort s, ap_list lst)
  ;;
end

module Out = struct
  let rec sort tm =
    let info = Provenance.calculated_here [%here] [ Kernel.Sort.info tm ] in
    match tm with
    | Kernel.Sort.Ap (_, (_, name), lst) -> Lvca_syntax.Sort.Ap (info, name, ap_list lst)
    | Name (_, (_, name)) -> Name (info, name)

  and ap_list tm =
    let info = Provenance.calculated_here [%here] [ Kernel.Ap_list.info tm ] in
    match tm with
    | Kernel.Ap_list.Nil _ -> Lvca_syntax.Sort.Nil info
    | Cons (_, s, lst) -> Cons (info, sort s, ap_list lst)
  ;;
end

module Sort = struct
  (*
  type t = Kernel.Sort.t =
    | Ap of Provenance.t * Primitive.String.t * Kernel.Ap_list.t
    | Name of Provenance.t * Primitive.String.t
     *)
  include Kernel.Sort
  include Nominal.Convertible.Extend (Kernel.Sort)

  let into = Into.sort
  let out = Out.sort
end

module Ap_list = struct
  include Kernel.Ap_list
  include Nominal.Convertible.Extend (Kernel.Ap_list)

  let into = Into.ap_list
  let out = Out.ap_list
end

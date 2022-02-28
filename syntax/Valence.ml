open Lvca_util

type t = Valence of Sort_slot.t list * Sort.t

let equivalent
    ?(info_eq = fun _ _ -> true)
    (Valence (slots1, sort1))
    (Valence (slots2, sort2))
  =
  List.equal Sort_slot.(equivalent ~info_eq) slots1 slots2
  && Sort.(equivalent ~info_eq) sort1 sort2
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let pp ppf (Valence (binders, result)) =
  match binders with
  | [] -> Sort.pp ppf result
  | _ ->
    Fmt.pf ppf "%a. %a" Fmt.(list ~sep:(any ".@ ") Sort_slot.pp) binders Sort.pp result
;;

let instantiate env = function
  | Valence (binding_sort_slots, body_sort) ->
    Valence
      ( List.map binding_sort_slots ~f:(Sort_slot.instantiate env)
      , Sort.instantiate env body_sort )
;;

let kind_check env (Valence (binding_slots, value_sort)) =
  let env = binding_slots |> List.fold ~init:env ~f:Sort_slot.kind_check in
  Sort.kind_check env value_sort
;;

let parse =
  let open Lvca_parsing.Parser in
  let open Construction in
  let t =
    sep_by1 (symbol ".") Sort_slot.parse
    >>= fun slots ->
    let binders, body_slot = List.unsnoc slots in
    match body_slot with
    | Sort_slot.Sort_binding body_sort -> return (Valence (binders, body_sort))
    | _ ->
      fail
        (Fmt.str
           "Expected a simple sort, instead found a pattern sort (%a)"
           Sort_slot.pp
           body_slot)
  in
  t <?> "valence"
;;

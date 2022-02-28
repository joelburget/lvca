open Base

type t =
  | Sort_binding of Sort.t
  | Sort_pattern of Pattern_sort.t

let equivalent ?(info_eq = fun _ _ -> true) slot1 slot2 =
  match slot1, slot2 with
  | Sort_binding s1, Sort_binding s2 -> Sort.equivalent ~info_eq s1 s2
  | Sort_pattern ps1, Sort_pattern ps2 -> Pattern_sort.equivalent ~info_eq ps1 ps2
  | _, _ -> false
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let pp ppf = function
  | Sort_binding sort -> Sort.pp ppf sort
  | Sort_pattern ps -> Pattern_sort.pp ppf ps
;;

let instantiate env = function
  | Sort_binding s -> Sort_binding (Sort.instantiate env s)
  | Sort_pattern ps -> Sort_pattern (Pattern_sort.instantiate env ps)
;;

let kind_check env = function
  | Sort_binding sort -> Sort.kind_check env sort
  | Sort_pattern { pattern_sort; var_sort } ->
    [ pattern_sort; var_sort ] |> List.fold ~init:env ~f:Sort.kind_check
;;

let parse =
  let open Lvca_parsing.Parser in
  let open Construction in
  let p =
    let+ sort = Sort.parse
    and+ continuation =
      choice
        ~failure_msg:"sort slot"
        [ (brackets Sort.parse
          >>| fun var_sort sort -> Sort_pattern { pattern_sort = sort; var_sort })
        ; return (fun sort -> Sort_binding sort)
        ]
    in
    continuation sort
  in
  p <?> "sort slot"
;;

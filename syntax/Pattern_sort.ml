type t =
  { pattern_sort : Sort.t
  ; var_sort : Sort.t
  }

let equivalent ?(info_eq = fun _ _ -> true) ps1 ps2 =
  Sort.equivalent ~info_eq ps1.pattern_sort ps2.pattern_sort
  && Sort.equivalent ~info_eq ps1.var_sort ps2.var_sort
;;

let ( = ) = equivalent ~info_eq:Provenance.( = )

let pp ppf { pattern_sort; var_sort } =
  match pattern_sort with
  | Sort.Name _ -> Fmt.pf ppf "%a[%a]" Sort.pp pattern_sort Sort.pp var_sort
  | _ -> Fmt.pf ppf "(%a)[%a]" Sort.pp pattern_sort Sort.pp var_sort
;;

let instantiate env { pattern_sort; var_sort } =
  { pattern_sort = Sort.instantiate env pattern_sort
  ; var_sort = Sort.instantiate env var_sort
  }
;;

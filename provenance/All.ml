let stag_functions =
  Stdlib.Format.
    { mark_open_stag =
        (function
        | Range.Stag t -> Fmt.str "<%a>" Range.pp t
        | Ranges.Stag t -> Fmt.str "<%a>" Ranges.pp t
        (* | Opt_range.Stag t -> Fmt.str "<%a>" Opt_range.pp t *)
        | Source_range.Stag t -> Fmt.str "<%a>" Source_range.pp t
        | Source_ranges.Stag t -> Fmt.str "<%a>" Source_ranges.pp t
        | _ -> "")
    ; mark_close_stag =
        (function
        | Range.Stag t -> Fmt.str "</%a>" Range.pp t
        | Ranges.Stag t -> Fmt.str "</%a>" Ranges.pp t
        (* | Opt_range.Stag t -> Fmt.str "</%a>" Opt_range.pp t *)
        | Source_range.Stag t -> Fmt.str "</%a>" Source_range.pp t
        | Source_ranges.Stag t -> Fmt.str "</%a>" Source_ranges.pp t
        | _ -> "")
    ; print_open_stag = (fun _ -> ())
    ; print_close_stag = (fun _ -> ())
    }
;;
